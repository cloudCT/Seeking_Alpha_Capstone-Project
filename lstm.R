##################################
################## LSTM
###########################

# Installing Keras (Tensorflow backend)
library(keras)
install_keras()
#install_tensorflow()
library(tensorflow)


library(doParallel)
# detectCores() # - to find out number of cores available to you
registerDoParallel(cores = 8) # Device with 10 cores 
# (only incremental change the more cores are used)






##################
############ A First Model
# Next Month Predictions


## Scaling and Reshaping

# We scale the data and sequence it. LSTM requires sequences of input data on 
# on which the predictions are made, and sequences of output data that are being
# predicted. We end up with 3 sets; x_train, x_test and a third y_train with the
# input sequence for our predictions.
# We then reshape the training and test data to have a 3D tensor shape. 
# - > Found in wrangling section


## Hyperparameters:

# decide hyperparameters
features <- 1
units <- 32
epochs <- 50
batch <- 1

# ^ These will be changed later and are not fixed

# as a reminder:
# steps_in = 40
# steps_out = 20


#### Note:
# Various Tunes and Architectures were tried before arriving at the Final Model
# The list includes, but is not limited to:
# - with or without dropout ( values between 0.1 and 0.3)
# - statefulness ( all layers, only first, stateless)
# - rolling /sliding window input data
# - return sequences = True with stateful and batch size 1 vs inverse with full
#   sample batch size
# - number of dense layers (1 or 2)
# - number or units (32,64,128) ; increasing and decreasing number or units 
#   throughout layers
# - stacked and unstacked lstm
# - time distributed dense layer with return sequences True vs inverse
# - different activation functions for layers
# - batch and layer normalization
# - different input features


# In compile:
# loss functions: "mae","mse","mape" (among others to lesser extend)
# optimizers : "adam", "nadam", "adamax" (adamW sadly not available in this
#                                         implementation)


# Models were trained with and without seed and compared, given the stochastic
# nature of LSTM's
# In total, about 500+ Models were evaluated counting the duplicate runs.
# For simplicity RMSE was evaluated as shown, even though that might not be the
# best approach for our scenario


# Later explored:
# - conv1d layer with lstm 
# - encoder decoder lstm ; with and without conv1d
# - + attention 



### Stacked Multistep Univariate LSTM 


# Define the LSTM model using Keras
# tensorflow::set_random_seed(936)

lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = units,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, features)) %>%
  layer_lstm(units = units,return_sequences = F,stateful = T) %>%
  layer_dense(units = steps_out)


# Compiling the model


lstm_model %>% compile(loss = "mse", optimizer = "adam",
                  metrics = c('cosine_proximity'))


summary(lstm_model)



# Model Training

#tensorflow::set_random_seed(936)

lstm_model %>% fit(
  x = x_train,
  y = y_train,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds <- lstm_model %>%
  predict(x_test,batch_size = batch) 





### Rescale Predictions

lstm_preds_ret <- standard_scale(lstm_preds, sf_msft_l_ret,reverse = T) %>% 
  .[1,]
   
  

### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)




p_lstm <- p[2:length(p)]


lstm_month_plot <- msft_monthly_test_l %>% cbind(p_lstm) %>% 
  rename(preds = "p_lstm") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()




###########
####### Final Stacked Multistep Univariate LSTM 


### Rolling Window Approach:

# Notes: Since more data is used, this model runs far slower. We act under the
#        assumption that the lstm model is able to extract structure even though
#        we are not using adjacent windows while also using a stateful model.
#        Furthermore, we assume that the model is able to discern the data input
#        as being lagged and computes its states accordingly. 
#        In practice this seems to hold true. Additionally, unstateful rolling
#        window models do not perform better.


k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_xrw <- keras_model_sequential() %>%
  layer_lstm(units = 256,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, 1)) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 64) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_xrw %>% compile(loss = "mse", optimizer = "adam",
                         metrics = c('cosine_proximity'))


summary(lstm_model_xrw)

# Model Training
tensorflow::set_random_seed(936)
lstm_model_xrw %>% fit(
  x = x_train,            
  y = y_train,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_xrw <- lstm_model_xrw %>%
  predict(x_test,batch_size = batch)


### Rescale Predictions

lstm_preds_ret_xrw <- standard_scale(lstm_preds_xrw, sf_msft_l_ret,reverse = T) %>% 
  .[1,]


### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_xrw/100,
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)

lstm_rw_1_eval<- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)


### Sliding Window Approach

# Note: The sliding window approach is the sensible and rational approach. It
#       makes logical sense, since the windows are adjacent to one another and
#       thus the whole dataset can be seen as one ongoing sequence.
#       Hidden states and cell states can be saved from one batch/sample/input
#       sequence to another without having to make assumptions.
#       The downside is the decreased sample size. 
#       In practice this approach showed to yield slightly better results.



k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_xsw <- keras_model_sequential() %>%
  layer_lstm(units = 256,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, 1)) %>%
  #  layer_batch_normalization() %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 64) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_xsw %>% compile(loss = "mse", optimizer = "adam",
                         metrics = c('cosine_proximity'))


summary(lstm_model_xsw)

# Model Training
tensorflow::set_random_seed(936)
lstm_model_xsw %>% fit(
  x = x_train_sw,                # Trying sliding window
  y = y_train_sw,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_xsw <- lstm_model_xsw %>%
  predict(x_test,batch_size = batch)


### Rescale Predictions

lstm_preds_ret_xsw <- standard_scale(lstm_preds_xsw, sf_msft_l_ret,reverse = T) %>% 
  .[1,]


### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_xsw/100,
                 xi = log(msft_monthly_train_l$adjusted[2510])))

rmse(p[2:length(p)],msft_monthly_test_l$adjusted)

lstm_sw_1_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)





# plot
p_lstm <- p[2:length(p)]


lstm_month_plot <- msft_monthly_test_l %>% cbind(p_lstm) %>% 
  rename(preds = "p_lstm") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()


# Comparison

lstm_win_eval <- data_frame(Approach = c("Rolling window", "Sliding Window"),
                            RMSE = c(lstm_rw_1_eval,lstm_sw_1_eval)) %>%
  kbl() %>% 
  kable_material_dark(full_width = F)
  





##### Adding Features

# In our model features did not have a positive impact. Features were added to
# the model independently but also in groups and pairs. They were tested on
# multiple runs while being scaled and unscaled. 

# Observations on independently tested features:

# Fundamental Analysis Features:
# Interest Rate - > Slightly worse than featureless model
# Volume - > Did not have good results

# Technical Indicators:
# RSI - > Slightly worse than featureless model; was considered for pairing
# MACD - > Slightly worse than featureless model; was considered for pairing;
#          better results when not scaled
# VWAP - > Slightly worse than featureless model; was considered for pairing
# ATR - > Best results but still worse than featureless model; model showed
#         decreased variance
# CMF - > Did not have good results
# MFI - > Did not have good results
# OBV - > Slightly worse than featureless model
# Donchian Channels - > Did not have good results; independent channels or 
#                       grouped


# Adding all features to the model yielded terrible results but will still be
# shown below for demonstration purposes.

# All features were mostly tested on the sliding window data, given faster model
# runtimes.



#### All Fundamental Analysis Features


k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_xf <- keras_model_sequential() %>%
  layer_lstm(units = 256,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, 3)) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 64) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_xf %>% compile(loss = "mse", optimizer = "adam",
                         metrics = c('cosine_proximity'))


summary(lstm_model_xf)

# Model Training
tensorflow::set_random_seed(936)
lstm_model_xf %>% fit(
  x = x_train_3_sw,                # Trying sliding window
  y = y_train_sw,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_xf <- lstm_model_xf %>%
  predict(x_test_3,batch_size = batch)


### Rescale Predictions

lstm_preds_ret_xf <- standard_scale(lstm_preds_xf, sf_msft_l_ret,reverse = T) %>% 
  .[1,]


### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_xf/100,
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)





#### All Features:


k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_xti <- keras_model_sequential() %>%
  layer_lstm(units = 256,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, 12)) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 64) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_xti %>% compile(loss = "mse", optimizer = "adam",
                          metrics = c('cosine_proximity'))


summary(lstm_model_xti)

# Model Training
tensorflow::set_random_seed(936)
lstm_model_xti %>% fit(
  x = x_train_4_sw,                # Trying sliding window
  y = y_train_sw,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_xti <- lstm_model_xti %>%
  predict(x_test_4,batch_size = batch)


### Rescale Predictions

lstm_preds_ret_xti <- standard_scale(lstm_preds_xti, sf_msft_l_ret,reverse = T) %>% 
  .[1,]


### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_xti/100,
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)




###### Just ATR for demonstration


k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_xt <- keras_model_sequential() %>%
  layer_lstm(units = 256,return_sequences = TRUE,stateful = T,
             batch_input_shape = c(batch,steps_in, 2)) %>%
#  layer_batch_normalization() %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 64) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_xt %>% compile(loss = "mse", optimizer = "adam",
                         metrics = c('cosine_proximity'))


summary(lstm_model_xt)

# Model Training
tensorflow::set_random_seed(936)
lstm_model_xt %>% fit(
  x = x_train_5_sw,                # Trying sliding window
  y = y_train_sw,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_xt <- lstm_model_xt %>%
  predict(x_test_5,batch_size = batch)


### Rescale Predictions

lstm_preds_ret_xt <- standard_scale(lstm_preds_xt, sf_msft_l_ret,reverse = T) %>% 
  .[1,]


### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_xt/100,
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)








###################
########## CNN - LSTM

reset_states(lstm_model_cnn) # used before subsequent model runs
k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_cnn_vo <- keras_model_sequential() %>%
  layer_conv_1d(filters =32, kernel_size = 3, strides =1, padding = "same",
                batch_input_shape = c(batch,steps_in,1)) %>% 
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, strides =1,padding = "same") %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = T,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 64,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 40) %>% 
  layer_dense(units = steps_out)


# Compiling the model


lstm_model_cnn_vo %>% compile(loss = "mse", optimizer = "adam",
                         metrics = c('cosine_proximity'))


summary(lstm_model_cnn_vo)



# Model Training
tensorflow::set_random_seed(936)

lstm_model_cnn_vo %>% fit(
  x = x_train_sw,
  y = y_train_sw,
  batch_size = batch,
  epochs = epochs,
  verbose = 1,
  shuffle = FALSE)


# Predictions

lstm_preds_cnn_vo <- lstm_model_cnn_vo %>%
  predict(x_test,batch_size = batch) 




### Rescale Predictions

lstm_preds_ret_cnn_vo <- standard_scale(lstm_preds_cnn_vo, sf_msft_l_ret,reverse = T) %>% 
  .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_cnn_vo/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)

cnn_lstm_vo_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)

# Notes on CNN - LSTM Vector Output Model:

# - Results seem to be very unstable
# - Similarly to previous LSTM model, extra features do not improve model



# Tested on seed (sw dataset) :

# All on 32/64 cnn filters
# - strides 1; padding "same" ; 0.2 dropout / 5.4
# - strides 1; padding "causal" ; 0.3 dropout / 6.6
# - strides 1; padding "causal" ; 0.2 dropout / 5.8
# - strides 1; padding "same" ; 0.3 dropout / 6.0




########
###### Encoder-Decoder CNN-LSTM


reset_states(lstm_model_cnn)
k_clear_session()
 
tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_cnn <- keras_model_sequential() %>%
  layer_conv_1d(filters = 100, kernel_size =7, strides =3 , padding = "same",
                batch_input_shape = c(batch,steps_in,1)) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  # layer_conv_1d(filters = 100, kernel_size =3, strides =3 , padding = "causal") %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.2) %>%
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>% 
  layer_repeat_vector(steps_out) %>% 
  layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 40) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_cnn %>% compile(loss = "mse", optimizer = "adam",
                           metrics = c('cosine_proximity'))

summary(lstm_model_cnn)


# Model Training
tensorflow::set_random_seed(936)

lstm_model_cnn %>% fit(
  x = x_train_sw,
  y = y_train_sw,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)

# Predictions

lstm_preds_cnn <- lstm_model_cnn %>%
  predict(x_test,batch_size = batch) 


### Rescale Predictions
 
lstm_preds_ret_cnn <- standard_scale(lstm_preds_cnn, sf_msft_l_ret,reverse = T) %>% 
  .[1,]

   
   
### Revert Predictions Back to Adjusted Close Price
 
options(digits= 15)
 
p <- exp(diffinv(lstm_preds_ret_cnn/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)
cnn_lstm_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)


# plot
p_lstm <- p[2:length(p)]


lstm_month_plot <- msft_monthly_test_l %>% cbind(p_lstm) %>% 
  rename(preds = "p_lstm") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()


## Note on CNN-LSTM Encoder/Decoder with features:

# - interest rate and volume did not have good results. Individually or paired
#     -> volume showed to be absolutely terrible again
# - RSI,MACD,ATR : Tested individually; About the same if not slightly worse
# - RSI,MACD,ATR : Tested paired; Really bad results
# - All TI's together: Absolutely horrible results


### Note on CNN-LSTM Encoder/Decoder Tune:

# Model is still shows variance but less so than previously experienced

## These were some good tunes:

# Shown are approximate averages

# One Layer Conv1d:

# - 32 filters ;  kernel size 3 ; 3 stride , padding "same" (6.2)
# - 32 filters ;  kernel size 5 ; 3 stride , padding "same" (5.3) causal 5.5
# - 32 filters ;  kernel size 7 ; 3 stride , padding "same" (5.3) causal 6.3

# - 32 filters ;  kernel size 5 ; 1 stride , padding "same" (6.1)
# - 32 filters ;  kernel size 7 ; 1 stride , padding "same" (6.5)

# - 100 filters ; kernel size 5 ; 3 strides , padding "same" (5.7) causal 6.2
# - 100 filters ; kernel size 9 ; 3 strides , padding "same" (5.3) causal 5

# 128 filters

# maybe : 300 filter / kernel 9 / 3 strides ( 6.2)
#         300 filter / kernel 3 / 3 strides ( 6.4)
#         300 filter / kernel 9 / 1 strides ( 6.2)
#         300 filter / kernel 3 / 1 strides ( 6)
# causal or same ^

# maybe : 200 filter / kernel 3 / 3 strides ( 6.7)





# Two Layers Conv1d:

# - 32 /100 /kernel 3 , 3 / strides 3 ; causal (5.6) 
# - 32 /100 /kernel 9 , 9 / strides 3 ; causal (6.6)
# - 32 /100 /kernel 9 , 3 / strides 3 ; causal (6.4)

# - 32 /100 /kernel 3 , 3 / strides 1 ; causal (7)
# - 32 /100 /kernel 9 , 9 / strides 1 ; causal (6.6) 


# Above tunes were tested with and without seed. 


# Comparison of RMSE, recorded on tensorflow seed:

lstm_cnn_fin_eval <- data_frame(Model = c("Final Stacked LSTM", 
                                          "Vector-Ouput CNN-LSTM",
                                          "Encoder-Decoder CNN-LSTM"),
                                RMSE = c(lstm_sw_1_eval,
                                         cnn_lstm_vo_eval,
                                         cnn_lstm_eval))# %>%
# kbl() %>% 
# kable_material_dark(full_width = F)

lstm_cnn_fin_eval %>% 
  kbl() %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(lstm_cnn_fin_eval$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E"))









#### Experiment
######### LSTM Encoder - Decoder with Attention




#########
####### Additive Self-Attention (Bahdanau-style)



# as a reminder:
# steps_in = 40
# steps_out = 20


# Define the model using Keras


reset_states(bs_attention_model)
k_clear_session()
tensorflow::set_random_seed(936)

inputs <- layer_input(batch_shape = c(batch,steps_in, 1))

lstm_output <- inputs %>%
  layer_conv_1d(filters = 100, kernel_size =5, strides =3 , padding = "causal",
                batch_input_shape = c(batch,steps_in,1)) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_conv_1d(filters = 100, kernel_size =3, strides =3 , padding = "causal") %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_repeat_vector(steps_out) %>%
  layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2)



## Additive Attention

outputs <- layer_additive_attention(object = c(lstm_output,lstm_output),
                                    use_scale = TRUE,
                                    batch_size = 1,
                                    trainable = T,
                                    causal = TRUE) %>%
  # layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50) %>%
  layer_dense(units =steps_out)




# Additive attention model with cnn-lstm-att-lstm

bs_attention_model <- keras_model(inputs=inputs,outputs = outputs)






# Compiling the model


bs_attention_model %>% compile(loss = "mse", optimizer = "adam",
                            metrics = c('cosine_proximity'))



summary(bs_attention_model)

# Model Training
tensorflow::set_random_seed(936)

bs_attention_model %>% fit(
  x = x_train_sw,
  y = y_train_sw,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)


# Predictions

bs_attention_model_preds <- bs_attention_model %>%
  predict(x_test,batch_size = batch) 





### Rescale Predictions

bs_att_ret_preds <- standard_scale(bs_attention_model_preds, sf_msft_l_ret,reverse = T) %>% 
  .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(bs_att_ret_preds/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)


bs_att_cnn_lstm_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)



# Notes on Additive-Self Attention:
# Additive self attention can be employed in many ways. To give some examples:
# 2x cnn - lstm - addatt - lstm
# 1x cnn - lstm - addatt - lstm
# 1x cnn - addatt - 2x lstm        etc.

# Additionally we can utilize it in Vector Output models or Encoder-Decoders

# - cnn-lstm with split lstm and attention in between showed good results and 
#   was rarely unstable
# - with both lstm layers after attention, results were not great and no clear
#   improvement could be observed
# - tested models did not show good results with features in initial testing;
#   some seemed better than others with no clear improvement

# Generally more research is necessary to properly implement it and much still
# has to be explored

# It is also to be noted that different tunes of dropout, norm etc were tested
# in addition to cnn and lstm tunes and attention tunes. Also, as mentioned
# before, tested with and without seed





#########
####### Multiplicative Self-Attention (Luong-style attention)



# Define the model using Keras
tensorflow::set_random_seed(936)

#reset_states(ls_attention_model)
k_clear_session()


inputs <- layer_input(batch_shape = c(batch,steps_in, 1))

lstm_output <- inputs %>%
  layer_conv_1d(filters = 100, kernel_size =7, strides =3 , padding = "causal",
                batch_input_shape = c(batch,steps_in,1)) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>% 
  layer_conv_1d(filters = 100, kernel_size =3, strides =3 , padding = "causal") %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>%
  layer_repeat_vector(steps_out) # %>%
  # layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.2)




## attention layer

outputs <- layer_attention(
  inputs = list(lstm_output, lstm_output),
  use_scale = TRUE,
  batch_size = 1,
  trainable = T) %>%
  layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50) %>%
  layer_dense(units =steps_out)



ls_attention_model <- keras_model(inputs=inputs,outputs = outputs)




# Compiling the model

ls_attention_model %>% compile(loss = "mse", optimizer = "adam",
                            metrics = c('cosine_proximity'))

summary(ls_attention_model)

# Model Training
tensorflow::set_random_seed(936)

ls_attention_model %>% fit(
  x = x_train_sw,
  y = y_train_sw,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)


# Predictions

ls_attention_model_preds <- ls_attention_model %>%
  predict(x_test,batch_size = batch) 



### Rescale Predictions

ls_att_ret_preds <- standard_scale(ls_attention_model_preds, sf_msft_l_ret,reverse = T) %>% 
  .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(ls_att_ret_preds/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)

ls_att_cnn_lstm_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)


# Note on Multiplicative Attention:
# Similarly to Additive Attention there are many options. Many different tunes
# can also be tried out for the respective cnn and lstm layers. For instance, 
# good results were achieved on a tune including decreasing cnn filters.

# Some good tunes, tested with and without seed:
# - cnn-lstm with split lstm attention in between showed good results while a 
#   bit unstable 
# - both lstm layers after attention were a bit unstable and no clear 
#   improvement; worse than split lstm
# - same^ + double cnn with causal pad : somewhat good results but unstable
# 
# some of the tested feature results: 
# - all: no clear improvement / bad
# - no vol : no clear improvement
# - just ti’s no atr: no improvement
# - macd+vwap+rsi: no clear improvement 

# It is also to be noted that different tunes of dropout, norm etc were tested
# in addition to cnn and lstm tunes and attention tunes.


# Generally more research is necessary to properly implement it and much still
# has to be explored






#########
####### Multi-Head Attention


reset_states(mh_attention_model)
k_clear_session()
tensorflow::set_random_seed(936)

# Input Layer:

inputs <- layer_input(batch_shape = c(batch,steps_in, 1))


# We can choose to additionally add self-attention:

add_Att <- layer_additive_attention(object = c(inputs,inputs),
                                    use_scale = TRUE,
                                    batch_size = 1,
                                    trainable = T,
                                    causal = TRUE)


cnn_output <- add_Att %>%
  layer_conv_1d(filters = 100, kernel_size =9, strides =3 , padding = "same") %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_repeat_vector(steps_out) #%>%
  # layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.2)


# If self-attention at the beginning is not chosen:

# cnn_output <- inputs %>%
#   layer_conv_1d(filters =100, kernel_size = 9, strides =3, padding = "same") %>%
#   layer_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_dropout(rate = 0.2) #%>%
#   # layer_conv_1d(filters = 100, kernel_size = 3, strides =3,padding = "causal") %>%
#   # layer_normalization() %>%
#   # layer_activation_leaky_relu() %>%
#   # layer_dropout(rate = 0.2) %>%
#   layer_max_pooling_1d(pool_size = 2) %>%
#   layer_flatten() %>%
#   layer_repeat_vector(steps_out) %>%
#   layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
#   layer_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_dropout(rate = 0.2)



# Decoder with multi-head

# For num_heads and key_dim:
# Generally speaking, d_k = key_dim
#                     d_v = embed_dim / num_heads -> d_v > d_k = dim reduction
#                                                    d_v < d_k = dim expansion

multi_head <- layer_multi_head_attention( num_heads = 2,
                                         key_dim = 128)


output <- multi_head(cnn_output,cnn_output) %>%
  # layer_max_pooling_1d(pool_size = 2) %>%
  # layer_flatten() %>%
  # layer_repeat_vector(steps_out) %>%
  # layer_lstm(units = 256,return_sequences = T,stateful = T) %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.2) %>%     #could be chosen instead of before multi
  layer_lstm(units = 128,return_sequences = F,stateful = T) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50) %>%
  layer_dense(units =steps_out)





mh_attention_model <- keras_model(inputs=inputs,outputs = output)

  
# Compiling the model


mh_attention_model %>% compile(loss = "mse", optimizer = "adam",
                           metrics = c('cosine_proximity'))



summary(mh_attention_model)

# Model Training
tensorflow::set_random_seed(936)

mh_attention_model %>% fit(
  x = x_train_sw,
  y = y_train_sw,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)


# Predictions

mh_attention_model_preds <- mh_attention_model %>%
  predict(x_test,batch_size = batch) 





### Rescale Predictions

mh_att_ret_preds <- standard_scale(mh_attention_model_preds, sf_msft_l_ret,reverse = T) %>% 
  .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(mh_att_ret_preds/100, 
                 xi = log(msft_monthly_train_l$adjusted[2510])))


rmse(p[2:length(p)],msft_monthly_test_l$adjusted)


mh_att_cnn_lstm_eval <- rmse(p[2:length(p)],msft_monthly_test_l$adjusted)




p_lstm <- p[2:length(p)]

lstm_month_plot <- msft_monthly_test_l %>% cbind(p_lstm) %>% 
  rename(preds = "p_lstm") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()


# Notes on Multi-Head:
# Tested on various tunes but needs the most extra research and exploration
# Added self-attention works well in some cases
# Features had mixed result and needs more testing
# By far the most options. Initial impression was that dim red works better.




# Attention Comparison:

att_fin_eval <- data_frame(Model = c("Additive Attention",
                                     "Multiplicative Attention",
                                     "Multi-Head Attention"),
                                RMSE = c(bs_att_cnn_lstm_eval,
                                         ls_att_cnn_lstm_eval,
                                         mh_att_cnn_lstm_eval))# %>%
# kbl() %>% 
# kable_material_dark(full_width = F)

att_fin_eval %>% 
  kbl() %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(att_fin_eval$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E"))









################
######## CNN-LSTM Next Day Prediction
########################

########
##### Encoder-Decoder Model

reset_states(lstm_model_day)
k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_day <- keras_model_sequential() %>%
  layer_conv_1d(filters = 50, kernel_size =3, strides =3 , padding = "causal",
                batch_input_shape = c(batch,steps_in,1)) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(filters = 50, kernel_size =3, strides =3 , padding = "causal") %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>% 
  layer_repeat_vector(steps_out) %>% 
  layer_lstm(units = 32,return_sequences = T,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 16,return_sequences = F,stateful = F) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 5) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_day %>% compile(loss = "mse", optimizer = "adam",
                           metrics = c('cosine_proximity'))

summary(lstm_model_day)


# Model Training
tensorflow::set_random_seed(936)

lstm_model_day %>% fit(
  x = x_train_sw_day,
  y = y_train_sw_day,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)

# Predictions

lstm_preds_day <- lstm_model_day %>%
  predict(x_test_day,batch_size = batch) 


### Rescale Predictions

lstm_preds_ret_day <- standard_scale(lstm_preds_day, sf_msft_l_day_ret,
                                     reverse = T) %>% .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_day/100, 
                 xi = log(msft_day_train_l$adjusted[2548])))


rmse(p[2:length(p)],msft_day_test_l$adjusted)


cnn_lstm_day_eval <- rmse(p[2:length(p)],msft_day_test_l$adjusted)

# plot
p_lstm <- p[2:length(p)]


lstm_day_plot <- msft_day_test_l %>% cbind(p_lstm) %>% 
  rename(preds = "p_lstm") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()


##########
##### Next-Day Prediction Vector-Output


reset_states(lstm_model_day_2)
k_clear_session()

tensorflow::set_random_seed(936)

# Define the LSTM model using Keras
lstm_model_day_2 <- keras_model_sequential() %>%
  layer_conv_1d(filters =32, kernel_size = 3, strides =1, padding = "same",
                batch_input_shape = c(batch,steps_in,1)) %>% 
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, strides =1,padding = "same") %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 64,return_sequences = T,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 32,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 5) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_day_2 %>% compile(loss = "mse", optimizer = "adam",
                           metrics = c('cosine_proximity'))

summary(lstm_model_day_2)


# Model Training
tensorflow::set_random_seed(936)

lstm_model_day_2 %>% fit(
  x = x_train_sw_day,
  y = y_train_sw_day,
  batch_size = batch,
  epochs = 50,
  verbose = 1,
  shuffle = FALSE)

# Predictions

lstm_preds_day_2 <- lstm_model_day_2 %>%
  predict(x_test_day,batch_size = batch) 


### Rescale Predictions

lstm_preds_ret_day_2 <- standard_scale(lstm_preds_day_2, sf_msft_l_day_ret,
                                     reverse = T) %>% .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_day_2/100, 
                 xi = log(msft_day_train_l$adjusted[2548])))


rmse(p[2:length(p)],msft_day_test_l$adjusted)



















################
######## CNN-LSTM Next Year Prediction
########################


reset_states(lstm_model_year)
k_clear_session()

tensorflow::set_random_seed(936)
# Define the LSTM model using Keras
lstm_model_year <- keras_model_sequential() %>%
  layer_conv_1d(filters = 100, kernel_size =7, strides =3 , padding = "same",
                batch_input_shape = c(batch,steps_in,1)) %>%
  layer_normalization() %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.5) %>%
  # layer_conv_1d(filters = 100, kernel_size =3, strides =3 , padding = "same") %>%
  # layer_normalization() %>%
  # layer_activation_leaky_relu() %>%
  # layer_dropout(rate = 0.5) %>%
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>% 
  layer_repeat_vector(steps_out) %>% 
  layer_lstm(units = 512,return_sequences = T,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units = 256,return_sequences = F,stateful = T) %>%
  layer_normalization() %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = steps_out)


# Compiling the model

lstm_model_year %>% compile(loss = "mse", optimizer = "adam",
                           metrics = c('cosine_proximity'))

summary(lstm_model_year)


# Model Training
tensorflow::set_random_seed(936)

lstm_model_year %>% fit(
  x = x_train_sw_year,
  y = y_train_sw_year,
  batch_size = batch,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE)

# Predictions

lstm_preds_year <- lstm_model_year %>%
  predict(x_test_year,batch_size = batch) 


### Rescale Predictions

lstm_preds_ret_year <- standard_scale(lstm_preds_year, sf_msft_l_year_ret,
                                     reverse = T) %>% .[1,]



### Revert Predictions Back to Adjusted Close Price

options(digits= 15)

p <- exp(diffinv(lstm_preds_ret_year/100, 
                 xi = log(msft_year_train_l$adjusted[2030])))

rmse(p[2:length(p)],msft_year_test_l$adjusted)

cnn_lstm_year_eval <- rmse(p[2:length(p)],msft_year_test_l$adjusted)


# plot
p_lstm <- p[2:length(p)]


lstm_year_plot <- msft_year_test_l %>% cbind(p[2:length(p)]) %>% 
  rename(preds = "p[2:length(p)]") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()






#### LSTM Horizon Evaluation


eval_lstm <- data_frame(Horizon = c("Next-Day","Next-Month","Next-Year"),
                       RMSE = c(cnn_lstm_day_eval,
                                cnn_lstm_eval,
                                cnn_lstm_year_eval)) 

eval_lstm %>% 
  kbl() %>%
  kable_material_dark(full_width = F)


# For colored values
eval_lstm %>% 
  kbl() %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(eval_lstm$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E"))


