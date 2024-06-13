
##################################
################## Final- Validation
###########################





###### All Model Results on all Forecast Horizons


full_eval <- data_frame(Method = c("Prophet",
                                  "ARIMA",
                                  "CNN-LSTM"),
                       "Next-Day Prediction" = c(eval_pro$RMSE[1],
                                    eval_arima$RMSE[1],
                                    eval_lstm$RMSE[1]),
                       "Next-Month Prediction" = c(eval_pro$RMSE[2],
                                                 eval_arima$RMSE[2],
                                                 eval_lstm$RMSE[2]),
                       "Next-Year Prediction" = c(eval_pro$RMSE[3],
                                                 eval_arima$RMSE[3],
                                                 eval_lstm$RMSE[3])
                       ) #%>% 
  #kbl() %>% 
# kable_material_dark(full_width = F)

full_eval %>% 
  kbl() %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(full_eval$"Next-Day Prediction"[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(3, color = "white", 
              background = spec_color(full_eval$"Next-Month Prediction"[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(4, color = "white", 
              background = spec_color(full_eval$"Next-Year Prediction"[1:3], end = 0.7,
                                      direction = -1,option="E")) 


 








#### Final-Validation on 6 Stocks

# We will conduct the final-validation on the CNN-LSTM models as they showed
# the most promise and beat out the other models. Additionally, they showed
# the most room for improvement for further research.



### Function to analyse stocks on specified horizon, given their stock ticker 
#   as input




final_validation <- function(ticker_list,
                             horizon = c("Next-Day","Next-Month", 
                                                    "Next-Year"),
                             split =c("sliding window","rolling window"),
                             seed = 936){
  total <-data_frame()
  for (ticker in ticker_list) {
    
    symbol <- getSymbols(ticker)
    data <- tq_get(symbol,get="stock.prices")
    
    temp_adj <- data %>% select(symbol,date,adjusted) %>% mutate(date = ymd(date))
    
    adj <- right_join(temp_adj,msft_l, by= "date") %>% select(-adjusted.y) %>% 
      rename(adjusted =adjusted.x)# So time is same
    
    
    # Function to get scaling factors and reverse scaling:
    scaling_factors <- function(data){
      factors <- c(mean = mean(data), sd = sd(data))
      return(factors)
    }
    standard_scale <- function(data, scaling_factors, reverse = FALSE) {
      
      if (reverse) temp <- (data * scaling_factors[2]) + scaling_factors[1]
      else temp <- (data - scaling_factors[1]) / scaling_factors[2]
      
      scaled <- temp %>% as.matrix()
      return(scaled)
    }
    ### Splitting Data
    # IF x TRUE then x train, if FALSE then x test
    
    kera_transform <- function(data, x = TRUE, steps_in = 12, steps_out = 1) {
      
      if (x) {
        
        temp <- sapply(
          1:(length(data) - steps_in - 2*(steps_out) + 1)
          ,function(x) data[x:(x + steps_in - 1), 1]
        ) %>% t()
        
        set <- array(
          temp %>% unlist() %>% as.numeric()
          ,dim = c(nrow(temp), steps_in, 1)
        )
        
      }  else {
        if (steps_out != 1) {
          temp <- sapply(
            (1 + steps_in):(length(data) - 2*(steps_out) + 1)
            ,function(x) data[x:(x + steps_out - 1), 1]
          ) %>% t()
          
          set <- array(
            temp %>% unlist() %>% as.numeric()
            ,dim = c(nrow(temp), steps_out)
          )
        } else {
          ran <- data[(1+steps_in):(length(data) - 2*(steps_out) + 1)]
          temp <- split(ran, seq_along(ran))
          
          set <- temp %>% unlist() %>% as.numeric() %>% array %>% 
            array_reshape(dim=c(length(temp),1))
          
        }
      }
      
      return(set)
      
    }
    kera_pred_transform <- function(data,steps_in = 12, steps_out = 1){
      temp <- data[(length(data) - steps_in - steps_out + 1):(length(data)-steps_out)]
      set <- array(temp, c(1, steps_in,1))
      return(set)
    }
    
    
    kera_transform_sw <- function(data, x = TRUE,steps_in = 12, steps_out = 1){
      
      max <- length(data) - steps_out - steps_in
      low <- (max - (floor((length(data) - steps_out-steps_in) /steps_in) * steps_in)) +1 
      ran <- data[low:max]
      
      if (x) {
        
        temp <- split(ran, ceiling(seq_along(ran)/steps_in))
        
        set <- temp %>% unlist() %>% as.numeric() %>% array %>% 
          array_reshape(dim=c(length(temp),steps_in,1))
        
      } 
      
      else {
        if (steps_out !=1) {
          s <- seq(low+steps_in,length(data)-steps_out-steps_in+1,steps_in)
          set <- sapply(s,
                        function(x) data[x:(x + steps_out - 1)]
          ) %>% t() 
        } else {
          ran <- data[sum(low+steps_in):(length(data) -steps_out-steps_in+1)]
          temp <- ran[seq(1,length(ran), by = steps_in)]
          set <- temp %>% unlist() %>% as.numeric() %>% array %>% 
            array_reshape(dim=c(length(temp),1))
        }
        
      }
      
      return(set)
    }
    
    ## Day-Prediction
    if (horizon == "Next-Day") {
      
      
      day_train <- adj[1:sum(length(adj$date)),] 

      day_test <- adj[sum(length(adj$date)),]

      day_ret <- 100*diff(log(day_train$adjusted)) 
      
      # Scaling returns subset:
      sf_day_ret <- scaling_factors(day_ret)
      
      day_ret <- standard_scale(day_ret, sf_day_ret)
      
      steps_in <- 15 
      steps_out <- 1
      x_test_day <- kera_pred_transform(day_ret,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)
      
      if (split == "rolling window") {
      
        ### Splitting Data
      steps_in <- 15 
      steps_out <- 1
      
      x_train_day <- kera_transform(day_ret, x = T,
                                    steps_in = steps_in, 
                                    steps_out = steps_out)
      
      y_train_day <- kera_transform(day_ret, x = F,
                                    steps_in = steps_in, 
                                    steps_out = steps_out)
      

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
      
      lstm_model_day %>% compile(loss = "mse", optimizer = "adam",
                                 metrics = c('cosine_proximity'))
      
      # Model Training
      tensorflow::set_random_seed(936)
      
      lstm_model_day %>% fit(
        x = x_train_day,
        y = y_train_day,
        batch_size = batch,
        epochs = 20,
        verbose = 1,
        shuffle = FALSE)
      
      # Predictions
      
      lstm_preds_day <- lstm_model_day %>%
        predict(x_test_day,batch_size = batch) 
      
      
      ### Rescale Predictions
      lstm_preds_ret_day <- standard_scale(lstm_preds_day, sf_day_ret,
                                           reverse = T) %>% .[1,]
      ### Revert Predictions Back to Adjusted Close Price
      options(digits= 15)
      
      p <- exp(diffinv(lstm_preds_ret_day/100, 
                       xi = log(day_train$adjusted[2549])))
      
      
      tmp <- rmse(p[2:length(p)],day_test$adjusted)
      
      }
      
      if (split == "sliding window") {
        
        
        steps_in <- 15
        steps_out <- 1
        
        x_train_sw_day <- kera_transform_sw(day_ret, x = T,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)
        
        y_train_sw_day <- kera_transform_sw(day_ret, x = F,
                                            steps_in = steps_in,
                                            steps_out = steps_out)
        
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
        
        lstm_model_day %>% compile(loss = "mse", optimizer = "adam",
                                   metrics = c('cosine_proximity'))
        
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
        lstm_preds_ret_day <- standard_scale(lstm_preds_day, sf_day_ret,
                                             reverse = T) %>% .[1,]
        ### Revert Predictions Back to Adjusted Close Price
        options(digits= 15)
        
        p <- exp(diffinv(lstm_preds_ret_day/100, 
                         xi = log(msft_day_train_l$adjusted[2548])))
        
        
        tmp <- rmse(p[2:length(p)],msft_day_test_l$adjusted)
        
        
      }
      
      
      
     
    } # end of day
  
    
    
    ## Month-Prediction
    
    if (horizon == "Next-Month") {
      
      
      month_train <- adj[1:sum(length(adj$date)-20),]
      
      month_test <- adj[sum(length(adj$date)-19):sum(length(adj$date)),]
      
      month_ret <- 100*diff(log(adj$adjusted)) 
      
      month_ret_sub <- month_ret[1:sum(length(month_ret))]
      
      # Scaling returns subset:
      sf_month_ret <- scaling_factors(month_ret_sub)
      
      month_ret_sub <- standard_scale(month_ret_sub, sf_month_ret)
      
      
      steps_in <- 40
      steps_out <- 20
      x_test_month <- kera_pred_transform(month_ret_sub,
                                    steps_in = steps_in, 
                                    steps_out = steps_out)
      
      if (split == "rolling window") {
        
        steps_in <- 40
        steps_out <- 20
        
        x_train_month <- kera_transform(month_ret_sub, x = T,
                                  steps_in = steps_in, 
                                  steps_out = steps_out)
        
        y_train_month <- kera_transform(month_ret_sub, x = F,
                                  steps_in = steps_in, 
                                  steps_out = steps_out)
        
        tensorflow::set_random_seed(936)
        # Define the LSTM model using Keras
        lstm_model_month <- keras_model_sequential() %>%
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
        
        lstm_model_month %>% compile(loss = "mse", optimizer = "adam",
                                   metrics = c('cosine_proximity'))
        
        
        # Model Training
        tensorflow::set_random_seed(936)
        
        lstm_model_month %>% fit(
          x = x_train_month,
          y = y_train_month,
          batch_size = batch,
          epochs = 20,
          verbose = 1,
          shuffle = FALSE)
        
        # Predictions
        
        lstm_preds_month <- lstm_model_month %>%
          predict(x_test_month,batch_size = batch) 
        
        
        ### Rescale Predictions
        
        lstm_preds_ret_month <- standard_scale(lstm_preds_month, sf_month_ret,reverse = T) %>% 
          .[1,]
        
        
        
        ### Revert Predictions Back to Adjusted Close Price
        
        options(digits= 15)
        
        p <- exp(diffinv(lstm_preds_ret_month/100, 
                         xi = log(month_train$adjusted[length(month_train$adjusted)])))
        
        
        
       tmp <- rmse(p[2:length(p)],month_test$adjusted)
        
        
        
        }
      if (split == "sliding window") {
        
        steps_in <- 40
        steps_out <- 20
        x_train_sw_month <- kera_transform_sw(month_ret_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)
        
        y_train_sw_month <- kera_transform_sw(month_ret_sub, x = F,
                                        steps_in = steps_in,
                                        steps_out = steps_out)
        
        tensorflow::set_random_seed(936)
        # Define the LSTM model using Keras
        lstm_model_month <- keras_model_sequential() %>%
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
        
        lstm_model_month %>% compile(loss = "mse", optimizer = "adam",
                                     metrics = c('cosine_proximity'))
        
        
        # Model Training
        tensorflow::set_random_seed(936)
        
        lstm_model_month %>% fit(
          x = x_train_sw_month,
          y = y_train_sw_month,
          batch_size = batch,
          epochs = 20,
          verbose = 1,
          shuffle = FALSE)
        
        # Predictions
        
        lstm_preds_month <- lstm_model_month %>%
          predict(x_test_month,batch_size = batch) 
        
        
        ### Rescale Predictions
        
        lstm_preds_ret_month <- standard_scale(lstm_preds_month, sf_month_ret,reverse = T) %>% 
          .[1,]
        
        
        
        ### Revert Predictions Back to Adjusted Close Price
        
        options(digits= 15)
        
        p <- exp(diffinv(lstm_preds_ret_month/100, 
                         xi = log(month_train$adjusted[length(month_train$adjusted)])))
        
        
       tmp<- rmse(p[2:length(p)],month_test$adjusted)
        
      
        
        
        
      } 
      
      
      
    } #end of month
    
    
    ## Next-Year Prediction
    
    
    if (horizon == "Next-Year") {
      
      
      year_train <- adj[1:sum(length(adj$date)),]
      # See Note in LSTM Day Wrangling 
      
      year_test <- adj[sum(length(adj$date)-259):sum(length(adj$date)),]
      
      year_ret <- 100*diff(log(year_train$adjusted)) 
      
      
      # Scaling returns subset:
      sf_year_ret <- scaling_factors(year_ret)
      
      year_ret_sub <- standard_scale(year_ret, sf_year_ret)
      
      
      steps_in <- 520
      steps_out <- 260
      x_test_year <- kera_pred_transform(year_ret_sub,
                                          steps_in = steps_in, 
                                          steps_out = steps_out)
      if (split == "rolling window") {
        
        steps_in <- 520
        steps_out <- 260
        
        x_train_year <- kera_transform(year_ret_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)
        
        y_train_year <- kera_transform(year_ret_sub, x = F,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)
        
        
        
        tensorflow::set_random_seed(936)
        # Define the LSTM model using Keras
        lstm_model_year <- keras_model_sequential() %>%
          layer_conv_1d(filters = 100, kernel_size =7, strides =3 , padding = "same",
                        batch_input_shape = c(batch,steps_in,1)) %>%
          layer_normalization() %>%
          layer_activation_leaky_relu() %>%
          layer_dropout(rate = 0.5) %>%
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

        lstm_model_year %>% compile(loss = "mse", optimizer = "adam",
                                    metrics = c('cosine_proximity'))
        
        
        # Model Training
        tensorflow::set_random_seed(936)
        
        lstm_model_year %>% fit(
          x = x_train_year,
          y = y_train_year,
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
        
        
        tmp <- rmse(p[2:length(p)],msft_year_test_l$adjusted)
        
      }
      
      
      if (split == "sliding window"){
        
        steps_in <- 520
        steps_out <- 260
        x_train_sw_year <- kera_transform_sw(year_ret_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)
        
        y_train_sw_year <- kera_transform_sw(year_ret_sub, x = F,
                                              steps_in = steps_in,
                                              steps_out = steps_out)
        
        
        tensorflow::set_random_seed(936)
        # Define the LSTM model using Keras
        lstm_model_year <- keras_model_sequential() %>%
          layer_conv_1d(filters = 100, kernel_size =7, strides =3 , padding = "same",
                        batch_input_shape = c(batch,steps_in,1)) %>%
          layer_normalization() %>%
          layer_activation_leaky_relu() %>%
          layer_dropout(rate = 0.5) %>%
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
        
        
       tmp<- rmse(p[2:length(p)],msft_year_test_l$adjusted)
        
        
      }
      
      }
    
  
    
    results <- data_frame(ticker =ticker, RMSE =tmp)
    total <- rbind(total,results)
    
    
    
  } 
  return(total) # for loop end
}



## Final Validation
# For some reason seed does not work

### Next-Day

ticker_list= list("MSFT","AAPL", "TSLA","GOOG","NVDA","AMD")

fin_val_day<- final_validation(ticker_list=ticker_list, horizon ="Next-Day",
                 split ="sliding window")

fin_val_day %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

fin_val_day %>% 
  rename(Ticker = ticker) %>% 
  kbl(caption = "One-Day Forecast Horizon") %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(fin_val_day$RMSE[1:6], end = 0.7,
                                      direction = -1,option="E")) 



### Next-Month

fin_val_month <- final_validation(ticker_list=ticker_list, horizon ="Next-Month",
                 split ="sliding window")

fin_val_month %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

fin_val_month %>% 
  rename(Ticker = ticker) %>% 
  kbl(caption = "One-Month Forecast Horizon") %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(fin_val_month$RMSE[1:6], end = 0.7,
                                      direction = -1,option="E")) 



### Next-Year


fin_val_year <- final_validation(ticker_list=ticker_list, horizon ="Next-Year",
                 split ="sliding window")


fin_val_year %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

fin_val_year %>% 
  rename(Ticker = ticker) %>% 
  kbl(caption = "One-Year Forecast Horizon") %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(fin_val_year$RMSE[1:6], end = 0.7,
                                      direction = -1,option="E")) 







