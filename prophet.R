##################################
################## Prophet
###########################


# Install prophet and load library:

# install.packages("prophet")

library(prophet)


# doParallel to enable computation with multiple cores:

library(doParallel)
# detectCores() # - to find out number of cores available to you
registerDoParallel(cores = 8) # Device with 10 cores 
# (only incremental change the more cores are used)





#########
#### Base - Model


# Initializing prophet model
msft_pro_mod <- prophet(msft_x)


# Creating future date dataframe containing future dates to predict
msft_future <- make_future_dataframe(msft_pro_mod,periods = 1)


# Predictions

msft_preds <- predict(msft_pro_mod, msft_future)

msft_preds

dyplot.prophet(msft_pro_mod,msft_preds)

plot(msft_pro_mod,msft_preds)



#### Prophet base prediction

msft_pro_mod_2 <- prophet(msft_x)


# Creating future date dataframe containing future dates to predict
msft_future_2 <- make_future_dataframe(msft_pro_mod,periods = 365)


# Predictions

msft_preds_2 <- predict(msft_pro_mod_2, msft_future_2)

# msft_preds_2

dyplot.prophet(msft_pro_mod_2,msft_preds_2)




################
####### Prophet next trading day prediction



#### Initializing prophet model
msft_pro_mod_day <- prophet(holidays = NULL, 
                            changepoint.prior.scale = 0.4,
                            seasonality.prior.scale = 10,
                            seasonality.mode = "additive",
                            changepoint.range = 0.9,
                            daily.seasonality = F)


msft_pro_mod_day <- fit.prophet(msft_pro_mod_day, msft_x_daily_train)




#### Creating future date dataframe containing future dates to predict

# Note: make_future_dataframe does not work with holidays; periods to do not
#     include holidays

msft_future_day <- msft_x[1:sum(length(msft_x$ds)-1),] %>% select(ds)
# msft_future_day <- make_future_dataframe(msft_pro_mod_day, periods = 1)



#### Predictions

msft_preds_day <- predict(msft_pro_mod_day, msft_future_day)


dyplot.prophet(msft_pro_mod_day,msft_preds_day)





#### Model Evaluation


### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_day_f <- RMSE(msft_x[1:sum(length(msft_x$ds)-1),]$y ,msft_preds_day$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_day <- RMSE(msft_x_daily_test$y, 
                 msft_preds_day[length(msft_preds_day$ds),]$yhat)


eval_pro <- data_frame(Prediction = "Next Day", RMSE = rmse_day )
eval_pro_full <- data_frame(Prediction = "Next Day", RMSE = rmse_day_f )


### MSE

# MSE of all values (stock prices) in the dataset:

mse_day_f <- mse(msft_x[1:sum(length(msft_x$ds)-1),]$y ,msft_preds_day$yhat)

# MSE of the unknown future period we are trying to predict:

mse_day <- mse(msft_x_daily_test$y, 
               msft_preds_day[length(msft_preds_day$ds),]$yhat)


eval_pro <- left_join(eval_pro, data_frame(Prediction = "Next Day",
                                           MSE = mse_day ))
eval_pro_full <- left_join(eval_pro_full, data_frame(Prediction = "Next Day",
                                                MSE = mse_day_f ))


### MAE

# MAE of all values (stock prices) in the dataset:

mae_day_f <- mae(msft_x[1:sum(length(msft_x$ds)-1),]$y ,msft_preds_day$yhat)

# MAE of the unknown future period we are trying to predict:

mae_day <- mae(msft_x_daily_test$y, 
               msft_preds_day[length(msft_preds_day$ds),]$yhat)


eval_pro <- left_join(eval_pro, data_frame(Prediction = "Next Day",
                                           MAE = mae_day ))
eval_pro_full <- left_join(eval_pro_full, data_frame(Prediction = "Next Day",
                                                MAE = mae_day_f ))




### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_day_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-1),]$y ,msft_preds_day$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_day <- Metrics::mape(msft_x_daily_test$y, 
                 msft_preds_day[length(msft_preds_day$ds),]$yhat)


eval_pro <- left_join(eval_pro, data_frame(Prediction = "Next Day",
                                           MAPE = mape_day ))
eval_pro_full <- left_join(eval_pro_full, data_frame(Prediction = "Next Day",
                                                MAPE = mape_day_f ))









###############
####### Prophet next month prediction ( 20-day trading period)


#### Initializing prophet model and adding holidays (weekends included)

msft_pro_mod_month <- prophet(holidays = NULL, 
                              changepoint.prior.scale = 0.3,
                              seasonality.prior.scale = 5,
                              holidays.prior.scale = 10,
                              seasonality.mode = "additive",
                              changepoint.range = 0.9,
                              daily.seasonality = F)
msft_pro_mod_month <- add_country_holidays(msft_pro_mod_month, 
                                           country_name = 'US')


msft_pro_mod_month <- fit.prophet(msft_pro_mod_month, msft_x_monthly_train)




#### Creating future date dataframe containing future dates to predict

# Note: make_future_dataframe does not work with holidays; periods to do not
#     include holidays


msft_future_month <- msft_x[1:sum(length(msft_x$ds)-20),] %>% select(ds)



#### Predictions

msft_preds_month <- predict(msft_pro_mod_month, msft_future_month)

# msft_preds_month %>% arrange(desc(ds)) %>% select(ds,yhat) %>% view

dyplot.prophet(msft_pro_mod_month,msft_preds_month)

plot(msft_pro_mod_month,msft_preds_month)




#### Model Evaluation


### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_mon_f<- RMSE(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_mon <- RMSE(msft_x_monthly_test$y, 
     msft_preds_month[sum(length(msft_preds_month$ds)-19):length(msft_preds_month$ds),]$yhat)





### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon <- mse(msft_x_monthly_test$y, 
    msft_preds_month[sum(length(msft_preds_month$ds)-19):length(msft_preds_month$ds),]$yhat)




### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon <- mae(msft_x_monthly_test$y, 
    msft_preds_month[sum(length(msft_preds_month$ds)-19):length(msft_preds_month$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon <- Metrics::mape(msft_x_monthly_test$y, 
    msft_preds_month[sum(length(msft_preds_month$ds)-19):length(msft_preds_month$ds),]$yhat)




eval_pro <- full_join(eval_pro, data_frame(Prediction = "Next Month",
                                           RMSE = rmse_mon,
                                           MSE = mse_mon,
                                           MAE = mae_mon,
                                           MAPE = mape_mon))
eval_pro_full <- full_join(eval_pro_full, data_frame(Prediction = "Next Month",
                                                     RMSE = rmse_mon_f,
                                                     MSE = mse_mon_f,
                                                     MAE= mae_mon_f,
                                                     MAPE = mape_mon_f))


## Individual Model Components


prophet_plot_components(msft_pro_mod_month,msft_preds_month)






#########
##### Next Month Prediction with External Regressor


msft_pro_mod_month_2 <- prophet(holidays = NULL, 
                              changepoint.prior.scale = 0.5,
                              seasonality.prior.scale = 5,
                              holidays.prior.scale = 10,
                              seasonality.mode = "additive",
                              changepoint.range = 0.9,
                              daily.seasonality = F)
msft_pro_mod_month_2 <- add_country_holidays(msft_pro_mod_month_2, 
                                           country_name = 'US')

msft_pro_mod_month_2 <- add_regressor(msft_pro_mod_month_2, "rsi")

msft_pro_mod_month_2 <- fit.prophet(msft_pro_mod_month_2,
                                    msft_x_monthly_train_rsi)




#### Creating future date dataframe containing future dates to predict


msft_future_month_2 <- msft_x[1:sum(length(msft_x$ds)-20),] %>% 
  left_join(msft_rsi_av) %>% select(ds,rsi)


#### Predictions

msft_preds_month_2 <- predict(msft_pro_mod_month_2, msft_future_month_2)

# msft_preds_month %>% arrange(desc(ds)) %>% select(ds,yhat) %>% view

dyplot.prophet(msft_pro_mod_month_2,msft_preds_month_2)

plot(msft_pro_mod_month_2,msft_preds_month_2)




#### Model Evaluation


### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_mon_2_f<- RMSE(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_mon_2 <- RMSE(msft_x_monthly_test$y, 
                msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)



### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_2_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_2 <- mse(msft_x_monthly_test$y, 
               msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)


### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_2_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_2 <- mae(msft_x_monthly_test$y, 
               msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_2_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_2 <- Metrics::mape(msft_x_monthly_test$y, 
                 msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)




################
####### Properly built Month Model with External Regressor

# In the earlier model, actual future values of the RSI were used in the
# predictions. In a practical scenario we do not have access to those.


######
## RSI prediction

rsi_pro_mod <- prophet(holidays = NULL,
                       changepoint.prior.scale = 0.4,
                       seasonality.prior.scale = 5,
                       holidays.prior.scale = 10,
                       seasonality.mode = "multiplicative",
                       changepoint.range = 0.9,
                       daily.seasonality = F)
rsi_pro_mod <- add_country_holidays(rsi_pro_mod, 
                                    country_name = 'US')

rsi_pro_mod <- fit.prophet(rsi_pro_mod,
                           msft_rsi_x)


#### Creating future date dataframe containing future dates to predict

msft_future_rsi <- msft_x[1:sum(length(msft_x$ds)-20),] %>% select(ds)


#### Predictions

rsi_preds_month <- predict(rsi_pro_mod, msft_future_rsi)



### Evaluation

## RMSE

rmse_rsi <- RMSE(msft_rsi_test$y, 
                  rsi_preds_month[sum(length(rsi_preds_month$ds)-19):length(rsi_preds_month$ds),]$yhat)


# dataframe to use:
rsi_preds <- rsi_preds_month %>% select(ds,yhat)





######
## Next Month Prediction model with predicted future RSI values


msft_pro_mod_month_3 <- prophet(holidays = NULL, 
                                changepoint.prior.scale = 0.5,
                                seasonality.prior.scale = 5,
                                holidays.prior.scale = 10,
                                seasonality.mode = "additive",
                                changepoint.range = 0.9,
                                daily.seasonality = F)
msft_pro_mod_month_3 <- add_country_holidays(msft_pro_mod_month_3, 
                                             country_name = 'US')

msft_pro_mod_month_3 <- add_regressor(msft_pro_mod_month_3, "rsi")


msft_pro_mod_month_3 <- fit.prophet(msft_pro_mod_month_3,
                                    msft_x_monthly_train_rsi)


#### Creating future date dataframe containing future dates to predict

msft_future_month_3 <- msft_x[1:sum(length(msft_x$ds)-20),] %>% 
  left_join(rsi_preds) %>% select(ds,yhat) %>% rename(rsi =yhat)


#### Predictions

msft_preds_month_3 <- predict(msft_pro_mod_month_3, msft_future_month_3)

# msft_preds_month %>% arrange(desc(ds)) %>% select(ds,yhat) %>% view

dyplot.prophet(msft_pro_mod_month_3,msft_preds_month_3)

plot(msft_pro_mod_month_3,msft_preds_month_3)


#### Model Evaluation


### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_mon_3_f<- RMSE(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_mon_3 <- RMSE(msft_x_monthly_test$y, 
                   msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)



### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_3_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_3 <- mse(msft_x_monthly_test$y, 
               msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)

### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_3_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_3 <- mae(msft_x_monthly_test$y, 
               msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_3_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_3 <- Metrics::mape(msft_x_monthly_test$y, 
                 msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)



################
####### Adding Both MACD and RSI


### MACD prediction: 
macd_pro_mod <- prophet(holidays = NULL,
                        changepoint.prior.scale = 0.4,
                        seasonality.prior.scale = 5,
                        holidays.prior.scale = 10,
                        seasonality.mode = "multiplicative",
                        changepoint.range = 0.9,
                        daily.seasonality = F)
macd_pro_mod <- add_country_holidays(macd_pro_mod, 
                                     country_name = 'US')

macd_pro_mod <- fit.prophet(macd_pro_mod,
                            macd_x)


#### Creating future date dataframe containing future dates to predict

msft_future_macd <- msft_x[1:sum(length(msft_x$ds)-20),] %>% select(ds)


#### Predictions

macd_preds_month <- predict(macd_pro_mod, msft_future_macd)



### Evaluation

## RMSE

rmse_macd <- RMSE(macd_x_test$y, 
                  macd_preds_month[sum(length(macd_preds_month$ds)-19):length(macd_preds_month$ds),]$yhat)


# Dataframe to use:
macd_preds <- macd_preds_month %>% select(ds,yhat)





######
## Next Month Prediction Model with 2 Regressors

# Model with both, correctly predicted, External Regressors


msft_pro_mod_month_4 <- prophet(holidays = NULL, 
                                changepoint.prior.scale = 0.5,
                                seasonality.prior.scale = 5,
                                holidays.prior.scale = 10,
                                seasonality.mode = "additive",
                                changepoint.range = 0.9,
                                daily.seasonality = F)
msft_pro_mod_month_4 <- add_country_holidays(msft_pro_mod_month_4, 
                                             country_name = 'US')

msft_pro_mod_month_4 <- add_regressor(msft_pro_mod_month_4, "rsi")
msft_pro_mod_month_4 <- add_regressor(msft_pro_mod_month_4, "macd")


msft_pro_mod_month_4 <- fit.prophet(msft_pro_mod_month_4,
                                    msft_x_monthly_train_rm)


#### Creating future date dataframe containing future dates to predict

msft_future_month_4 <- msft_x[1:sum(length(msft_x$ds)-20),] %>% 
  left_join(rsi_preds) %>% select(ds,yhat) %>% rename(rsi =yhat) %>% 
  left_join(macd_preds) %>% select(ds,rsi, yhat) %>% rename(macd =yhat)

#### Predictions

msft_preds_month_4 <- predict(msft_pro_mod_month_4, msft_future_month_4)

dyplot.prophet(msft_pro_mod_month_4,msft_preds_month_4)

plot(msft_pro_mod_month_4,msft_preds_month_3)




#### Model Evaluation


### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_mon_4_f<- RMSE(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_mon_4 <- RMSE(msft_x_monthly_test$y, 
                   msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)

### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_4_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_4 <- mse(msft_x_monthly_test$y, 
                 msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)

### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_4_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_4 <- mae(msft_x_monthly_test$y, 
                 msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_4_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_4 <- Metrics::mape(msft_x_monthly_test$y, 
                   msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)





###### External Regressor Eval

# Next Month Prediction Model results:

xreg_eval <- data_frame(Model = "No Regressor",
                        RMSE = rmse_mon,
                        MSE= mse_mon,
                        MAE= mae_mon,
                        MAPE= mape_mon)
xreg_eval <- full_join(xreg_eval, data_frame(Model = "Known Future RSI",
                       RMSE = rmse_mon_2,
                       MSE= mse_mon_2,
                       MAE= mae_mon_2,
                       MAPE= mape_mon_2))
xreg_eval <- full_join(xreg_eval, data_frame(Model = "Predicted Future RSI",
                       RMSE = rmse_mon_3,
                       MSE= mse_mon_3,
                       MAE= mae_mon_3,
                       MAPE= mape_mon_3))
xreg_eval <- full_join(xreg_eval, data_frame(Model = "Predicted Future RSI and MACD",
                       RMSE = rmse_mon_4,
                       MSE= mse_mon_4,
                       MAE= mae_mon_4,
                       MAPE= mape_mon_4))


# External Regressor Coefficients


regressor_coefficients(msft_pro_mod_month_4) %>% 
  kable() # Show to have very little impact




###############
####### Prophet next year prediction ( 260-day trading period)


#### Initializing prophet model

msft_pro_mod_year <- prophet(holidays = NULL, 
                            changepoint.prior.scale = 0.4,
                            seasonality.prior.scale = 5,
                            holidays.prior.scale = 10,
                            seasonality.mode = "multiplicative",
                            changepoint.range = 0.9,
                            daily.seasonality = F)

msft_pro_mod_year <- add_country_holidays(msft_pro_mod_year, 
                                           country_name = 'US')
msft_pro_mod_year <- fit.prophet(msft_pro_mod_year, msft_x_yearly_train)




#### Creating future date dataframe containing future dates to predict

# Note: make_future_dataframe does not work with holidays; periods to do not
#     include holidays

msft_future_year <- msft_x[1:sum(length(msft_x$ds)-260),] %>% select(ds)




#### Predictions

msft_preds_year <- predict(msft_pro_mod_year, msft_future_year)


## Plot
dyplot.prophet(msft_pro_mod_year,msft_preds_year)


msft_x_yearly_test %>% left_join(by = "ds",msft_preds_year ) %>% 
  select(ds,yhat,y,yhat_upper,yhat_lower) %>%
  pivot_longer(cols =c(yhat,y),names_to = "name", values_to = "value") %>% 
  ggplot(aes(ds,value, color = name)) +
  geom_line()+
  geom_ribbon(aes(ymin = yhat_lower,max= yhat_upper), linetype =2, alpha = 0.1)+
  xlab("Date")+
  ylab("Adjusted Close")+
  ggtitle("Predicted vs Actual Values")+
  scale_color_manual(labels =c("Predictions","Actual"),values =c("blue","red"))+
  labs(color = "Price Values")

#### Model Evaluation

### RMSE

# RMSE of all values (stock prices) in the dataset:

rmse_yr_f <- RMSE(msft_x[1:sum(length(msft_x$ds)-260),]$y ,msft_preds_year$yhat)

# RMSE of the unknown future period we are trying to predict:

rmse_yr <- RMSE(msft_x_yearly_test$y, 
                 msft_preds_year[sum(length(msft_preds_year$ds)-259):length(msft_preds_year$ds),]$yhat)


### MSE

# MSE of all values (stock prices) in the dataset:

mse_yr_f <- mse(msft_x[1:sum(length(msft_x$ds)-260),]$y ,msft_preds_year$yhat)

# MSE of the unknown future period we are trying to predict:

mse_yr <- mse(msft_x_yearly_test$y, 
               msft_preds_year[sum(length(msft_preds_year$ds)-259):length(msft_preds_year$ds),]$yhat)


### MAE

# MAE of all values (stock prices) in the dataset:

mae_yr_f <- mae(msft_x[1:sum(length(msft_x$ds)-260),]$y ,msft_preds_year$yhat)


# MAE of the unknown future period we are trying to predict:

mae_yr <- mae(msft_x_yearly_test$y, 
               msft_preds_year[sum(length(msft_preds_year$ds)-259):length(msft_preds_year$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_yr_f <- Metrics::mape(msft_x[1:sum(length(msft_x$ds)-260),]$y ,msft_preds_year$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_yr <- Metrics::mape(msft_x_yearly_test$y, 
                 msft_preds_year[sum(length(msft_preds_year$ds)-259):length(msft_preds_year$ds),]$yhat)




eval_pro <- full_join(eval_pro, data_frame(Prediction = "Next Year",
                                           RMSE = rmse_yr,
                                           MSE = mse_yr,
                                           MAE = mae_yr,
                                           MAPE = mape_yr))
eval_pro_full <- full_join(eval_pro_full, data_frame(Prediction = "Next Year",
                                                     RMSE = rmse_mon_f,
                                                     MSE = mse_mon_f,
                                                     MAE= mae_mon_f,
                                                     MAPE = mape_mon_f))








###

library(knitr)
library(kableExtra)
kable(eval_pro)


# Evaluation of prediction for entire dataset

eval_pro_full %>% 
  kbl(caption = "Prediction Results for all Data Points") %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(eval_pro$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(3, color = "white", 
              background = spec_color(eval_pro$MSE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(4, color = "white", 
              background = spec_color(eval_pro$MAE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(5, color = "white", 
              background = spec_color(eval_pro$MAE[1:3], end = 0.7,
                                      direction = -1,option="E"))












options(digits = 5)
eval_pro %>% 
  kbl(caption = "Prediction Results for Forecast Horizon") %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(eval_pro$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(3, color = "white", 
              background = spec_color(eval_pro$MSE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(4, color = "white", 
              background = spec_color(eval_pro$MAE[1:3], end = 0.7,
                                      direction = -1,option="E")) %>% 
  column_spec(5, color = "white", 
              background = spec_color(eval_pro$MAE[1:3], end = 0.7,
                                      direction = -1,option="E"))

