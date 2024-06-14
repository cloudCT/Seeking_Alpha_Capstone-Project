##################################
################## ARIMA
###########################



## Install package and load libraries if not done already:

# install.packages("forecast")
library(forecast)

library(tseries)




###########
##### Testing for First Base ARIMA Model



####
## Augmented Dicky-Fuller test

# First, we test for stationarity with the unit root test for stationarity 
# adf (found in tseries package)

adf.test(msft_a$adjusted)

adf.test(msft_monthly_train_a$adjusted)

adf.test(msft_month_train_ret)


# Note: We reject the null-hypothesis since p-value > 0.05




############
###### Model Tuning
## Using auto.arima to estimate p,d and q

am_auto_ret <- auto.arima(msft_month_train_ret)

am_auto_adj <- auto.arima(msft_monthly_train_a$adjusted)
                        


set.seed(123)
arima_model_ret <- auto.arima(msft_month_train_ret, stationary = T, ic = c("aicc", "aic", "bic"), 
                          trace = TRUE)

arima_model_adj <- auto.arima(msft_monthly_train_a$adjusted, stationary = F, ic = c("aicc", "aic", "bic"), 
                          trace = TRUE)



# Both automated models give the same return






#### ACF and PACF

# For returns set (differenced) :

par(mfrow = c(1, 2))
acf(msft_month_train_ret, main = "ACF Returns Dataset")
pacf(msft_month_train_ret, main = "PACF Returns Dataset")

# -> MA process; stationary
## Try AR 15
##     AR 9 
##     MA 9


# For adjusted close set (non differenced) :
par(mfrow = c(1, 2))
acf(msft_monthly_train_a$adjusted)
pacf(msft_monthly_train_a$adjusted)

# -> non-stationary






#### Checking AICc and Residuals
# Checking if residuals from the ARIMA model show that all autocorrelations are
# within the threshold limits, indicating that the residuals are behaving like
# white noise

summary(am_auto_ret)
checkresiduals(am_auto_ret)

summary(am_auto_adj)
checkresiduals(am_auto_adj)


#### Full Auto search
# Checking if full auto search yields better results (based on AiCc)


# Returns set (Differenced):

am_auto_ret_full <- auto.arima(msft_month_train_ret,approximation = F,
                              stepwise = F)

summary(am_auto_ret_full)
checkresiduals(am_auto_ret_full)

summary(am_auto_ret)

# No improvement observed

## Adjusted close set:

am_auto_adj_full <-auto.arima(msft_monthly_train_a$adjusted,approximation = F,
                              stepwise = F)

summary(am_auto_adj_full)
checkresiduals(am_auto_adj_full)

summary(am_auto_adj)

# Full search improves on previous auto model, hence we will treat it as the new
# best performing auto model

am_auto_adj <- am_auto_adj_full




#### Making own estimations based on previous observations

## Returns set (Differenced)

am1_own1 <- Arima(msft_month_train_ret,order = c(15,0,0),include.drift = FALSE)
am1_own2 <- Arima(msft_month_train_ret,order = c(9,0,0),include.drift = FALSE)


am1_own3 <- Arima(msft_month_train_ret,order = c(0,0,9),include.drift = FALSE)
am1_own4 <- Arima(msft_month_train_ret,order = c(0,0,15),include.drift = FALSE)

am1_own5 <- Arima(msft_month_train_ret,order = c(9,0,3),include.drift = FALSE)

summary(am1_own1)
checkresiduals(am1_own1)
summary(am1_own2)
checkresiduals(am1_own2)

summary(am1_own3)
checkresiduals(am1_own3)
summary(am1_own4)
checkresiduals(am1_own4)

summary(am1_own5)
checkresiduals(am1_own5)


# Looking at the summary and the residuals we ascertain that the first two
# models we build ourselves perform the best.
# am1_own1 -> low AICc(2nd); Ljiung-Box p-value over 0.05 indicating white noise ;
#             ACF res values within threshold limits
# am1_own2 -> lowest AICc; Ljiung-Box p-value over 0.05 indicating noise ;
#             ACF res values not within threshold limits (one lag is not)


# Decided on am1_own1 because of threshold limits



## Adjusted close set 

# d = 2 since we assume series has a time-varying trend
# (e.g. a random trend or LES-type model)

am2_own1 <- Arima(msft_monthly_train_a$adjusted,order = c(1,2,1),
                   include.drift = F)


summary(am2_own1)
checkresiduals(am2_own1)

# no improvement; 
# auto-model did a good job with the undifferenced values





#########
#### Predictions


# Automated models
preds_auto_ret <- forecast(am_auto_ret, h = length(msft_month_test_ret))

plot(preds_auto_ret, include = 50)

preds_auto_adj <- forecast(am_auto_adj,h = length(msft_monthly_test_a$adjusted))

plot(preds_auto_adj, include = 50)



# Best performing models based on observations
preds_own_ret <- forecast(am1_own1, h = length(msft_month_test_ret))

plot(preds_own_ret, include = 50)

preds_own_adj <- forecast(am2_own1, h = length(msft_monthly_test_a$adjusted))

plot(preds_own_adj, include = 50)



#########
#### Eval

#### Automated Models:

### Returns set eval:
accuracy(preds_auto_ret, msft_month_test_ret)

rmse(preds_auto_ret$mean,msft_month_test_ret)


## Accuracy of Return transformed back:
options(digits= 15)

p <- exp(diffinv(preds_auto_ret$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)
ret_eval_auto <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


### Adjusted close set eval:

accuracy(preds_auto_adj$mean, msft_monthly_test_a$adjusted)

adj_eval_auto <- accuracy(preds_auto_adj, msft_monthly_test_a$adjusted)[2,2]


#### Self-tuned Models:

### Returns set eval:
accuracy(preds_own_ret, msft_month_test_ret)

rmse(preds_own_ret$mean,msft_month_test_ret)

## Accuracy of Return transformed back:

options(digits= 15)

p <- exp(diffinv(preds_own_ret$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

ret_eval_own <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


### Adjusted close set eval:

accuracy(preds_own_adj, msft_monthly_test_a$adjusted)

adj_eval_own <- accuracy(preds_own_adj, msft_monthly_test_a$adjusted)[2,2]



# RMSE Results
arima_preds_eval <- data_frame(Model = c("Returns Auto",
                                         "Adj Auto",
                                         "Returns Self-Tuned",
                                         "Adj Self-Tuned"), 
                               RMSE = c(ret_eval_auto,
                                        adj_eval_auto,
                                        ret_eval_own,
                                        adj_eval_own))
kbl(arima_preds_eval) %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(arima_preds_eval$RMSE[1:4], end = 0.7,
                                      direction = -1,option="E"))





###### Testing models with exogenous regressors (features)

##### Interest Rate Regressor

#### Model with interest rate regressor
# Returns
am_auto_ret_i1 <- auto.arima(msft_month_train_ret, xreg= fred_int_rate)

# Adjusted
am_auto_adj_i1 <- auto.arima(msft_monthly_train_a$adjusted,approximation = F,
                              stepwise = F, xreg= fred_int_rate_adj)



#### Checking AICc and Residuals
# Checking AICc and comparing to non regressor model

summary(am_auto_ret_i1)
checkresiduals(am_auto_ret_i1)

summary(am_auto_ret)
checkresiduals(am_auto_ret)

summary(am_auto_adj_i1)
checkresiduals(am_auto_adj_i1)

summary(am_auto_adj)
checkresiduals(am_auto_adj)

# Both regressor models beat out original

#### Predictions

# Automated models
preds_auto_ret_i1 <- forecast(am_auto_ret_i1, h = length(msft_month_test_ret),
                           xreg = fred_int_rate_fut)

plot(preds_auto_ret_i1, include = 50)

preds_auto_adj_i1 <- forecast(am_auto_adj_i1,h = length(msft_monthly_test_a$adjusted),
                             xreg = fred_int_rate_fut)

plot(preds_auto_adj_i1, include = 50)






#### Eval

#### Automated Models:

### Returns set eval:

# Original Returns model without regressor
accuracy(preds_auto_ret, msft_month_test_ret)

# Fitted with interest rate regressor

accuracy(preds_auto_ret_i1, msft_month_test_ret)

# - > With regressor better RMSE


## Accuracy of Return transformed back:

# Original, without regressor:
options(digits= 15)

p <- exp(diffinv(preds_auto_ret$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

length(p)

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)


# Fitted with interest rate regresssor
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_i1$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

length(p)

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

intr_eval_auto <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


### Adjusted close set eval:

# Original adjusted close model without regressor
accuracy(preds_auto_adj, msft_monthly_test_a$adjusted)


# Fitted with interest rate regressor

accuracy(preds_auto_adj_i1, msft_monthly_test_a$adjusted)
inta_eval_auto <- accuracy(preds_auto_adj_i1, msft_monthly_test_a$adjusted)[2,2]

#  - > We see clear improvement for the return set model, while seeing a slight
# improvement for the adjusted close set model that was not differenced


#
intr_preds_eval <- data_frame(Model = c("Original Returns Model",
                                        "Int Rate Included"),
                              RMSE = c(ret_eval_auto,
                                       intr_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

inta_preds_eval <- data_frame(Model = c("Original Adjusted Close Model",
                                        "Int Rate Included"),
                              RMSE = c(adj_eval_auto,
                                       inta_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)



##### Interest Rate Regressor (lagged)

# Here, we experiment with the same regressor but lag it this time to see if we
# can improve our model.
# 

#### Model with lagged interest rate regressor
# Returns
am_auto_ret_i2 <- auto.arima(msft_month_train_ret, xreg= fred_int_rate_lag)

am_auto_ret_i3 <- auto.arima(msft_month_train_ret, xreg= fred_int_rate_lag_2)


#### Checking AICc and Residuals
# Checking AICc and comparing to unlagged regressor model

summary(am_auto_ret_i1)
checkresiduals(am_auto_ret_i1)

summary(am_auto_ret_i2)
checkresiduals(am_auto_ret_i2)

summary(am_auto_ret_i3)
checkresiduals(am_auto_ret_i3)

# Lagged regressor model does not perform better

#### Predictions

preds_auto_ret_i2 <- forecast(am_auto_ret_i2, h = length(msft_month_test_ret),
                             xreg = fred_int_rate_fut_lag)

preds_auto_ret_i3 <- forecast(am_auto_ret_i3, h = length(msft_month_test_ret),
                             xreg = fred_int_rate_fut_lag_2)





#### Eval


### Returns set eval:

# Fitted with interest rate regressor

accuracy(preds_auto_ret_i1, msft_month_test_ret)

# Fitted with lagged interest rate regressor

accuracy(preds_auto_ret_i2, msft_month_test_ret)

accuracy(preds_auto_ret_i3, msft_month_test_ret)


## Conclusion:
# Our second transformation for the lagged regressors worked better on the train
# set. Still, the unlagged regressor performed better than both lagged regressors
# on the test set.

# -> We will use unlagged interest rate regressor since it improves our model



# Lagged interest rate regressors transformed back
options(digits= 15)
p <- exp(diffinv(preds_auto_ret_i2$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
lag_int_1_eval<- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]
p <- exp(diffinv(preds_auto_ret_i3$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
lag_int_2_eval<- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


lag_int_eval <- data_frame(Model = c("Unlagged Int Rate",
                                     "Log Diff Int Rate",
                                     "Daily Lag Int Rate"),
                              RMSE = c(intr_eval_auto,
                                       lag_int_1_eval,
                                       lag_int_2_eval)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)






#######
##### Real GDP Regressor
# (Note: We use potential real GDP, as explained in wrangling section)


#### Model with real GDP regressor
# Returns

am_auto_ret_g1 <- auto.arima(msft_month_train_ret, xreg= fred_rea_gdp)


# Adjusted

am_auto_adj_g1 <- auto.arima(msft_monthly_train_a$adjusted,approximation = F,
                            stepwise = F, xreg= fred_rea_gdp_adj)



#### Checking AICc and Residuals
# Checking AICc and comparing to non regressor model

summary(am_auto_ret_g1)
checkresiduals(am_auto_ret_g1)

summary(am_auto_ret)
checkresiduals(am_auto_ret)

summary(am_auto_adj_g1)
checkresiduals(am_auto_adj_g1)

summary(am_auto_adj)
checkresiduals(am_auto_adj)

# Better AICc for adjusted set model;
# Worse AICc for returns set model; 


#### Predictions

# Automated models
preds_auto_ret_g1 <- forecast(am_auto_ret_g1, h = length(msft_month_test_ret),
                             xreg = fred_rea_gdp_fut)

plot(preds_auto_ret_g1, include = 50)

preds_auto_adj_g1 <- forecast(am_auto_adj_g1,h = length(msft_monthly_test_a$adjusted),
                             xreg = fred_rea_gdp_fut)

plot(preds_auto_adj_g1, include = 50)



#### Eval

#### Automated Models:

### Returns set eval:

# Original Returns model without regressor

accuracy(preds_auto_ret, msft_month_test_ret)

# Fitted with GDP regressor:

accuracy(preds_auto_ret_g1, msft_month_test_ret)

# - > With regressor worse RMSE on train but better on test


## Accuracy of Return transformed back:

# Original, without regressor:
options(digits= 15)
p <- exp(diffinv(preds_auto_ret$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)


# Fitted with GDP regresssor
options(digits= 15)
p <- exp(diffinv(preds_auto_ret_g1$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

gdpr_eval_auto <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]

# - > Better rmse with regressor when transformed back


### Adjusted close set eval:

# Original adjusted close model without regressor
accuracy(preds_auto_adj, msft_monthly_test_a$adjusted)

# Fitted with GDP regressor

accuracy(preds_auto_adj_g1, msft_monthly_test_a$adjusted)

gdpa_eval_auto <-accuracy(preds_auto_adj_g1, msft_monthly_test_a$adjusted)[2,2]
# - > Better rmse with regressor



#
gdpr_preds_eval <- data_frame(Model = c("Original Returns Model",
                                        "GDP Included"),
                              RMSE = c(ret_eval_auto,
                                       gdpr_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

gdpa_preds_eval <- data_frame(Model = c("Original Adjusted Close Model",
                                        "GDP Included"),
                              RMSE = c(adj_eval_auto,
                                       gdpa_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)


##### GDP Regressor (lagged)

# Here, we experiment with the same regressor but lag it this time to see if we
# can improve our model.
# 

#### Model with lagged GDP regressor
# Returns
am_auto_ret_g2 <- auto.arima(msft_month_train_ret, xreg= fred_rea_gdp_lag)

am_auto_ret_g3 <- auto.arima(msft_month_train_ret, xreg= fred_rea_gdp_lag_2)


#### Checking AICc and Residuals
# Checking AICc and comparing to unlagged regressor model

summary(am_auto_ret_g1)
checkresiduals(am_auto_ret_g1)

summary(am_auto_ret_g2)
checkresiduals(am_auto_ret_g2)

summary(am_auto_ret_g3)
checkresiduals(am_auto_ret_g3)

# Both lagged worse


#### Predictions

preds_auto_ret_g2 <- forecast(am_auto_ret_g2, h = length(msft_month_test_ret),
                             xreg = fred_rea_gdp_fut_lag)

preds_auto_ret_g3 <- forecast(am_auto_ret_g3, h = length(msft_month_test_ret),
                             xreg = fred_rea_gdp_fut_lag_2)

#### Eval


### Returns set eval:

# Fitted with GDP regressor

accuracy(preds_auto_ret_g1, msft_month_test_ret)

# Fitted with lagged interest rate regressor

accuracy(preds_auto_ret_g2, msft_month_test_ret)

accuracy(preds_auto_ret_g3, msft_month_test_ret)


# Fitted with unlagged GDP regresssor(transformed)
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_g1$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)



# Fitted with lagged GDP regresssor (transformed)
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_g2$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

#
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_g3$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

length(p)

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)


# - > Unlagged regressor gives best results


#

gdp_tr_eval <-accuracy(preds_auto_ret_g1, msft_month_test_ret)[1,2] # train
gdp_ts_eval <- accuracy(preds_auto_ret_g1, msft_month_test_ret)[2,2] # test
lag_gdp_tr_2_eval <- accuracy(preds_auto_ret_g2, msft_month_test_ret)[1,2]
lag_gdp_ts_2_eval <- accuracy(preds_auto_ret_g2, msft_month_test_ret)[2,2]
lag_gdp_tr_3_eval <- accuracy(preds_auto_ret_g3, msft_month_test_ret)[1,2]
lag_gdp_ts_3_eval <- accuracy(preds_auto_ret_g3, msft_month_test_ret)[2,2]

lag_gdp_eval <- data_frame(Model = c("Unlagged GDP",
                                     "Log Diff GDP",
                                     "Daily Lag GDP"),
                           Train = c(gdp_tr_eval,
                                     lag_gdp_tr_2_eval,
                                     lag_gdp_tr_3_eval),
                           Test = c(gdp_ts_eval,
                                    lag_gdp_ts_2_eval,
                                    lag_gdp_ts_3_eval)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)






#######
##### Both exogenous regressors together (interest rate + GDP)

# Returns
am_auto_ret_iag <- auto.arima(msft_month_train_ret,
                              xreg = fred_int_gdp)


# Adjusted
am_auto_adj_iag <- auto.arima(msft_monthly_train_a$adjusted,approximation = F,
                            stepwise = F, 
                            xreg= fred_int_gdp_adj)



#### Checking AICc and Residuals
# Checking AICc and comparing to non regressor model

summary(am_auto_ret_iag)
checkresiduals(am_auto_ret_iag)

summary(am_auto_ret_i1)
checkresiduals(am_auto_ret_i1)

summary(am_auto_ret)
checkresiduals(am_auto_ret)

summary(am_auto_adj_iag)
checkresiduals(am_auto_adj_iag)

summary(am_auto_adj_i1)
checkresiduals(am_auto_adj_i1)

summary(am_auto_adj)
checkresiduals(am_auto_adj)

# 

#### Predictions

# Automated models
preds_auto_ret_iag <- forecast(am_auto_ret_iag, h = length(msft_month_test_ret),
                              xreg = fred_int_gdp_fut)

plot(preds_auto_ret_iag, include = 50)

preds_auto_adj_iag <- forecast(am_auto_adj_iag,h = length(msft_monthly_test_a$adjusted),
                               xreg = fred_int_gdp_fut)

plot(preds_auto_adj_iag, include = 50)



#### Eval

#### Automated Models:

### Returns set eval:

# Original Returns model without regressor
accuracy(preds_auto_ret, msft_month_test_ret)

# Fitted with interest rate regressor

accuracy(preds_auto_ret_i1, msft_month_test_ret)

# Fitted with both regressors

accuracy(preds_auto_ret_iag, msft_month_test_ret)


## Accuracy of Return transformed back:

# Original, without regressor:
options(digits= 15)

p <- exp(diffinv(preds_auto_ret$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)


# Fitted with interest rate regresssor
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_i1$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

# Fitted with both regresssors
options(digits= 15)

p <- exp(diffinv(preds_auto_ret_iag$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

iagr_eval_auto <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


### Adjusted close set eval:

# Original adjusted close model without regressor
accuracy(preds_auto_adj, msft_monthly_test_a$adjusted)

# Fitted with interest rate regressor

accuracy(preds_auto_adj_i1, msft_monthly_test_a$adjusted)

# Fitted with both regressors

accuracy(preds_auto_adj_iag, msft_monthly_test_a$adjusted)

iaga_eval_auto <- accuracy(preds_auto_adj_iag, msft_monthly_test_a$adjusted)[2,2]



# 

xregr_preds_eval <- data_frame(Model = c("Original Returns Model",
                                        "Int Rate Included",
                                        "Int Rate + GDP"),
                              RMSE = c(ret_eval_auto,
                                       intr_eval_auto,
                                       iagr_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)

xrega_preds_eval <- data_frame(Model = c("Original Adjusted Close Model",
                                        "Int Rate Included",
                                        "Int Rate + GDP"),
                              RMSE = c(adj_eval_auto,
                                       inta_eval_auto,
                                       iaga_eval_auto)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F)



############
#### Final model


# With both regressors
am1_mon_fin <- Arima(msft_month_train_ret,order = c(15,0,0),
                     include.drift = FALSE,
                     xreg = fred_int_gdp)

# Only interest rate
am2_mon_fin <- Arima(msft_month_train_ret,order = c(15,0,0),
                     include.drift = FALSE,
                     xreg = fred_int_rate)





## AICc and Residuals
summary(am_auto_ret_iag)
checkresiduals(am_auto_ret_iag)

summary(am_auto_ret_i1)
checkresiduals(am_auto_ret_i1)

summary(am_auto_ret)
checkresiduals(am_auto_ret)


summary(am1_own1)
checkresiduals(am1_own1) # 2nd best aic

summary(am1_mon_fin)
checkresiduals(am1_mon_fin) # 3rd best aic best p

summary(am2_mon_fin)
checkresiduals(am2_mon_fin) # best aic 


# Predictions

preds_mon_fin <- forecast(am1_mon_fin, h = length(msft_month_test_ret),
                               xreg = fred_int_gdp_fut)

preds_mon_fin_2 <- forecast(am2_mon_fin, h = length(msft_month_test_ret),
                          xreg = fred_int_rate_fut)




# Eval
accuracy(preds_mon_fin, msft_month_test_ret) # 2nd best
accuracy(preds_mon_fin_2, msft_month_test_ret) # best
accuracy(preds_own_ret, msft_month_test_ret) # 3rd best

accuracy(preds_auto_ret, msft_month_test_ret)
accuracy(preds_auto_ret_i1, msft_month_test_ret)
accuracy(preds_auto_ret_iag, msft_month_test_ret)



## Accuracy of Return transformed back:

# Final 1
options(digits= 15)

p <- exp(diffinv(preds_mon_fin$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

iag_fin_eval <- accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]
# Final 2

options(digits= 15)

p <- exp(diffinv(preds_mon_fin_2$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

int_fin_eval <-accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)[2]


# Interest rate regressor

options(digits= 15)

p <- exp(diffinv(preds_auto_ret_i1$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)


# Interest rate and GDP

options(digits= 15)

p <- exp(diffinv(preds_auto_ret_iag$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))

length(p)

accuracy(p[2:length(p)],msft_monthly_test_a$adjusted)

#


xreg_fin_preds_eval <- data_frame(Model = c("Self-Estimated Model",
                                         "+ Interest Rate",
                                         "+ Int Rate and GDP",
                                         "Auto Int Rate and GDP"),
                               RMSE = c(ret_eval_own,
                                        int_fin_eval,
                                        iag_fin_eval,
                                        iagr_eval_auto))
kbl(xreg_fin_preds_eval) %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(xreg_fin_preds_eval$RMSE[1:4], end = 0.7,
                                      direction = -1,option="E"))





### Conclusion

# Since our self-tuned model with interest rate as lone exogenous regressor 
# performed best, we will choose it to be our final model.

am_month <- am2_mon_fin

options(digits= 15)
p <- exp(diffinv(preds_mon_fin_2$mean/100, 
                 xi = log(msft_monthly_train_a$adjusted[2510])))
preds_month_a <- p[2:length(p)]

preds_month_plot <- msft_monthly_test_a %>% cbind(preds_month_a) %>% 
  rename(preds = "preds_month_a") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()





############################
#############   MSFT Next Day Prediction with Arima

# We will fit our best performing model to predict only the next trading day



### Model
# Using differenced adjusted close values (returns)
 
am_day1 <- Arima(msft_day_train_ret,order = c(15,0,0),
                     include.drift = FALSE,
                     xreg = fred_int_rate_day)

# Without external regressor
am_day2 <- Arima(msft_day_train_ret,order = c(15,0,0),
                 include.drift = FALSE)


## Checking AICc and Residuals

summary(am_day1)
checkresiduals(am_day1)

summary(am_day2)
checkresiduals(am_day2)

# - > p-value over 0.05; ACF inside thresholds


### Predictions

preds_day_a <- forecast(am_day1, h = length(msft_day_test_ret),
                            xreg = fred_int_rate_fut_day)

preds_day_a2 <- forecast(am_day2, h = length(msft_day_test_ret))




### Eval

# On returns set
accuracy(preds_day_a , msft_day_test_ret)

accuracy(preds_day_a2 , msft_day_test_ret)


# Transformed returns to adjusted close

options(digits= 15)

p <- exp(diffinv(preds_day_a$mean/100, 
                 xi = log(msft_daily_train_a$adjusted[length(msft_daily_train_a$adjusted)])))


accuracy(p[2:length(p)],msft_daily_test_a$adjusted)

int_day_eval <-accuracy(p[2:length(p)],msft_daily_test_a$adjusted)[2]

# Without regressor:
options(digits= 15)

p <- exp(diffinv(preds_day_a2$mean/100, 
                 xi = log(msft_daily_train_a$adjusted[length(msft_daily_train_a$adjusted)])))

accuracy(p[2:length(p)],msft_daily_test_a$adjusted)

day_eval <- accuracy(p[2:length(p)],msft_daily_test_a$adjusted)[2]

### Conclusion:

# -> Arima model does not predict next day values well. Model without regressor
# performed slightly better but still not satisfactory. Would have predicted the
# price to rise, when in actuality it fell. 


day_preds_eval <- data_frame(Model = c("Next-Day Prediction with Int Rate",
                                       "Next-Day Prediction without Int Rate"),
                                  RMSE = c(int_day_eval,
                                           day_eval)) %>% 
  kbl() %>% 
  kable_material_dark(full_width = F) 



### Final Next-Day Prediction Model:

am_day <- am_day2

options(digits= 15)
p <- exp(diffinv(preds_day_a2$mean/100, 
                 xi = log(msft_daily_train_a$adjusted[length(msft_daily_train_a$adjusted)])))

preds_day_a <- p[2:length(p)]

  
############################
#############   MSFT Next Year Prediction with Arima

# We will fit our best performing model to predict the next trading year



### Model
# Using differenced adjusted close values (returns)

am_year1 <- Arima(msft_year_train_ret,order = c(15,0,0),
                 include.drift = FALSE,
                 xreg = fred_int_rate_year)

# Without external regressor
am_year2 <- Arima(msft_year_train_ret,order = c(15,0,0),
                 include.drift = FALSE)


# With both regressors(gdp + interest rate)

am_year3 <- Arima(msft_year_train_ret,order = c(15,0,0),
                  include.drift = FALSE,
                  xreg = fred_int_gdp_year)


# Both regressors and adjusted close set
am_year4 <- Arima(msft_yearly_train_a$adjusted,order = c(15,1,0),
                  include.drift=T,
                  xreg = fred_int_gdp_year_adj )


## Checking AICc and Residuals

summary(am_year1)
checkresiduals(am_year1)

summary(am_year2)
checkresiduals(am_year2)

summary(am_year3)
checkresiduals(am_year3)

summary(am_year4)
checkresiduals(am_year4)

### Predictions

a_preds_year1 <- forecast(am_year1, h = length(msft_year_test_ret),
                        xreg = fred_int_rate_fut_year)

a_preds_year2 <- forecast(am_year2, h = length(msft_year_test_ret))

a_preds_year3 <- forecast(am_year3, h = length(msft_year_test_ret),
                          xreg = fred_int_gdp_fut_year)

a_preds_year4 <- forecast(am_year4, h = length(msft_yearly_test_a$adjusted),
                          xreg = fred_int_gdp_fut_year)


### Eval

# On returns set
accuracy(a_preds_year1 , msft_year_test_ret)

accuracy(a_preds_year2 , msft_year_test_ret)

accuracy(a_preds_year3 , msft_year_test_ret)


## Transformed returns to adjusted close

# With int rate regressor:
options(digits= 15)

p <- exp(diffinv(a_preds_year1$mean/100, 
                 xi = log(msft_yearly_train_a$adjusted[length(msft_yearly_train_a$adjusted)])))
p_year_1 <- p[2:length(p)]
int_year_eval <- accuracy(p_year_1,msft_yearly_test_a$adjusted)[2]


# Without regressor:
options(digits= 15)
p <- exp(diffinv(a_preds_year2$mean/100, 
                 xi = log(msft_yearly_train_a$adjusted[length(msft_yearly_train_a$adjusted)])))

p_year_2 <- p[2:length(p)]

ret_year_eval <- accuracy(p_year_2,msft_yearly_test_a$adjusted)[2]

p_year_2_plot <- msft_yearly_test_a %>% cbind(p_year_2) %>% 
  rename(preds = "p_year_2") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()

# With both regressors:

options(digits= 15)
p <- exp(diffinv(a_preds_year3$mean/100, 
                 xi = log(msft_yearly_train_a$adjusted[length(msft_yearly_train_a$adjusted)])))
p_year_3 <- p[2:length(p)]
iag_year_eval <- accuracy(p_year_3,msft_yearly_test_a$adjusted)[2]



# Adjusted close set

adj_iag_year_eval <- accuracy(a_preds_year4 ,msft_yearly_test_a$adjusted)[2,2]

p_year_4_plot <- msft_yearly_test_a %>% cbind(a_preds_year4$mean) %>% 
  rename(preds = "a_preds_year4$mean") %>% 
  pivot_longer(cols = c(adjusted,preds),values_to = "price", names_to = "type") %>% 
  ggplot(aes(x = date, y = price, color = type))+
  geom_line()



year_preds_eval <- data_frame(Model = c("Returns set with Int Rate",
                                       "Returns set without Exogenous Regressor",
                                       "Returns set with GDP and Int Rate",
                                       "Adj Close set with GDP and Int Rate"),
                             RMSE = c(int_year_eval,
                                      ret_year_eval,
                                      iag_year_eval,
                                      adj_iag_year_eval)) 
kbl(year_preds_eval) %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(year_preds_eval$RMSE[1:4], end = 0.7,
                                      direction = -1,option="E"))



### Conclusion:


# We are getting very surprising results. The Adjusted close set performs a lot
# better when we judge by rmse. It also achieves better results when including
# both regressors (interest rate and gdp).
# On the other hand, the returns set performs best without any regressors.

# It is important to note that the adjusted close set (model 4) performed terribly
# in our initial model analysis and would not have gotten accepted if we stayed
# true to our thresholds. Its p- value is below 0.05 and multiple values jump over
# our thresholds.

# This could be a case of overfitting the model.

# If we choose to prefer a model that is less likely to have been overfit, is 
# more interpretable and better balances complexity and accuracy we should choose
# the model with the better AICc(model 2).
# If we purely value accuracy model 4 would be best suited.

# Looking at the plots of the model with the lowest AICc (model 2), and the
# plot with the lowest RMSE (model 4), we see that Arima does not perform well
# when predicting far into the future.



### Final Next-Year Prediction Model:

am_year <- am_year2 
preds_year_a <- p_year_2






###########

am_day
am_month
am_year

preds_day_a
preds_month_a
preds_year_a

# Make sure to use forecast package for accuracy
eval_day_a <- accuracy(preds_day_a,msft_daily_test_a$adjusted)
eval_month_a <- accuracy(preds_month_a,msft_monthly_test_a$adjusted)
eval_year_a <- accuracy(preds_year_a,msft_yearly_test_a$adjusted)

eval_arima <- data_frame(Horizon = "Next Day",
                         RMSE =eval_day_a[2],
                         MAE =eval_day_a[3],
                         MAPE =eval_day_a[4]) %>%
  full_join(data_frame(Horizon = "Next Month",
                       RMSE =eval_month_a[2],
                       MAE =eval_month_a[3],
                       MAPE =eval_month_a[4])) %>%
              full_join(data_frame(Horizon = "Next Year",
                                   RMSE =eval_year_a[2],
                                   MAE =eval_year_a[3],
                                   MAPE =eval_year_a[4])) 
              
kbl(eval_arima) %>% 
  kable_material_dark(full_width = F) %>% 
  column_spec(2, color = "white", 
              background = spec_color(eval_arima$RMSE[1:3], end = 0.7,
                                      direction = -1,option="E"))
  

