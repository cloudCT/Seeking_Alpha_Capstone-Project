##################################
################## Data Wrangling
###########################




###########
##### Wrangling data for first prophet model
# We will only use adjusted price and change the column names to "ds" for date,
# and "y" for the adjusted price.


msft_x <- msft %>% select(date,adjusted) %>% rename(ds = date, y = adjusted)

msft_x




############ msft train and test split

# We will split the data twice. Once for comparing all the models to one another
# and tuning the model, and once for final testing.
# We will split the data for the first train/test split by removing the last 
# two periods and putting the older one of these two into a test set. (2 days,
# 2 months, 2 years).
# For the second train/test split, we will remove only the last period and put it
# into the test set.




###### First train/test split "daily" (next day prediction)


msft_x

msft_x_daily_train <- msft_x[1:sum(length(msft_x$ds)-2),]


msft_x_daily_test <- msft_x[length(msft_x$ds)-1,]



###### Second train/test split "daily" (next trading day prediction)

msft_x_daily_final_train <- msft_x[1:sum(length(msft_x$ds)-1),]
  

msft_x_daily_final_test <- msft_x[length(msft_x$ds),]
  
  
  
  

###### First train/test split "monthly" (next month prediction; 20-trading days 
#                                                                        period)


msft_x %>% arrange(desc(ds)) %>% view()

msft_x_monthly_train <- msft_x[1:sum(length(msft_x$ds)-40),]


msft_x_monthly_test <- msft_x[sum(length(msft_x$ds)-39):sum(length(msft_x$ds)-20),]

# msft_x_monthly_test %>% arrange(desc(ds)) %>% view
# msft_x_monthly_train%>% arrange(desc(ds))






###### Second train/test split "monthly" (next month prediction)

msft_x_monthly_final_train <- msft_x[1:sum(length(msft_x$ds)-20),]


msft_x_monthly_final_test <- msft_x[sum(length(msft_x$ds)-19):length(msft_x$ds),]




###### First train/test split "yearly" (next year prediction; 260-trading days 
#                                                                        period)




msft_x_yearly_train <- msft_x[1:sum(length(msft_x$ds)-520),]


msft_x_yearly_test <- msft_x[sum(length(msft_x$ds)-519):sum(length(msft_x$ds)-260),]



###### Second train/test split "yearly" (next year prediction)

msft_x_yearly_final_train <- msft_x[1:sum(length(msft_x$ds)-260),]


msft_x_yearly_final_test <- msft_x[sum(length(msft_x$ds)-259):length(msft_x$ds),]



###### Wrangling for External Regressors
# Analyzed and evaluated on next month prediction model

msft_x_monthly_train_rsi <- msft_x_monthly_train %>% left_join(msft_rsi_av) 



msft_rsi_x <-  msft_x_monthly_train %>% left_join(msft_rsi_av) %>% 
  select(ds,rsi) %>% 
  rename(y = rsi)

msft_rsi_test <- msft_x_monthly_test %>% left_join(msft_rsi_av) %>% 
  select(ds,rsi) %>% 
  rename(y = rsi)


#
msft_macd_x <- msft_macd %>% rename(ds =date)

msft_x_monthly_train_macd <- msft_x_monthly_train %>% left_join(msft_macd_x) 


macd_x <-  msft_x_monthly_train %>% left_join(msft_macd_x) %>% 
  select(ds,macd) %>% 
  rename(y = macd)

msft_macd_test <- msft_x_monthly_test %>% left_join(msft_macd_x) %>% 
  select(ds,macd) %>% 
  rename(y = macd)


msft_x_monthly_train_rm <-msft_x_monthly_train %>% left_join(msft_rsi_av) %>% 
  left_join(msft_macd_x)



#################
######## Wrangling for ARIMA Model


msft_a <- msft %>% select(date,adjusted) %>% mutate(date = ymd(date))




####### Test and Train set
###


####### Month
###### First train/test split "monthly" (next month prediction; 20-trading days 
#                                                                        period)


# msft_a %>% arrange(desc(date)) %>% view()

msft_monthly_train_a <- msft_a[1:sum(length(msft_a$date)-40),]


msft_monthly_test_a <- msft_a[sum(length(msft_a$date)-39):sum(length(msft_a$date)-20),]

# msft_monthly_test_a %>% arrange(desc(date)) %>% view
# msft_monthly_train_a  %>% arrange(desc(date))


# Returns; to difference data

msft_month_train_ret <-  100*diff(log(msft_monthly_train_a$adjusted))

msft_month_test_ret <- c(msft_monthly_train_a$adjusted[2510],msft_monthly_test_a$adjusted)
msft_month_test_ret <-  100*diff(log(msft_month_test_ret))




### Revert to normal price:

options(digits= 15)

exp(diffinv(msft_month_train_ret/100, xi = log(msft_monthly_train_a$adjusted[1])))

exp(diffinv(msft_month_test_ret/100, xi = log(msft_monthly_train_a$adjusted[2510])))



####### Day
###### First train/test split "daily" (next day prediction; 1-trading day)

# msft_a %>% arrange(desc(date)) %>% view()


msft_daily_train_a <- msft_a[1:sum(length(msft_a$date)-2),]

msft_daily_test_a <- msft_a[sum(length(msft_a$date)-1),]

# Returns; to difference data

msft_day_train_ret <-  100*diff(log(msft_daily_train_a$adjusted))

# length(msft_daily_train_a$adjusted)
msft_day_test_ret <- c(msft_daily_train_a$adjusted[length(msft_daily_train_a$adjusted)],
                       msft_daily_test_a$adjusted)
msft_day_test_ret <-  100*diff(log(msft_day_test_ret))


####### Year
###### First train/test split "yearly" (next year prediction; 260-trading days)

msft_a %>% arrange(desc(date)) %>% view()


msft_yearly_train_a <- msft_a[1:sum(length(msft_a$date)-520),]

msft_yearly_test_a <- msft_a[sum(length(msft_a$date)-519):sum(length(msft_a$date)-260),]

# Returns; to difference data

msft_year_train_ret <-  100*diff(log(msft_yearly_train_a$adjusted))


msft_year_test_ret <- c(msft_yearly_train_a$adjusted[length(msft_yearly_train_a$adjusted)],
                       msft_yearly_test_a$adjusted)
msft_year_test_ret <-  100*diff(log(msft_year_test_ret))


####### Final Validation train/test splits
##

####### Month
###### Second train/test split "monthly" (next month prediction; 20-trading days 
#       
msft_monthly_final_train_a <- msft_a[1:sum(length(msft_a$date)-20),]
msft_monthly_final_test_a <- msft_a[sum(length(msft_a$date)-19):length(msft_a$date),]

# Returns; to difference data
msft_month_final_train_ret <-  100*diff(log(msft_monthly_final_train_a$adjusted))
msft_month_final_test_ret <- c(msft_monthly_final_train_a$adjusted[length(msft_monthly_final_train_a$adjusted)],
                               msft_monthly_final_test_a$adjusted)
msft_month_final_test_ret <-  100*diff(log(msft_month_final_test_ret))

####### Day
###### Second train/test split "daily" (next day prediction; 1-trading day)
#
msft_daily_final_train_a <- msft_a[1:sum(length(msft_a$date)-1),]
msft_daily_final_test_a <- msft_a[length(msft_a$date),]

# Returns; to difference data
msft_day_final_train_ret <-  100*diff(log(msft_daily_final_train_a$adjusted))
msft_day_final_test_ret <- c(msft_daily_final_train_a$adjusted[length(msft_daily_final_train_a$adjusted)],
                       msft_daily_test_a$adjusted)
msft_day_final_test_ret <-  100*diff(log(msft_day_final_test_ret))

####### Year
###### First train/test split "yearly" (next year prediction; 260-trading days)
#
msft_yearly_final_train_a <- msft_a[1:sum(length(msft_a$date)-260),]
msft_yearly_final_test_a <- msft_a[sum(length(msft_a$date)-259):length(msft_a$date),]

# Returns; to difference data
msft_year_final_train_ret <-  100*diff(log(msft_yearly_final_train_a$adjusted))
msft_year_final_test_ret <- c(msft_yearly_final_train_a$adjusted[length(msft_yearly_final_train_a$adjusted)],
                        msft_yearly_final_test_a$adjusted)
msft_year_final_test_ret <-  100*diff(log(msft_year_final_test_ret))



######### Extra feature wrangling for Arima (exogenous regressors)


### Interest Rate 

# Future projected via quantmod/tidyquant (not needed for month/day set)
fed_proj_rates <- tq_get("FEDTARMD", get = "economic.data",from = "2023-01-01", to  = "2025-12-31")

# Download via Alpha Vantage
fred_int_rate_av <- av_get(av_fun     = "FEDERAL_FUNDS_RATE",
                       outputsize = "full") %>% rename(date = timestamp)


## Wrangling; fitting to test and train set
fred_int_rate <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                       max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  .[2:length(.$date),] %>% select(int_rate) %>% as.matrix()


fred_int_rate_fut <- data.frame(date = seq(min(msft_monthly_test_a$date), 
                                           max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_monthly_test_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% as.matrix()


# For adjusted (undifferenced) set
fred_int_rate_adj <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                       max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(int_rate = value) %>%
  select(int_rate) %>% as.matrix()



## Lagged interest rate

fred_int_rate_lag <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                           max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% summarize(int_rate = 100*diff(log(int_rate))) %>% 
  as.matrix()

fred_int_rate_fut_lag <- data.frame(date = seq(min(msft_monthly_train_a$date[2510]), 
                                           max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% summarize(int_rate = 100*diff(log(int_rate))) %>% 
  as.matrix()

fred_int_rate_lag_2 <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                           max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% summarize(int_rate = diff(int_rate)) %>% 
  as.matrix()

fred_int_rate_fut_lag_2 <- data.frame(date = seq(min(msft_monthly_train_a$date[2510]), 
                                               max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% summarize(int_rate = diff(int_rate)) %>% 
  as.matrix()







### GDP
# Checked multiple sources for real us gdp.
# Projected GDP could only be found as growth percentage, while real growth
# numbers could not be found.
# Ultimately decided on choosing potential real us gdp since it includes 
# projections. Downloaded via tidyquant from FRED.


# Download via Alpha Vantage
fred_rea_gdp_av <- av_get(av_fun     = "REAL_GDP",
                       outputsize = "full")

# fred_rea_gdp_av  %>% arrange(desc(timestamp))



# Tidyquant
fred_rea_pot_gdp_tq <- tq_get("GDPPOT", get = "economic.data",
                           from = "2014-01-01", to  = "2025-12-31")



# view(fred_rea_pot_gdp)

## Wrangling; fitting to test and train set
# Will omit "potential" notation from this point forward

fred_rea_gdp <- data.frame(date = seq(ymd("2014-01-01"), 
                                       max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq ) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(gdp = price) %>% 
  .[2:length(.$date),] %>% select(gdp) %>% as.matrix()


fred_rea_gdp_fut <- data.frame(date = seq(ymd("2023-10-01"), 
                                           max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_monthly_test_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% as.matrix()


# For adjusted (undifferenced) set
fred_rea_gdp_adj <- data.frame(date = seq(ymd("2014-01-01"), 
                                           max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(gdp = price) %>%
  select(gdp) %>% as.matrix()



## Lagged real GDP

fred_rea_gdp_lag <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                           max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% summarize(gdp = 100*diff(log(gdp))) %>% 
  as.matrix()

fred_rea_gdp_fut_lag <- data.frame(date = seq(min(msft_monthly_train_a$date[2510]), 
                                               max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% summarize(gdp = 100*diff(log(gdp))) %>% 
  as.matrix()

fred_rea_gdp_lag_2 <- data.frame(date = seq(min(msft_monthly_train_a$date), 
                                             max(msft_monthly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_monthly_train_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% summarize(gdp = diff(gdp)) %>% 
  as.matrix()

fred_rea_gdp_fut_lag_2 <- data.frame(date = seq(min(msft_monthly_train_a$date[2510]), 
                                                 max(msft_monthly_test_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% summarize(gdp = diff(gdp)) %>% 
  as.matrix()


### GDP + Interest rate matrix

fred_int_gdp <- cbind(fred_int_rate,fred_rea_gdp)

fred_int_gdp_fut <- cbind(fred_int_rate_fut,fred_rea_gdp_fut)

fred_int_gdp_adj <- cbind(fred_int_rate_adj,fred_rea_gdp_adj)




#### Interest Rate Regressor for Next-Day Prediction


## Wrangling; fitting to test and train set
fred_int_rate_day <- data.frame(date = seq(min(msft_daily_train_a$date), 
                                       max(msft_daily_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_daily_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  .[2:length(.$date),] %>% select(int_rate) %>% as.matrix()


fred_int_rate_fut_day <- data.frame(date = seq(min(msft_daily_train_a$date), 
                                           max(msft_daily_test_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_daily_test_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% as.matrix()


#### Interest Rate Regressor for Next-Year Prediction


## Wrangling; fitting to test and train set
fred_int_rate_year <- data.frame(date = seq(min(msft_yearly_train_a$date), 
                                           max(msft_yearly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_yearly_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  .[2:length(.$date),] %>% select(int_rate) %>% as.matrix()


fred_int_rate_fut_year <- data.frame(date = seq(min(msft_yearly_train_a$date), 
                                               max(msft_yearly_test_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_yearly_test_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% as.matrix()


fred_int_rate_year_adj <- data.frame(date = seq(min(msft_yearly_train_a$date), 
                                                max(msft_yearly_train_a$date), by = "days")) %>%
  left_join(fred_int_rate_av) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_yearly_train_a) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% as.matrix()


### GDP

fred_rea_gdp_year <- data.frame(date = seq(ymd("2014-01-01"), 
                                      max(msft_yearly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq ) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_yearly_train_a) %>% tibble() %>% rename(gdp = price) %>% 
  .[2:length(.$date),] %>% select(gdp) %>% as.matrix()


fred_rea_gdp_fut_year <- data.frame(date = seq(ymd("2022-01-01"), 
                                          max(msft_yearly_test_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_yearly_test_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% as.matrix()


fred_rea_gdp_year_adj <- data.frame(date = seq(ymd("2014-01-01"), 
                                               max(msft_yearly_train_a$date), by = "days")) %>%
  left_join(fred_rea_pot_gdp_tq ) %>%
  fill(price, .direction = "downup") %>% 
  inner_join(msft_yearly_train_a) %>% tibble() %>% rename(gdp = price) %>% 
  select(gdp) %>% as.matrix()



### GDP + Interest rate matrix

fred_int_gdp_year <- cbind(fred_int_rate_year,fred_rea_gdp_year)

fred_int_gdp_fut_year <- cbind(fred_int_rate_fut_year,fred_rea_gdp_fut_year)

fred_int_gdp_year_adj <- cbind(fred_int_rate_year_adj,fred_rea_gdp_year_adj)




########################
############ Wrangling for LSTM Model
####################


msft_l <- msft %>% select(date,adjusted) %>% mutate(date = ymd(date))



### Creating Test and Train Split


msft_monthly_train_l <- msft_l[1:sum(length(msft_l$date)-40),]

msft_monthly_test_l <- msft_l[sum(length(msft_l$date)-39):sum(length(msft_l$date)-20),]



msft_month_train_ret_l <-  100*diff(log(msft_monthly_train_l$adjusted))

msft_month_test_ret_l <- c(msft_monthly_train_l$adjusted[length(msft_monthly_train_l$adjusted)],
                         msft_monthly_test_l$adjusted)
msft_month_test_ret_l <-  100*diff(log(msft_month_test_ret_l))


msft_l_ret <- 100*diff(log(msft_l$adjusted)) 


msft_l_ret_sub <- msft_l_ret[1:sum(length(msft_l_ret)-20)]


# Scaling for LSTM Model; we will use a standard scaler instead of a min/max 
# scaler since a min/max scaler makes little practical sense in our context



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

# Scaling returns subset:
sf_msft_l_ret <- scaling_factors(msft_l_ret_sub)

msft_l_ret_sub <- standard_scale(msft_l_ret_sub, sf_msft_l_ret)




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




steps_in <- 40
steps_out <- 20

x_train <- kera_transform(msft_l_ret_sub, x = T,
                          steps_in = steps_in, 
                          steps_out = steps_out)

y_train <- kera_transform(msft_l_ret_sub, x = F,
                          steps_in = steps_in, 
                          steps_out = steps_out)

x_test <- kera_pred_transform(msft_l_ret_sub,
                              steps_in = steps_in, 
                              steps_out = steps_out)

x_train
y_train

x_test


dim(x_test)
dim(x_train)
dim(y_train)






############## Features LSTM


### Interest Rate

fred_int_rate_l <- data.frame(date = seq(min(msft_l$date), 
                                       max(msft_l$date), by = "days")) %>%
  left_join(fred_int_rat) %>%
  fill(value, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% rename(int_rate = value) %>% 
  select(int_rate) %>% as.matrix()



#int_rate_l <- diff(fred_int_rate_l)
int_rate_l <- fred_int_rate_l[2:length(fred_int_rate_l),]

int_rate_l_sub <- int_rate_l[1:sum(length(int_rate_l)-20)]


# Scaling
sf_int_rate_l <- scaling_factors(int_rate_l_sub)

int_rate_l_sub <- standard_scale(int_rate_l_sub, sf_int_rate_l)


# Transform into proper shape
#steps_in <- 40
#steps_out <- 20

kera_int_x_train <- kera_transform(int_rate_l_sub, x = T,
                          steps_in = steps_in, 
                          steps_out = steps_out)


kera_int_x_test <- kera_pred_transform(int_rate_l_sub,
                              steps_in = steps_in, 
                              steps_out = steps_out)


dim(kera_int_x_train)
dim(x_test)

x_train_2 <- array(c(x_train,kera_int_x_train), dim= c(nrow(x_train), steps_in, 2))

x_test_2 <- array(c(x_test,kera_int_x_test), dim= c(nrow(x_test), steps_in, 2))





### Volume

msft_vol


msft_vol_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_vol) %>%
  fill(volume, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  select(volume) %>% as.matrix()


msft_vol_l <- diff(msft_vol_l)

msft_vol_l_sub <- msft_vol_l[1:sum(length(msft_vol_l)-20)]


# Scaling
sf_msft_vol_l <- scaling_factors(msft_vol_l_sub)

msft_vol_l_sub <- standard_scale(msft_vol_l_sub, sf_msft_vol_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_vol_x_train <- kera_transform(msft_vol_l_sub, x = T,
                                   steps_in = steps_in, 
                                   steps_out = steps_out)


kera_msft_vol_x_test <- kera_pred_transform(msft_vol_l_sub,
                                       steps_in = steps_in, 
                                       steps_out = steps_out)



n_feat <- 3
x_train_3 <- array(c(x_train_2,kera_msft_vol_x_train), 
                   dim= c(nrow(x_train_2), steps_in, n_feat))

x_test_3 <- array(c(x_test_2,kera_msft_vol_x_test), 
                  dim= c(nrow(x_test_2), steps_in, n_feat))





####### Technical Indicators Features


### RSI

msft_rsi

msft_rsi_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_rsi) %>%
  fill(rsi, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(rsi) %>% as.matrix()



msft_rsi_l_sub <- msft_rsi_l[1:sum(length(msft_rsi_l)-20)]


# Scaling
sf_msft_rsi_l <- scaling_factors(msft_rsi_l_sub)

msft_rsi_l_sub <- standard_scale(msft_rsi_l_sub, sf_msft_rsi_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_rsi_x_train <- kera_transform(msft_rsi_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)


kera_msft_rsi_x_test <- kera_pred_transform(msft_rsi_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)


### VWAP

msft_vwap


msft_vwap_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_vwap) %>%
  fill(VWAP, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% rename(vwap = VWAP) %>% 
  .[2:length(.$date),] %>% select(vwap) %>% as.matrix()



msft_vwap_l_sub <- msft_vwap_l[1:sum(length(msft_vwap_l)-20)]


# Scaling
sf_msft_vwap_l <- scaling_factors(msft_vwap_l_sub)

msft_vwap_l_sub <- standard_scale(msft_vwap_l_sub, sf_msft_vwap_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_vwap_x_train <- kera_transform(msft_vwap_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)


kera_msft_vwap_x_test <- kera_pred_transform(msft_vwap_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)


### MACD

msft_macd


msft_macd_l <- data.frame(date = seq(min(msft_l$date), 
                                     max(msft_l$date), by = "days")) %>%
  left_join(msft_macd) %>%
  fill(macd, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(macd) %>% as.matrix()



msft_macd_l_sub <- msft_macd_l[1:sum(length(msft_macd_l)-20)]


# Scaling
sf_msft_macd_l <- scaling_factors(msft_macd_l_sub)

msft_macd_l_sub <- standard_scale(msft_macd_l_sub, sf_msft_macd_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_macd_x_train <- kera_transform(msft_macd_l_sub, x = T,
                                         steps_in = steps_in, 
                                         steps_out = steps_out)


kera_msft_macd_x_test <- kera_pred_transform(msft_macd_l_sub,
                                             steps_in = steps_in, 
                                             steps_out = steps_out)


### ATR

msft_atr


msft_atr_l <- data.frame(date = seq(min(msft_l$date), 
                                     max(msft_l$date), by = "days")) %>%
  left_join(msft_atr) %>%
  fill(atr, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(atr) %>% as.matrix()



msft_atr_l_sub <- msft_atr_l[1:sum(length(msft_atr_l)-20)]


# Scaling
sf_msft_atr_l <- scaling_factors(msft_atr_l_sub)

msft_atr_l_sub <- standard_scale(msft_atr_l_sub, sf_msft_atr_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_atr_x_train <- kera_transform(msft_atr_l_sub, x = T,
                                         steps_in = steps_in, 
                                         steps_out = steps_out)


kera_msft_atr_x_test <- kera_pred_transform(msft_atr_l_sub,
                                             steps_in = steps_in, 
                                             steps_out = steps_out)



### CMF

msft_cmf 

msft_cmf_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_cmf) %>%
  fill(cmf, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(cmf) %>% as.matrix()



msft_cmf_l_sub <- msft_cmf_l[1:sum(length(msft_cmf_l)-20)]


# Scaling
sf_msft_cmf_l <- scaling_factors(msft_cmf_l_sub)

msft_cmf_l_sub <- standard_scale(msft_cmf_l_sub, sf_msft_cmf_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_cmf_x_train <- kera_transform(msft_cmf_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)


kera_msft_cmf_x_test <- kera_pred_transform(msft_cmf_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)



### MFI

msft_mfi



msft_mfi_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_mfi) %>%
  fill(mfi, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(mfi) %>% as.matrix()



msft_mfi_l_sub <- msft_mfi_l[1:sum(length(msft_mfi_l)-20)]


# Scaling
sf_msft_mfi_l <- scaling_factors(msft_mfi_l_sub)

msft_mfi_l_sub <- standard_scale(msft_mfi_l_sub, sf_msft_mfi_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_mfi_x_train <- kera_transform(msft_mfi_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)


kera_msft_mfi_x_test <- kera_pred_transform(msft_mfi_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)


### OBV

msft_obv

msft_obv_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_obv) %>%
  fill(obv, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(obv) %>% as.matrix()



msft_obv_l_sub <- msft_obv_l[1:sum(length(msft_obv_l)-20)]


# Scaling
sf_msft_obv_l <- scaling_factors(msft_obv_l_sub)

msft_obv_l_sub <- standard_scale(msft_obv_l_sub, sf_msft_obv_l)



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_obv_x_train <- kera_transform(msft_obv_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)


kera_msft_obv_x_test <- kera_pred_transform(msft_obv_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)


### Donchian Channels

msft_dc

msft_dc_l <- data.frame(date = seq(min(msft_l$date), 
                                    max(msft_l$date), by = "days")) %>%
  left_join(msft_dc) %>%
  fill(-date, .direction = "downup") %>% 
  inner_join(msft_l) %>% tibble() %>% 
  .[2:length(.$date),] %>% select(dc_high,dc_mid,dc_low) %>% as.matrix()


msft_dc_l_sub <- msft_dc_l[1:sum(length(msft_dc_l[,1])-20),]




# Scaling
sf_msft_dc_l <- scaling_factors(msft_dc_l_sub)

msft_dc_l_sub <- standard_scale(msft_dc_l_sub, sf_msft_dc_l)

# split into separate matrices so we do not have to rewrite kerasize function
msft_dch_l_sub <- msft_dc_l_sub[,1] %>% as.matrix()
msft_dcm_l_sub <- msft_dc_l_sub[,2] %>% as.matrix()
msft_dcl_l_sub <- msft_dc_l_sub[,3] %>% as.matrix()



# Transform into proper shape

#steps_in <- 40
#steps_out <- 20

kera_msft_dch_x_train <- kera_transform(msft_dch_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)

kera_msft_dcm_x_train <- kera_transform(msft_dcm_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)

kera_msft_dcl_x_train <- kera_transform(msft_dcl_l_sub, x = T,
                                        steps_in = steps_in, 
                                        steps_out = steps_out)

kera_msft_dch_x_test <- kera_pred_transform(msft_dch_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)

kera_msft_dcm_x_test <- kera_pred_transform(msft_dcm_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)

kera_msft_dcl_x_test <- kera_pred_transform(msft_dcl_l_sub,
                                            steps_in = steps_in, 
                                            steps_out = steps_out)

#### All TI's in feature array

n_feat <- 12

x_train_4 <- array(c(x_train_3,
                     kera_msft_rsi_x_train,
                     kera_msft_vwap_x_train,
                     kera_msft_atr_x_train,
                     kera_msft_cmf_x_train,
                     kera_msft_mfi_x_train,
                     kera_msft_obv_x_train,
                     kera_msft_dch_x_train,
                     kera_msft_dcm_x_train,
                     kera_msft_dcl_x_train), 
                   dim= c(nrow(x_train_3), steps_in, n_feat))

x_test_4 <- array(c(x_test_3,
                    kera_msft_rsi_x_test,
                    kera_msft_vwap_x_test,
                    kera_msft_atr_x_test,
                    kera_msft_cmf_x_test,
                    kera_msft_mfi_x_test,
                    kera_msft_obv_x_test,
                    kera_msft_dch_x_test,
                    kera_msft_dcm_x_test,
                    kera_msft_dcl_x_test), 
                  dim= c(nrow(x_test_3), steps_in, n_feat))





###### To test independent features and combinations easier

# Note: To try unscaled features, run feature code without scaling.
#       For fundamental feature experimentation, need to change code above.
#       Do not forget to change ")" placement

n_feat <- 3 # change when needed

x_test_5 <- array(c(x_test,
                    kera_msft_rsi_x_test,
#                    kera_msft_vwap_x_test,
                    kera_msft_macd_x_test),
#                    kera_msft_atr_x_test,
#                    kera_msft_cmf_x_test,
#                    kera_msft_mfi_x_test,
#                    kera_msft_obv_x_test,
#                    kera_msft_dch_x_test,
#                    kera_msft_dcm_x_test,
#                    kera_msft_dcl_x_test),
                  dim= c(nrow(x_test), steps_in, n_feat)) # x_test for just Ti
                                                          # x_test_3 for + Fund 


dim(x_test_5)





#################
######## LSTM Wrangling for Next Day Prediction
#############################


# head(msft_l)
# nrow(msft_l)

### Creating Test and Train Spit


msft_day_train_l <- msft_l[1:sum(length(msft_l$date)-1),] 
# Note: -1 since test will get taken out during transform function; only taking
# out final validation set since value will get used later

msft_day_test_l <- msft_l[sum(length(msft_l$date)-1),]



msft_l_day_ret <- 100*diff(log(msft_day_train_l$adjusted)) 


# Scaling for LSTM Model; we will use a standard scaler instead of a min/max 
# scaler since a min/max scaler makes little practical sense in the context of
# our analysis



# Scaling returns subset:
sf_msft_l_day_ret <- scaling_factors(msft_l_day_ret)

msft_l_day_ret <- standard_scale(msft_l_day_ret, sf_msft_l_day_ret)



### Splitting Data


steps_in <- 15 # Last three weeks in trading days
steps_out <- 1

x_train_day <- kera_transform(msft_l_day_ret, x = T,
                          steps_in = steps_in, 
                          steps_out = steps_out)

y_train_day <- kera_transform(msft_l_day_ret, x = F,
                          steps_in = steps_in, 
                          steps_out = steps_out)

x_test_day <- kera_pred_transform(msft_l_day_ret,
                              steps_in = steps_in, 
                              steps_out = steps_out)


dim(x_train_day)
dim(y_train_day)
dim(x_test_day)

#################
######## LSTM Wrangling for Next Year Prediction
#############################


# head(msft_l)

### Creating Test and Train Spit


msft_year_train_l <- msft_l[1:sum(length(msft_l$date)-260),]
# See Note in LSTM Day Wrangling 

msft_year_test_l <- msft_l[sum(length(msft_l$date)-519):sum(length(msft_l$date)-260),]



msft_l_year_ret <- 100*diff(log(msft_year_train_l$adjusted)) 


# Scaling for LSTM Model; we will use a standard scaler instead of a min/max 
# scaler since a min/max scaler makes little practical sense in the context of
# our analysis



# Scaling returns subset:
sf_msft_l_year_ret <- scaling_factors(msft_l_year_ret)

msft_l_year_ret <- standard_scale(msft_l_year_ret, sf_msft_l_year_ret)



### Splitting Data


steps_in <- 520
steps_out <- 260 # 260 day trading period

x_train_year <- kera_transform(msft_l_year_ret, x = T,
                              steps_in = steps_in, 
                              steps_out = steps_out)

y_train_year <- kera_transform(msft_l_year_ret, x = F,
                              steps_in = steps_in, 
                              steps_out = steps_out)

x_test_year <- kera_pred_transform(msft_l_year_ret,
                                  steps_in = steps_in, 
                                  steps_out = steps_out)


