## NOTE:
# This script serves as an all-in-one inclusion of the scripts. 
# For easier reading it is recommended to view them in their original separated
# form, which can be found on github.

##################################
############ Downloading Dependencies
#################






#############
################### Packages to Download


if(!require(tidyverse)) install.packages("tidyverse",repos ="http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite",repos ="http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret",repos ="http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate",repos ="http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown",repos ="http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr",repos ="http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr",repos ="http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",repos ="http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr",repos ="http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales",repos ="http://cran.us.r-project.org")

if(!require(widyr)) install.package("widyr",repos ="http://cran.us.r-project.org")
if(!require(knitr)) install.package("knitr",repos ="http://cran.us.r-project.org")
if(!require(gghighlight)) install.package("gghighlight",repos ="http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer",repos ="http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra",repos ="http://cran.us.r-project.org")


# Stock API:
if(!require(alphavantager)) install.packages("alphavantager")

if(!require(quantmod)) install.packages("quantmod")
if(!require(tidyquant)) install.packages("tidyquant")


# Prophet:
if(!require(prophet)) install.packages("prophet")

# Forecast and tseries (ARIMA):
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")

# LSTM:
if(!require(keras)) install.packages("keras")

# Uncomment to download parallel computing package
# install.package("doParallel") 





#############
################### Libraries

library(tidyverse)
library(caret)
library(dplyr)
library(tidyr)
library(scales)
library(widyr)
library(knitr)
library(lubridate)
library(magrittr)
#library(Metrics)
library(jsonlite)
library(reticulate)
library(quantmod)
library(httr)
library(kableExtra)
# library(doParallel) # If wanted



##############################################################################
##############################################################################


##################################
################## Downloading Data
###########################




# We begin by downloading the data we will be working with. To get the data we
# use a stock API called "Alpha Vantage":

# Note:
# To be able to use this API, users will need to visit the Alpha Vantage website
# and obtain a free API key.
# Link : https://www.alphavantage.co


# Install the API R package and load library:

install.packages("alphavantager")
# Or
# devtools::install_github("business-science/alphavantager")

library(alphavantager)


# Set your API key:

av_api_key("YBEX2W45GF6IXNJG")
print(av_api_key())



# Download stock tickers:

msft_van <- av_get(symbol     = "MSFT",
                   av_fun     = "TIME_SERIES_DAILY",
                   outputsize = "full")


msft_van




############
########## quantmod

# Here we download data with the quantmod package

library(quantmod)
library(tidyquant)


getSymbols("MSFT") # creates dataframe with ticker name of class xsx/zoo


## tidyquant

# With the help of the tidyquant package we can download the data as a tibble:

msft <- tq_get("MSFT",get="stock.prices")






###### Additional features:

## Financial Data:



# With quantmod:
tq_get("MSFT", get ="dividends")



# With AV:
av_api_key("YBEX2W45GF6IXNJG")



msft_fin <- av_get(symbol     = "MSFT",
                   av_fun     = "BALANCE_SHEET",
                   outputsize = "full")

msft_fin

library(httr)
base_url <- "https://www.alphavantage.co"
myquery <- list("function"="BALANCE_SHEET",
                "symbol"= "MSFT",
                "outputsize" = "full",
                "apikey"="YBEX2W45GF6IXNJG") # Remember to fill in your key

msft_fin <- GET(base_url, path = "query", query = myquery)

class(msft_fin)
names(msft_fin)
class(msft_fin$content)




# Convert the response content to a list
content_string <- rawToChar(msft_fin$content)
json_list <- fromJSON(content_string)

# Interrogate the list
class(json_list)
names(json_list)
names(json_list[1]) # json is lists of lists in R

# extracting "fiscalDateEnding", "totalAssets" and "total Liabilities"
result <- sapply(json_list[3], `[`)

fiscal_date <- sapply(json_list[3][[1]][[1]], `[`)
total_assets <-  sapply(json_list[3][[1]][[3]], `[`)
total_liabilities <- sapply(json_list[3][[1]][[20]], `[`)



msft_fin_dat <- tibble(fiscal_date=ymd(fiscal_date),
                       total_assets = as.numeric(total_assets),
                       total_liabilities = as.numeric(total_liabilities))


## Technical Indicators


# RSI
library(httr)
base_url <- "https://www.alphavantage.co"
myquery <- list("function" = "RSI",
                "symbol"= "MSFT",
                "interval" = "daily",
                "time_period"= "60",
                "outputsize" = "full",
                "series_type"= "close",
                "apikey"="YBEX2W45GF6IXNJG") # Remember to fill in your key

msft_rsi <- GET(base_url, path = "query", query = myquery)

class(msft_rsi)
names(msft_rsi)
class(msft_rsi$content)


# Convert the response content to a list
content_string <- rawToChar(msft_rsi$content)
json_list <- fromJSON(content_string)

# Interrogate the list
class(json_list)
names(json_list)
names(json_list[1]) # json is lists of lists in R

# extracting RSI
result <- sapply(json_list[2], `[`) %>% head

msft_rsi_av <- sapply(json_list[2][[1]], `[`) %>% as_tibble() %>% unnest(cols=c()) %>% 
  pivot_longer(cols = everything(),names_to="date", values_to ="rsi") %>% 
  mutate(date = ymd(str_remove(date, "\\.RSI$")),rsi = as.numeric(rsi)) %>% 
  rename(ds=date)






### Feature download with tq get and self-calculating
# No bottle cap on extractions since not using AV, but first few days excluded
# since self-calculating

tq_mutate_fun_options()


### Basic

# Volume

msft_vol <- tq_get("MSFT",get="stock.prices") %>% 
  select(volume,date)




#### Technical Indicators


# RSI (14 day RSI)
msft_rsi <- tq_get("MSFT",get="stock.prices") %>% 
  tq_transmute(select=adjusted,n = 14, mutate_fun=RSI) %>% 
  replace(is.na(.),0)



# VWAP (10 days)

msft_vwap <- tq_get("MSFT",get="stock.prices") %>% 
  mutate(price = (high + low + close) /3 ) %>% 
  tq_transmute_xy(x = price, y = volume, mutate_fun=VWAP) %>% 
  replace(is.na(.),0)



# MACD 
# On adjusted close,
# Long Term EMA = 26
# Short Term EMA = 12
# Signal line = 9 periods

msft_macd <- tq_get("MSFT",get="stock.prices") %>% 
  tq_transmute(select = adjusted,maType = "EMA", mutate_fun=MACD) %>% 
  select(date,macd) %>% 
  replace(is.na(.),0)




# ATR

msft_atr <- tq_get("MSFT",get="stock.prices") %>% 
  tq_transmute(select = c(high,low,close),n = 14, mutate_fun=ATR) %>% 
  select(date,atr) %>% 
  replace(is.na(.),0)



# CMF (Chaikin Money Flow)

tq_cmf <- function(clv, volume, n = 20){
  runSum(clv * volume, n)/runSum(volume, n)
}

msft_cmf <- tq_get("MSFT",get="stock.prices") %>% 
  tq_mutate(select = c(high, low, close), mutate_fun = CLV) %>%
  mutate(cmf = tq_cmf(clv, volume, 20)) %>% select(date,cmf) %>% 
  replace(is.na(.),0)




# MFI 

msft_mfi <- tq_get("MSFT",get="stock.prices") %>% 
  mutate(price = (high + low + close) /3 ) %>% 
  tq_transmute_xy(x = price, y = volume, mutate_fun=MFI) %>% 
  replace(is.na(.),0)



# OBV

msft_obv <- tq_get("MSFT",get="stock.prices") %>% 
  tq_transmute_xy(x = adjusted,y = volume, mutate_fun=OBV)



# Donchian Channels


msft_dc <- tq_get("MSFT",get="stock.prices") %>% 
  tq_transmute(select= c(high,low), mutate_fun=DonchianChannel) %>% 
  rename(dc_high = high,
         dc_mid = mid,
         dc_low = low) %>% replace(is.na(.),0)





### Interest Rate 

# Future projected via quantmod/tidyquant
fed_proj_rates <- tq_get("FEDTARMD", get = "economic.data",from = "2023-01-01", to  = "2025-12-31")

# Download via Alpha Vantage
fred_int_rate_av <- av_get(av_fun     = "FEDERAL_FUNDS_RATE",
                           outputsize = "full") %>% rename(date = timestamp)


### GDP
# Checked multiple sources for real us gdp.
# Projected GDP could only be found as growth percentage, while real growth
# numbers could not be found.
# Ultimately decided on choosing potential real us gdp since it includes 
# projections. Downloaded via tidyquant from FRED.


# Download via Alpha Vantage
fred_rea_gdp_av <- av_get(av_fun     = "REAL_GDP",
                          outputsize = "full") %>%
  rename(date = timestamp)


# Tidyquant
fred_rea_pot_gdp_tq <- tq_get("GDPPOT", get = "economic.data",
                              from = "2014-01-01", to  = "2025-12-31") 

##############################################################################
##############################################################################


##################
####### Data Exploration



## First look at dataframe
# Our data will be stored in an OHLCV format

head(msft)





## Highest Price

# Highest Open
msft %>% select(-symbol) %>% filter(open == max(open)) %>% 
  summarize(date,highest_open = open)

# Highest Adjusted Close
msft %>% select(-symbol) %>% filter(adjusted == max(adjusted)) %>% 
  summarize(date,highest_adjusted = adjusted)

# Highest Close
msft %>% select(-symbol) %>% filter(close == max(close)) %>% 
  summarize(date,highest_close = close)

# or
msft[which.max(msft$open),]
msft[which.max(msft$adjusted),]
msft[which.max(msft$close),]




## Lowest Price

# Lowest Open
msft %>% select(-symbol) %>% filter(open == min(open)) %>%
  summarize(date,lowest_open = open)

# Lowest Adjusted Close
msft %>% select(-symbol) %>% filter(adjusted == min(adjusted)) %>% 
  summarize(date,lowest_adjusted = adjusted)

# Lowest Close
msft %>% select(-symbol) %>%filter(close == min(close)) %>% 
  summarize(date,lowest_close =close)

# or
msft[which.min(msft$open),]
msft[which.min(msft$adjusted),]
msft[which.min(msft$close),]




## Average prices

msft %>% select(-symbol) %>% summarize(avg_open = mean(open),
                                       avg_adjusted = mean(adjusted),
                                       avg_close = mean(close)) 




## Day to day differences in price

# Open

msft %>% reframe(lag = diff(open)) %>% summarize(open_lag_avg = mean(lag),
                                                 open_lag_high = max(lag),
                                                 open_lag_low = min(lag),
                                                 sd = sd(lag))


# Adjusted Close

msft %>% reframe(lag = diff(adjusted)) %>% summarize(adj_lag_avg = mean(lag),
                                                     adj_lag_high = max(lag),
                                                     adj_lag_low = min(lag),
                                                     sd = sd(lag))


# Close
msft %>% reframe(lag = diff(close)) %>% summarize(close_lag_avg = mean(lag),
                                                  close_lag_high = max(lag),
                                                  close_lag_low = min(lag),
                                                  sd = sd(lag))


# Up-Down split


msft %>% reframe(adj = diff(adjusted)) %>%
  mutate(day = 2:sum(nrow(msft)), direction = case_when(adj > 0 ~ "Up",
                                                        adj < 0 ~ "Down",
                                                        TRUE ~ "No change")) %>%
  summarize(up = mean(direction == "Up"),
            down = mean(direction == "Down"),
            same = mean(direction == "No change"))



## Intraday difference

msft %>% group_by(date) %>% mutate(diff = open - close) %>% ungroup() %>% 
  select(date,diff) %>% 
  summarize(intraday_difference = mean(diff), sd = sd(diff))




## Average price on each trading day


msft %>% mutate(weekday = weekdays(as.Date(date))) %>% group_by(weekday) %>% 
  summarize(avg_close = mean(close),
            avg_open = mean(open),
            avg_adj = mean(adjusted),
            avg_vol = mean(volume)) %>% 
  arrange(match(weekday, c("Monday","Tuesday","Wednesday","Thursday","Friday")))





## Covid

msft %>% filter(date >= "2018-1-1") %>% mutate(date = year(date)) %>% 
  group_by(date) %>% select(-symbol) %>% summarize_all(mean) %>% 
  mutate(date = str_replace(date,"2019","First Covid year"))





######## Charts

library(quantmod)


# Line Chart

chartSeries(MSFT,
            subset = "last 11 years",
            type = "line",
            TA = NULL,
            name = " Microsoft Line Chart")
# or
# lineChart(MSFT)

# other stocks for comparison
getSymbols("GOOG")
chartSeries(GOOG,
            subset = "last 11 years",
            type = "line",
            TA = NULL,
            name = " Microsoft Line Chart")


getSymbols("AAPL")
chartSeries(AAPL,
            subset = "last 11 years",
            type = "line",
            TA = NULL,
            name = " Microsoft Line Chart")



# Bar Chart

chartSeries(MSFT,
            subset = "last 1 year",
            type = "bar",
            TA = NULL,
            name = " Microsoft Bar Chart")

# or
# barChart(MSFT)

# Candlestick Chart

chartSeries(MSFT,
            subset = "last 1 year",
            type = "candlestick",
            TA = NULL,
            name = " Microsoft Candle Chart")

# or
# candleChart(MSFT)




# Bollinger Bands



chartSeries(MSFT,
            subset = "last 1 year",
            type = "line",
            TA = NULL,
            name = "Bollinger Bands")
addBBands()



# MACD underlayed

chartSeries(MSFT,
            subset = "2023",
            type = "line",
            TA = "addMACD()",
            name = " Microsoft")






# Day-to-day differences and log returns

msft %>% reframe(open = diff(open),
                 adj = diff(adjusted),
                 close = diff(close)) %>%
  mutate(day = 2:sum(nrow(msft))) %>% 
  pivot_longer(cols =c(open,adj,close),names_to = "name", values_to = "price") %>% 
  ggplot(aes(day,price, color = name)) +
  geom_line()



msft %>% reframe(open = 100*diff(log(open)),
                 adj = 100*diff(log(adjusted)),
                 close = 100*diff(log(close))) %>%
  mutate(day = 2:nrow(msft)) %>% 
  pivot_longer(cols =c(open,adj,close),names_to = "name", values_to = "price") %>% 
  ggplot(aes(day,price, color = name)) +
  geom_line()+
  # scale_color_manual(values =c("red","cornflowerblue","darkorange"))+
  xlab(NULL)+
  ylab("Log Adjusted Price Difference") +
  theme_minimal()+
  ggtitle("Day-to-day Differences in Price")+
  labs(color ="Price")+
  theme( plot.title = element_text(size = (10)),
         panel.background = element_rect(fill = "cornsilk"),
         axis.text.x = element_blank()) 





# Average Volume

msft %>% select(date,volume) %>% mutate(year = year(date),month= month(date)) %>% 
  group_by(year,month) %>% mutate(volume = mean(volume)) %>% 
  ggplot(aes(date,volume))+
  geom_col(color = "darkgrey") +
  ylab("Average Volume") +
  xlab("Year")+
  theme_minimal()+
  ggtitle("Microsoft Average Daily Volume per Month")+
  theme( plot.title = element_text(size = (10)),
         panel.background = element_rect(fill = "cornsilk"))+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))





##############################################################################
##############################################################################

# This is a collection of all wrangling, for all methods. 

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



##############################################################################
##############################################################################



########
###### Splitting Data for Sliding Window Approach




#
# As Reminder:
# steps_in <- 40
# steps_out <- 20



##### Sliding Window function

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



####### msft Dataset Sliding Window Reshaping

x_train_sw <- kera_transform_sw(msft_l_ret_sub, x = T,
                                steps_in = steps_in, 
                                steps_out = steps_out)

y_train_sw <- kera_transform_sw(msft_l_ret_sub, x = F,
                                steps_in = steps_in,
                                steps_out = steps_out)

dim(y_train_sw)
dim(x_train_sw)








####### Sliding Window Features LSTM


# Note: As before "x_test" stays the same regardless of rolling or sliding 
#       windows




### Interest Rate Sliding Window


kera_int_x_train_sw <-  kera_transform_sw(int_rate_l_sub, x = T,
                                          steps_in = steps_in, 
                                          steps_out = steps_out)

dim(kera_int_x_train_sw)


x_train_2_sw <- array(c(x_train_sw,kera_int_x_train_sw), 
                      dim= c(nrow(x_train_sw), steps_in, 2))




### Volume Sliding Window



kera_msft_vol_x_train_sw <- kera_transform_sw(msft_vol_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_vol_x_train_sw)


n_feat <- 3
x_train_3_sw <- array(c(x_train_2_sw,kera_msft_vol_x_train_sw), 
                      dim= c(nrow(x_train_2_sw), steps_in, n_feat))




#### Technical Indicators Sliding Window

### RSI sw

kera_msft_rsi_x_train_sw <- kera_transform_sw(msft_rsi_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_rsi_x_train_sw)


### VWAP sw

kera_msft_vwap_x_train_sw <- kera_transform_sw(msft_vwap_l_sub, x = T,
                                               steps_in = steps_in, 
                                               steps_out = steps_out)

dim(kera_msft_vwap_x_train_sw)



### MACD sw

kera_msft_macd_x_train_sw <- kera_transform_sw(msft_macd_l_sub, x = T,
                                               steps_in = steps_in, 
                                               steps_out = steps_out)

dim(kera_msft_macd_x_train_sw)



### ATR sw


kera_msft_atr_x_train_sw <- kera_transform_sw(msft_atr_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_atr_x_train_sw)



### CMF sw

kera_msft_cmf_x_train_sw <- kera_transform_sw(msft_cmf_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_cmf_x_train_sw)


### MFI sw


kera_msft_mfi_x_train_sw <- kera_transform_sw(msft_mfi_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_mfi_x_train_sw)



### OBV sw

kera_msft_obv_x_train_sw <- kera_transform_sw(msft_obv_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_obv_x_train_sw)



### Donchian Channels (dc)

kera_msft_dch_x_train_sw <- kera_transform_sw(msft_dch_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

kera_msft_dcm_x_train_sw <- kera_transform_sw(msft_dcm_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

kera_msft_dcl_x_train_sw <- kera_transform_sw(msft_dcl_l_sub, x = T,
                                              steps_in = steps_in, 
                                              steps_out = steps_out)

dim(kera_msft_dch_x_train_sw)
dim(kera_msft_dcm_x_train_sw)
dim(kera_msft_dcl_x_train_sw)



#### All TI's in sliding window feature array


n_feat <- 12

x_train_4_sw <- array(c(x_train_3_sw,
                        kera_msft_rsi_x_train_sw,
                        kera_msft_vwap_x_train_sw,
                        kera_msft_atr_x_train_sw,
                        kera_msft_cmf_x_train_sw,
                        kera_msft_mfi_x_train_sw,
                        kera_msft_obv_x_train_sw,
                        kera_msft_dch_x_train_sw,
                        kera_msft_dcm_x_train_sw,
                        kera_msft_dcl_x_train_sw), 
                      dim= c(nrow(x_train_3_sw), steps_in, n_feat))




#### TI's SW Testing

# See previous LSTM feature testing

n_feat <- 2  # change when needed

x_train_5_sw <- array(c(x_train_sw,
                        #                        kera_msft_rsi_x_train_sw,
                        #                        kera_msft_vwap_x_train_sw,
                        #                        kera_msft_macd_x_train_sw,
                        kera_msft_atr_x_train_sw),
                      #                        kera_msft_cmf_x_train_sw,
                      #                        kera_msft_mfi_x_train_sw,
                      #                        kera_msft_obv_x_train_sw,
                      #                        kera_msft_dch_x_train_sw,
                      #                        kera_msft_dcm_x_train_sw,
                      #                        kera_msft_dcl_x_train_sw), 
                      dim= c(nrow(x_train_sw), steps_in, n_feat))






######## Sliding Window Wrangling for Next Day Prediction


steps_in <- 15
steps_out <- 1

x_train_sw_day <- kera_transform_sw(msft_l_day_ret, x = T,
                                    steps_in = steps_in, 
                                    steps_out = steps_out)

y_train_sw_day <- kera_transform_sw(msft_l_day_ret, x = F,
                                    steps_in = steps_in,
                                    steps_out = steps_out)


dim(x_train_sw_day)
dim(y_train_sw_day)


######## Sliding Window Wrangling for Next Year Prediction


steps_in <- 520
steps_out <- 260

x_train_sw_year <- kera_transform_sw(msft_l_year_ret, x = T,
                                     steps_in = steps_in, 
                                     steps_out = steps_out)

y_train_sw_year <- kera_transform_sw(msft_l_year_ret, x = F,
                                     steps_in = steps_in,
                                     steps_out = steps_out)

dim(x_train_sw_year)
dim(y_train_sw_year)









##############################################################################
##############################################################################



##################################
################## Prophet
###########################


# Install prophet and load library:

install.packages("prophet")

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

msft_preds_2

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

plot(msft_pro_mod1,msft_preds)



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
mape_day_f <- mape(msft_x[1:sum(length(msft_x$ds)-1),]$y ,msft_preds_day$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_day <- mape(msft_x_daily_test$y, 
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
mape_mon_f <- mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon <- mape(msft_x_monthly_test$y, 
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

rmse_mon_2 <- RMSE(msft_monthly_test$y, 
                   msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)



### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_2_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_2 <- mse(msft_monthly_test$y, 
                 msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)


### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_2_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_2 <- mae(msft_monthly_test$y, 
                 msft_preds_month_2[sum(length(msft_preds_month_2$ds)-19):length(msft_preds_month_2$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_2_f <- mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_2$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_2 <- mape(msft_monthly_test$y, 
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

rmse_mon_3 <- RMSE(msft_monthly_test$y, 
                   msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)



### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_3_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_3 <- mse(msft_monthly_test$y, 
                 msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)

### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_3_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_3 <- mae(msft_monthly_test$y, 
                 msft_preds_month_3[sum(length(msft_preds_month_3$ds)-19):length(msft_preds_month_3$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_3_f <- mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_3$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_3 <- mape(msft_monthly_test$y, 
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

rmse_macd <- RMSE(msft_macd_test$y, 
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

rmse_mon_4 <- RMSE(msft_monthly_test$y, 
                   msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)

### MSE

# MSE of all values (stock prices) in the dataset:

mse_mon_4_f <- mse(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)

# MSE of the unknown future period we are trying to predict:

mse_mon_4 <- mse(msft_monthly_test$y, 
                 msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)

### MAE

# MAE of all values (stock prices) in the dataset:

mae_mon_4_f <- mae(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)


# MAE of the unknown future period we are trying to predict:

mae_mon_4 <- mae(msft_monthly_test$y, 
                 msft_preds_month_4[sum(length(msft_preds_month_4$ds)-19):length(msft_preds_month_4$ds),]$yhat)


### MAPE

# MAPE of all values (stock prices) in the dataset:
mape_mon_4_f <- mape(msft_x[1:sum(length(msft_x$ds)-20),]$y ,msft_preds_month_4$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_mon_4 <- mape(msft_monthly_test$y, 
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
msft_pro_mod_year <- fit.prophet(msft_pro_mod_year, msft_yearly_train)




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
mape_yr_f <- mape(msft_x[1:sum(length(msft_x$ds)-260),]$y ,msft_preds_year$yhat)

# MAPE of the unknown future period we are trying to predict:

mape_yr <- mape(msft_x_yearly_test$y, 
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


##############################################################################
##############################################################################


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


preds_month_plot <- msft_monthly_test_a %>% cbind(p_month_2) %>% 
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



##############################################################################
##############################################################################


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



##############################################################################
##############################################################################


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











##############################################################################
##############################################################################

# To create rmd without redownloading data. To simplify the evaluation of models
# as new data would be included at redownload



# 
# my_env <- new.env()
# my_env$msft <- msft
# 
# 
# rmarkdown::render("Seeking-Alpha_Capstone-Project.Rmd", envir = my_env)




##############################################################################
##############################################################################


# Thank you very much for reading, please refer to the README file for updates



