
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


# So we have the same subset originally used for training:

msft <- msft %>% filter(date <= "2024-02-20") 



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


