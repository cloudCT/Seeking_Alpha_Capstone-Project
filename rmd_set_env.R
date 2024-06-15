
######### Rendering RMD

# Do not use knit!



# Run if not already downloaded with download script
# Also make sure packages are downloaded and keras is installed:

# Installing Keras (Tensorflow backend)
# install_keras()
# install_tensorflow()


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
# library(Metrics)
library(ModelMetrics)
library(jsonlite)
library(reticulate)
library(httr)
library(kableExtra)
# LSTM:
library(keras)
library(tensorflow)


###############################################################################

library(alphavantager)


# Set your API key:

av_api_key("YBEX2W45GF6IXNJG")
print(av_api_key())


library(quantmod)
library(tidyquant)


getSymbols("MSFT") # creates dataframe with ticker name of class xsx/zoo


## tidyquant

# With the help of the tidyquant package we can download the data as a tibble:

msft <- tq_get("MSFT",get="stock.prices")


# So we have the same subset originally used for training:

msft <- msft %>% filter(date <= "2024-02-20") 


##########

av_api_key("YBEX2W45GF6IXNJG")


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

# RMD Rendering without continuous AV download (bottlecap)

# Also make sure necessary libraries are loaded from pertaining script

my_env <- new.env()
my_env$msft <- msft
my_env$msft_rsi_av <- msft_rsi_av
my_env$fred_int_rate_av <- fred_int_rate_av
my_env$fred_rea_gdp_av <- fred_rea_gdp_av


rmarkdown::render("Seeking-Alpha_Capstone-Project.Rmd", envir = my_env)




