
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







