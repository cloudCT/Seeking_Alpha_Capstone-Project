##########
# Seeking Alpha - An analysis of stock market forecasting methods
#####################


#### Overview
#############

This project focuses on the analytical challenges of stock market prediction.

Within this analysis, several forecasting algorithms and techniques are explored and applied to stock market data. Performance of each individual method is evaluated and their application procedure outlined.

The primary objective is to compare and evaluate popular systems for stock market forecasting, aiming to build a viable forecasting model for practical use. Additionally, the project aims to provide a reference compendium of methods and descriptions of popular techniques that can be utilized to develop custom models.

Naturally, a fundamental level of understanding of the complex financial markets is necessary to be able to properly assess the methods, apply them and to find opportunity. Therefore, an essential summary will be provided if the pre-existing knowledge does not exist.



#############################################
###################

Suggestions, constructive criticsm and improvement ideas are always appreciated. They can be submitted to:

blueberry.cloud.ct@gmail.com

If there is interest in the topic, collaboration requests are welcomed and can be submitted to the same email



#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
#!#!#!#!#! Disclaimer

Contents of this project and analysis should not be taken as financial advice or be used to make investment decision!

The sole purpose of this analysis is the analytical challenge.

#!#!#!#!#!#!#!#!

Various good resources for financial advice are available online. However, they should always be treated with the appropriate amount of care. Don't be stupid or naive. While incredibly interesting, financial markets offer as much risk as opportunity. Being consistently profitable on the stock market is challenging for a reason. If you invest, you should do an appropriate amount of research beforehand and be aware of how much risk you want to take. Additionally, being familiar with financial markets allows you to make better, safer decisions and enables you to invest money more effectively, rather than blindly investing in funds without understanding their operations.


Don't be stupid

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!


#########################
#################
#######
README File Contents

1. Planned Future Updates
- Issues in need of fixing
- Areas in which improvement is planned
- Future Additions

2. Table of Contents for Scripts
3. Table of Contents for RMD/PDF Documentation







##########
### List of planned Updates
###########################

The following list includes necessary fixes, planned improvements and future 
additions. Status will be updated and shown here.



##########################
############# Fixes

####

- Package conflict of Metrics and forecast  (Fixed)
(accuracy function)



##########################
############# Improvements


#### Models

# LSTM

- Better tuning; more exploration
- Properly implementing features
- explore more features
- attention
- testing on more data

# ARIMA

- testing on more data
- exploring if order can be chosen that is applicable for various data
- own auto ordering function?
- maybe use it for feature extraction
- try arimax

# Prophet

- tuning
- maybe utilizing components as output for other model


#### RMD

- cleaner more appropriate spacing
- better theme for code
- improve aesthetic
- maybe replace dyplot screenshots
- Updated rmd rendering script (Done)
- Name chunks


#### Text

- maybe more explaining in LSTM section
- additional resources



#### Other

- Better README file






##########################
############# Additions


#### Models


Initial list of models that will be explored:


- Bidirectional LSTM (Priority)

- Echo State Network (ESN) (Priority)

- Multi-Layer Perceptron (MLP)

- Subsequent Artificial Neural Network (SANN)

- XGBoost

- Support vector machines (SVM)

- RandomForest

- LightGBM

- GARCH

- Transformers



### Ensembles

Various ensembles have to be tried out. Some examples:

- Arima-LSTM-XGBoost
- Arima-Garch
- Bidirectional CNN



#### Data

- other exchanges
- stocks with different characteristics
- training models on multiple stocks
- 


#### Features

- Sentiment indicator
- generally more features



#### RMD and Project Documentation

- Sections for added models
- Section on practical eval and application



#### Other

# Practical Eval Function
Function that applies models to all NASDAQ stocks, then chooses best models and
calculates returns

Function should include:

- Up down predictions
- price predictions
- option to add forecast horizon
- evaluation of stocks based on price difference
- top n stocks as output for both short and long positions
- investment amount input
- calculation of actual and predicted returns


Planned general process:


-> input all NASDAQ stock tickers
-> download current data
-> additional feature download if applicable
-> model run
-> predictions for specified horizon; output last value
-> transform if applicable
-> up down prediction either by:
calc of pred vs last price 
or bidirectional lstm pred
-> depending on predicted movement - or + dif
-> input of all stock into dataframe (movement, last price, pred price, diff)
-> top n stocks chosen
-> adding actual values to dataframe
-> calc of predicted returns and actual:
-specified investment(by argument input) / n top stocks (either even split or dendent on potential; maybe sharpe ratio involvement)
-split investment/ last stock price for each top n stock = n stocks bought
leftover either added or stored
-then ;    n stocks bought * last price = actual investment ; 
          (n stocks bought * pred price) - actual investment = pred returns
          (n stocks bought * actual price) - actual investment = actual returns
-subtracting transaction cost (lets work with 2%)

-> dataframe output with returns for each chosen stock, overal return and invested money if applicable (initial investment - leftover)


- maybe sharpe ratio as eval next to price dif





# Function should then be applied to be able to predict unknown future values





- Practical Eval Metrics
Such as Sharpe Ratio (maybe include in above function)









##########################################################################
########################################################################
###################
Contents of Scripts
###################


####

1. Utilized Dependencies and Libraries
All utilized dependencies and libraries listed to allow for a simplified initialization

2. Data Download Process
Showcasing Alpha Vantage and quantmod with tidyquant implementation
Both, features download (Technical and Fundamental Indicators) and stock data download is described

####


3. Data Exploration
Some Data exploration and showing how charts can be easily drawn

####

4. Wrangling for all Models
Includes Feature wrangling and creation of train/test sets
Additionally function to scale and prepare data for LSTM models (Rolling window approach)

5. Sliding Window approach for data sequencing LSTM


####
6. Prophet Analysis
7. Arima Analysis
8. LSTM Analysis
- CNN-LSTM
- Attention (Addentive, Multiplicative, Multi-Head)


####

9. Final Validation
Collection of model evaluations and function that simplifies the process of applying several stocks to models simultaneously for testing or evaluation. 

####

(10. setting rmd environment and rendering rmd)


##########################################################################
########################################################################
###################
Table of Contents for RMD/PDF Documentation
###################

1. Introduction
2. Abstract
   2.1 Prior Work and Research
   2.2 Hurdles and Pitfalls
   2.3 Mission Statement
3. Methodology
   3.1 Stock Market Data
   3.3 Features
   3.4 Algorithms, Models and Theoretical Basis
    3.4.1 Basic Machine Learning Algorithms
    3.4.2 Time Series Forecasting Algorithms
    3.4.3 Deep Learning Methods
    3.4.4 Ensemble Learning Methods
   3.5  Error Metrics
   
4. Preliminary Steps
   4.1 Downloading Dependencies and Libraries
   4.2 Downloading Data
   4.3 Data Exploration
5. Prophet
   5.1 Prophet Wrangling and Preprocessing
   5.2 Base Model
   5.3 Different Forecast Horizons
   5.4 Model Evaluation
6. ARIMA
   6.1 Wrangling and Preprocessing
   6.2 ARIMA Modeling
   6.3 Exogenous Regressors
   6.4 Different Forecast Horizons
   6.5 Model Evaluation
7. LSTM
   7.1 Wrangling and Preprocessing
   7.2 LSTM Modeling
   7.3 Adding Features
   7.4 CNN-LSTM
   7.5 CNN-LSTM with Attention Mechanism
   7.6 Different Forecast Horizons
   7.7 Model Evaluation
8. Final Validation
9. Summary
10. Further Resources















################################################################################################################################################################

##############
###### Author
### Tim Conze


###################
#### Hardware Used
# Macbook Pro 23
# Apple M2 with 16 GB Ram


Thank you very much for reading my project. Suggestions, critiscm and ideas for improvement are always appreciated and can be submitted to:


blueberry.cloud.ct@gmail.com











