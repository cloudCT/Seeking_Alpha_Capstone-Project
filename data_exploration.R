

##################
####### Data Exploration



## First look at dataframe
# Our data will be stored in an OHLCV format

head(msft)




## Highest Price

# Highest Open
msft %>% select(-symbol) %>% filter(open == max(open)) %>% 
  summarize(date,highest_open = open) %>% kable() 

# Highest Adjusted Close
msft %>% select(-symbol) %>% filter(adjusted == max(adjusted)) %>% 
  summarize(date,highest_adjusted = adjusted) %>% kable() 

# Highest Close
msft %>% select(-symbol) %>% filter(close == max(close)) %>% 
  summarize(date,highest_close = close) %>% kable() 

# or
msft[which.max(msft$open),]
msft[which.max(msft$adjusted),]
msft[which.max(msft$close),]




## Lowest Price

# Lowest Open
msft %>% select(-symbol) %>% filter(open == min(open)) %>%
  summarize(date,lowest_open = open) %>% kable() 

# Lowest Adjusted Close
msft %>% select(-symbol) %>% filter(adjusted == min(adjusted)) %>% 
  summarize(date,lowest_adjusted = adjusted) %>% kable() 

# Lowest Close
msft %>% select(-symbol) %>%filter(close == min(close)) %>% 
  summarize(date,lowest_close =close)%>% kable() 

# or
msft[which.min(msft$open),]
msft[which.min(msft$adjusted),]
msft[which.min(msft$close),]




## Average prices

msft %>% select(-symbol) %>% summarize(avg_open = mean(open),
                                       avg_adjusted = mean(adjusted),
                                       avg_close = mean(close)) %>% kable() 




## Day to day differences in price

# Open

msft %>% reframe(lag = diff(open)) %>% summarize(open_lag_avg = mean(lag),
                                                  open_lag_high = max(lag),
                                                  open_lag_low = min(lag),
                                                 sd = sd(lag)) %>% kable() 


# Adjusted Close

msft %>% reframe(lag = diff(adjusted)) %>% summarize(adj_lag_avg = mean(lag),
                                                  adj_lag_high = max(lag),
                                                  adj_lag_low = min(lag),
                                                  sd = sd(lag)) %>% kable() 


# Close
msft %>% reframe(lag = diff(close)) %>% summarize(close_lag_avg = mean(lag),
                                                    close_lag_high = max(lag),
                                                    close_lag_low = min(lag),
                                                  sd = sd(lag))%>% kable() 


# Up-Down split


msft %>% reframe(adj = diff(adjusted)) %>%
  mutate(day = 2:sum(nrow(msft)), direction = case_when(adj > 0 ~ "Up",
                                                        adj < 0 ~ "Down",
                                                        TRUE ~ "No change")) %>%
  summarize(up = mean(direction == "Up"),
            down = mean(direction == "Down"),
            same = mean(direction == "No change")) %>% kable() 



## Intraday difference

msft %>% group_by(date) %>% mutate(diff = open - close) %>% ungroup() %>% 
  select(date,diff) %>% 
  summarize(intraday_difference = mean(diff), sd = sd(diff)) %>% kable() 




## Average price on each trading day


msft %>% mutate(weekday = weekdays(as.Date(date))) %>% group_by(weekday) %>% 
  summarize(avg_close = mean(close),
            avg_open = mean(open),
            avg_adj = mean(adjusted),
            avg_vol = mean(volume)) %>% 
  arrange(match(weekday, c("Monday","Tuesday","Wednesday","Thursday","Friday"))) %>% 
  kable() 





## Covid

msft %>% filter(date >= "2018-1-1") %>% mutate(date = year(date)) %>% 
  group_by(date) %>% select(-symbol) %>% summarize_all(mean) %>% 
  mutate(date = str_replace(date,"2019","First Covid year")) %>% kable()





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
            TA = c(addBBands()),
            name = "Bollinger Bands") 




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
  ggtitle("Day-to-day Differences in Price")+
  xlab(NULL)+
  ylab("Price Difference") +
  theme_minimal()+
  geom_line()+
  labs(color ="Price")+
  theme( plot.title = element_text(size = (10)),
         panel.background = element_rect(fill = "antiquewhite1"),
         axis.text.x = element_blank()) 



msft %>% reframe(open = 100*diff(log(open)),
                 adj = 100*diff(log(adjusted)),
                 close = 100*diff(log(close))) %>%
  mutate(day = 2:nrow(msft)) %>% 
  pivot_longer(cols =c(open,adj,close),names_to = "name", values_to = "price") %>% 
  ggplot(aes(day,price, color = name)) +
  geom_line()+
  #scale_color_manual(values =c("red","cornflowerblue","darkorange"))+
  xlab(NULL)+
  ylab("Log Adjusted Price Difference") +
  theme_minimal()+
  ggtitle("Daily Log Adjusted Returns")+
  labs(color ="Price")+
  theme( plot.title = element_text(size = (10)),
         panel.background = element_rect(fill = "antiquewhite1"),
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
  


