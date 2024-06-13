


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
