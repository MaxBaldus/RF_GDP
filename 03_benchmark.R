# Inspecting GDP and estimating ar & RW model 

ar = function(gdp){
  
  # xts package
  # gdp = ts(data$gdp_raw, start = data$dates[1], frequency = 4) # ts does not support date object 
  # if you want the character form of date in a time series object, you should consider using xts 
  # use xts package for a nicer plot
  
  gdp = ts(data$gdp, start = c(1959,1), frequency = 4) 
  # The first number in the start parameter is the number of the period depending on the frequency, 
  # while the second number is the first incident in that period (as not all series begin at January or at Sunday).
  
  ts.plot(gdp, xlab="quarter", ylab="GDP", type="l") # plot ts object
  
  #--------------------------------------------------------------------------
  # Out of sample forecast
  
  # 1) 1st order difference (extracting the trend)
  gdp_d = diff(gdp)
  ts.plot(gdp_d, xlab="quarter", ylab="GDP_difference", type="l") 
  
  # stationary test: dickey-fuller test 
  
  # acf and pacf
  acfpacf(gdp_d,50,HV="V")
  
  # centering data: 
  # gdp_ct = gdp_d - mean(gdp_d)
  gdp_ct = diff(gdp)
  
  # 2) removing cyclical component? -> via periodogram ??
  
  # fitting a random walk model
  rw = arima(gdp_ct, order = c(0,0,0), include.mean = FALSE)
  
  # fitting ARMA[1,1] model
  arma_fit = arima(gdp_ct, order = c(4,0,4)) 
  
  # (from 01_forecasting..)
  # significance test 
  # Residual analysis
  # information criteria
  # QQ plot
  
  # Forecasting
  h_max = 20 #forecast horizon: 5 years (i.e. 2o quarters)
  
  # Predictions
  rw_pred = predict(rw, h_max)
  arma_pred = predict(arma_fit, h_max)
  
  # inverting the difference filter for detrending and making ts non-stationary
  gdp_forecasts_rw =  diffinv(rw_pred$pred + mean(gdp_d), xi = gdp[length(gdp)]) 
  gdp_forecasts_arma =  diffinv(arma_pred$pred + mean(gdp_d), xi = gdp[length(gdp)]) 
  
  ts.plot(gdp,ylim = c(4500, 22000), xlim = c(1959,2027), xlab="quarter", ylab="GDP RW forecast", type="l")
  lines(gdp_forecasts_rw, col = "blue")
  
  ts.plot(gdp,ylim = c(4500, 22000), xlim = c(1959,2027), xlab="quarter", ylab="GDP ARMA forecast", type="l")
  lines(gdp_forecasts_arma, col = "red")
  
  # --------------------------------------------------------------------------------------------------------
  # in sample forecast: fit everything with training data & then forecast test data
  
  
  # ---------------------------------------------------------------------- 
  # Hodrick-Prescott Filter 
  
  # applying HP filter to remove trend and cyclical component using the HP filter
  # with lambda = 1600 (for quartely data)
  hp = hpfilter(gdp, freq = 1600)
  hp_res = gdp - hp$trend - hp$cycle # subtracting all components for original time series
  
  par(mfrow=c(2,2))
  ts.plot(gdp, xlab="quarter", ylab="GDP", type="l", col = "blue") 
  ts.plot(hp$trend, xlab="quarter", ylab="GDP_trend", type="l", col = "red") 
  ts.plot(hp$cycle, xlab="quarter", ylab="GDP_cycle", type="l", col = "green")
  ts.plot(hp_res, xlab="quarter", ylab="Resiudals", type="l", col = "black") 
  par(mfrow=c(1,1))
  
  # acf and pacf
  # acfpacf(hp_res,50,HV="V")  -> weird correlations??
  # "don't use HP filter for forecasting"
  
  
  return(list(rw_fit = rw, arma_fit = arma_fit))
  
  
  
}


# add in inspection with periodogram etc. to make gdp stationary .. (if time and needed in the end)

# rw forecast s