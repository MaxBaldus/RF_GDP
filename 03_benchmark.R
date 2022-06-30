# Inspecting GDP and estimating ar & RW model 

ar = function(gdp){
  
  # xts package
  # gdp = ts(data$gdp_raw, start = data$dates[1], frequency = 4) # ts does not support date object 
  # if you want the character form of date in a time series object, you should consider using xts 
  # use xts package for a nicer plot
  
  gdp_ts = ts(gdp, start = c(1959,1), frequency = 4) 
  # The first number in the start parameter is the number of the period depending on the frequency, 
  # while the second number is the first incident in that period (as not all series begin at January or at Sunday).
  
  #--------------------------------------------------------------------------
  # Out of sample forecast of growth rate / returns (log difference)
  
  # 1) 1st order difference (extracting the trend)
  gdp_d = diff(gdp_ts)
  ts.plot(gdp_d, xlab="quarter", ylab="GDP_difference", type="l") 
  
  # stationary test: dickey-fuller test 
  
  # acf and pacf
  tsapp::acfpacf(gdp_d,50,HV="V")
  
  # centering data s.t. mean is 0 
  gdp_ct = gdp_d - mean(gdp_d)
  # gdp_ct = diff(gdp) # not centering
  
  # 2) removing cyclical component? -> via periodogram ??
  
  # fitting a random walk model
  rw = arima(gdp_ct, order = c(0,0,0), include.mean = FALSE)
  
  # fitting ARMA[1,1] model
  arma_fit = arima(gdp_ct, order = c(1,0,1)) 
  
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
  
  return(list(rw_fit = rw, arma_fit = arma_fit))
  

}

ar_rolling = function(gdp){
  # ---------------------------------------------------------------------- 
  # in sample forecast: fit everything with in-sample data (training data) & then forecast  test data (out of sample data)
  # using rolling windows - fix parameters and forecast each season's value, 
  # using a new value iteratively to avoid mean convergence too fast 
  # (since arma cannot do long-term forecasts since lim E[gdp^hat] = 0 (if gdp centered))
  
  # using model specifications in 
}
# add in inspection with periodogram etc. to make gdp stationary .. (if time and needed in the end)


hp = function(gdp){
  # ---------------------------------------------------------------------- 
  # Hodrick-Prescott Filter 
  
  # applying HP filter to remove trend and cyclical component using the HP filter
  # with lambda = 1600 (for quartely data)
  hp = hpfilter(gdp, freq = 1600)
  hp_res = gdp - hp$trend - hp$cycle # subtracting all components for original time series
  
  ts.plot(gdp, hp$trend, hp$cycle, hp_res,  xlab="Time", ylab="GDP", type="l", col = "blue", main = "HP Filter GDP") 
  legend("bottomright", legend = c("gdp", "Trend component", "Cyclical component", "residuals"), 
         col = c("blue", "red", "green", "black"), lty = 1, cex = 0.5)
  # "don't use HP filter for forecasting"
}