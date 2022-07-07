# Inspecting GDP and estimating ar & RW model 

ar = function(gdp){
  
  # xts package
  # gdp = ts(data$gdp_raw, start = data$dates[1], frequency = 4) # ts does not support date object 
  # if you want the character form of date in a time series object, you should consider using xts 
  # use xts package for a nicer plot
  
  gdp_ts = ts(gdp, start = c(1959,1), frequency = 4) 
  # The first number in the start parameter is the number of the period depending on the frequency, 
  # while the second number is the first incident in that period (as not all series begin at January or at Sunday).
  
  ts.plot(gdp_ts, xlab="Time", ylab="GDP", type="l") # plot ts object
  
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
  # gdp_ct = diff(gdp_ts) # not centering
  
  # 2) removing cyclical component? -> via periodogram ??
  
  # fitting a random walk model
  rw = arima(gdp_ct, order = c(0,0,0), include.mean = FALSE)
  
  # fitting ARMA[1,1] model
  arma_fit = arima(gdp_ct, order = c(1,0,1)) 
  
  # (from 01_forecasting..)
  # significance test 
  # Residual analysis
  # resiual check: white-noise ?
  # information criteria -> can be left out: or only compare RW and choosen ARMA model based on ACF and PACF
  # QQ plot
  
  
  # significance tests
  # model comparison via Information criteria 
  
  # fit model to the data via plot
  
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
  # include 95% CI -> x-values?
  lines(gdp_forecasts_arma + 1.96*sd(gdp_forecasts_arma),lwd=2, col = "orange")
  lines(gdp_forecasts_arma - 1.96*sd(gdp_forecasts_arma),lwd=2, col = "orange")
  legend("bottomright", legend = c("Prediticion arma model", "+ 95% Confidence intervall", "-95% Confidence intervall"), 
         col = c("red", "orange", "orange"), lty = 1, cex = 0.5)

  
  
  
  
  return(list(rw_fit = rw, arma_fit = arma_fit))
}

# add in inspection with periodogram etc. to make gdp stationary .. (if time and needed in the end)
# if ar coefficient large => might want to use ARFIMA ???
# + additional: model variance via a GARCH (ATSA book)

ar_rolling = function(gdp){
  # ---------------------------------------------------------------------- 
  # in sample forecast: fit everything with in-sample data (training data) & then forecast  test data (out of sample data)
  # using rolling windows - fix (actually estimate!!!) parameters and forecast each season's value, up to last value N
  # forecasting 5 years * 4 values per year = 20 values for first out-of-sample forecasts
  # then decreasing to 19 values for next out-of-sample forecast 
  # ... and so on .. 
  
  # ???
  # using a new value iteratively to avoid mean convergence too fast 
  # (since arma cannot do long-term forecasts since lim E[gdp^hat] = 0 (if gdp centered))
  
  # corona forecast will give huge forecasting error => stop forecasting before corona ?
  # but since not in in-sample data => can just stop there (no problem)
  
  # use 10_nikkei_arfima -> save forecast in entry matrix
  
  # possibilities: 
  # each step reduce prediction / forecast horizon by 1 step, s.t. I don't forecast beyond 2022 e.g. when using last 3 observations
  # s.t. can still evaluate forecast when using last observation in 2021
  # => but: then have only 1 entry for the 5 day ahead forecast, since can only use Q12017 to forecast Q12022
  # but: might be better since not that much data available overall 
  
  # or:
  # need to cut-off training data 10 years before (2012)?? otherwise cannot forecast 5 years ahead with using last datapoint
  # => last datapoint used is then Q42016 
  # nin = # number of in-sample data  
   
  
  # add name to each row: i.e. 1:current row - 1 = in sample observations used to forecast 
  # insert true gdp values in every 2nd column 
  # compute mse & mae: using 10_out_of_sample.r -> into help function.R 
  # compute other statistics if time ..
  
  # Nobs <- length(rk_parzen)  
  
  # zeros <- rep(0,(Nobs-Nin)*(2*length(forchor)))
  # result_max <- matrix(zeros,Nobs-Nin,2*length(forchor))
  # 
  # for(i in Nin:(Nobs-1)){
  #   outfit2 <- arfima::arfima(samp[1:i],order=orderfit, fixed = 
  #             list(phi=outfit$modes[[1]]$phi, theta=outfit$modes[[1]]$theta, frac=outfit$modes[[1]]$dfrac)) 
  #   # we estimate the model outfit chosen above, for the new data samples
  #   
  #   p <- predict(outfit2,n.ahead=max(forchor),prop.use = 100)
  #   # we use the whole history for our forecasts, prop.use = 100
  #   # p contains the forecasts for log rv
  #   
  #   # we calculate the forecasts for rv
  #   # and consider the bias correction
  #   # based on the forecast variance p[[1]]$exactVar[forchor]
  #   # (see PhD Thesis by Justin Veenstra, author of the arfima package, 
  #   # for the computation of p[[1]]$exactVar)
  #   rvnew <- exp(p[[1]]$Forecast+p[[1]]$exactVar/2) # calculates forecasts for rv
  #   
  #   # we feed results in the results matrix
  #   result_max[i-Nin+1,2*(1:length(forchor))-1] <- rvnew[forchor]
  # }
  
  # feed into real results:
  # result_max[1:(Nobs-Nin),2] <- rk_parzen[(Nin+1):Nobs]; # first column: use all RV's
  # 
  # for (j in 2:length(forchor)){
  #   result_max[1:(Nobs-Nin-forchor[j]+1),2*j] <- rk_parzen[(Nin+forchor[j]):Nobs]
  #   # j = 2 result_max[1: 5057 - 4866 - 5 + 1, 2 * 2 = 4 ]  = rks[4866 + 5 : end of sample 5057]
  #   # i.e. fill entire matrix up to last 4 rows
  #   result_max[(Nobs-Nin-forchor[j]+2):(Nobs-Nin),2*j-1] <- 0
  #   # j = 2 result_max[1: 5057 - 4866 - 5 + 1, 2 * 2 -1 = 3 ]
  #   # for last 4 rows: fill in 0, since cannot compute forecasts 
  # }
  
}
# feed into the results into extra function

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