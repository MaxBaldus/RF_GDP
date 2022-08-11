# Estimating AR & RW models as benchmarks and forecasting 

# simple GDP forecast using first order difference
ar = function(gdp, ar_ord, ma_ord, h_max){
  par(mfrow = c(1,1))
  gdp_ts = ts(gdp, start = c(1959,2), frequency = 4) # create ts object
  
  # acf and pacf of nonstationary ts
  print(tsapp::acfpacf(gdp_ts, lag = 50))
  
  # 1) 1st order difference (extracting the trend)
  gdp_d = diff(gdp_ts)
  ts.plot(gdp_d, xlab="quarter", ylab="GDP_difference", type="l") 
  
  #centering ts
  gdp_ct = gdp_d - mean(gdp_d)
  
  # acf and pacf
  print(tsapp::acfpacf(gdp_ct, lag = 50, HV="V"))
 
  # stationary test: dickey-fuller test
  print(tseries::adf.test(gdp_ct))
  
  # Fitting
  #fitting ARMA[1,1] model
  arma_fit = arima(gdp_ct, order = c(ar_ord,0,ma_ord), include.mean = FALSE) 
  #significance tests
  coef = arma_fit$coef
  t = round(arma_fit$coef / diag(arma_fit$var.coef)^0.5, 4) # compute t-statistic
  p = round(2*(1-pnorm(abs(t))), 4) # compute corresponding p-value & round both to 4th value
  summa = cbind(coef, t, p)
  print(summa)
  
  # information criteria 
  ord = ar_ord + ma_ord # number of parameters estimated
  N = length(gdp) # length of ts
  res = arma_fit$resid
  IC = matrix(nrow = 1, ncol = 4)
  colnames(IC) = c("rss", "aic", "aicc", "bic")
  IC[1,"rss"] = sum( (res-mean(res))^2) # residual ssq
  IC[1,"aic"] = log(IC[1,"rss"]/N) + 2*ord/N # akaike information criterion
  IC[1,"aicc"] = log(IC[1,"rss"]/N) + 2*ord/(N-ord-2) # adjusted aic
  IC[1,"bic"] = log(IC[1,"rss"]/N) + ord*log(N)/N # baysian information criterion
  print(IC)
  
  # Residual analysis: are residuals white noise?
  print(LjungBoxPierceTest(arma_fit$resid, n.par = 2))
  #QQ plot
  qqnorm(arma_fit$resid)
  abline(0,sd(arma_fit$resid))
  
  # Predictions
  arma_pred = predict(arma_fit, n.ahead = h_max)

  # inverting the difference filter for detrending and making ts non-stationary
  gdp_forecasts_arma =  diffinv(arma_pred$pred + mean(gdp_d), xi = gdp[length(gdp)]) 
  

  return(list(fit = arma_fit, coefficients = summa, information_critieria = IC, 
              predicitons = arma_pred, predicitons_inverted = gdp_forecasts_arma))
}

# simple GDP growth forecast
ar_growth = function(gdp, ar_ord, ma_ord, h_max){
  par(mfrow = c(1,1))
  gdp_ts = ts(gdp, start = c(1959,2), frequency = 4) # create ts object
  
  # # acf and pacf of nonstationary ts
  # print(tsapp::acfpacf(gdp_ts, lag = 50))
  
  # # 1) 1st order difference (extracting the trend)
  # gdp_d = diff(gdp_ts)
  # ts.plot(gdp_d, xlab="quarter", ylab="GDP_difference", type="l") 
  
  # #centering ts
  # gdp_ct = gdp_d - mean(gdp_d)
  gdp_ct = gdp
  
  # acf and pacf
  print(tsapp::acfpacf(gdp_ct, lag = 50, HV="V"))
  
  # stationary test: dickey-fuller test
  print(tseries::adf.test(gdp_ct))
  
  # Fitting
  #fitting ARMA model
  arma_fit = arima(gdp_ct, order = c(ar_ord,0,ma_ord), include.mean = FALSE) 
  
  #significance tests
  coef = arma_fit$coef
  t = round(arma_fit$coef / diag(arma_fit$var.coef)^0.5, 4) # compute t-statistic
  p = round(2*(1-pnorm(abs(t))), 4) # compute corresponding p-value & round both to 4th value
  summa = cbind(coef, t, p)
  print(summa)
  
  # information criteria 
  ord = ar_ord + ma_ord # number of parameters estimated
  N = length(gdp) # length of ts
  res = arma_fit$resid
  IC = matrix(nrow = 1, ncol = 4)
  colnames(IC) = c("rss", "aic", "aicc", "bic")
  IC[1,"rss"] = sum( (res-mean(res))^2) # residual ssq
  IC[1,"aic"] = log(IC[1,"rss"]/N) + 2*ord/N # akaike information criterion
  IC[1,"aicc"] = log(IC[1,"rss"]/N) + 2*ord/(N-ord-2) # adjusted aic
  IC[1,"bic"] = log(IC[1,"rss"]/N) + ord*log(N)/N # baysian information criterion
  print(IC)
  
  # Residual analysis: are residuals white noise?
  print(LjungBoxPierceTest(arma_fit$resid, n.par = 2))
  #QQ plot
  qqnorm(arma_fit$resid)
  abline(0,sd(arma_fit$resid))
  
  # Predictions
  arma_pred = predict(arma_fit, n.ahead = h_max)
  
  
  return(list(fit = arma_fit, coefficients = summa, information_critieria = IC, 
              predicitons = arma_pred))
}

##################################################################################
# use specific gdp component extraction method and estimate linaer models again

# # 2) using seasonal trend decomposition to extract components
# out_stl = stl(gdp_ts, s.window = 7)
# plot(out_stl)
# e = out_stl$time.series[,3] # extracting the residuals
# 
# # acf and pacf
# print(tsapp::acfpacf(e, lag = 50, HV="V"))
# 
# # stationary test: dickey-fuller test 
# print(tseries::adf.test(e))
# 
# # Fitting
# #fitting ARMA[1,1] model
# arma_fit = arima(e, order = c(1,0,1)) 
# #significance tests
# coef = arma_fit$coef
# t = round(arma_fit$coef / diag(arma_fit$var.coef)^0.5, 4) # compute t-statistic
# p = round(2*(1-pnorm(abs(t))), 4) # compute corresponding p-value & round both to 4th value
# print(cbind(coef, t, p))
# 
# #fitting a random walk model
# rw = arima(e, order = c(0,0,0), include.mean = FALSE)

# # 2) using periodogram to test for a hidden periodic component
# periodo = tsapp::periodogram(e, length(e)/2)
# plot(periodo[,1], periodo[,2], type = "l", ylab = "periodogram", xlab = "frequency"
#      ) # plot first column (frequency) against periodogram ordinates
# # test for remaining hidden component
# print(tsapp::periodotest(e))

# add in inspection with periodogram etc. to make gdp stationary .. (if time and needed in the end)
# if ar coefficient large => might want to use ARFIMA ???
# + additional: model variance via a GARCH (ATSA book)


#################################################################################################
# Rolling window approach
ar_growth_rolling = function(gdp, ar_ord, ma_ord, h_max, forh){
  # gdp_ts = ts(gdp, start = c(1959,2), frequency = 4)
  
  # in sample forecast: fit everything with in-sample data (training data) & then forecast 
  # test data (out of sample data)
  # using rolling windows => estimate parameters iteratively with each new observation 
  # and forecast each season's value, up to last value N
  # forecasting 22 years * 4 values (quarters) per year = 88 values for the out-of-sample forecasts
  
  # using a new value iteratively to avoid mean convergence too fast 
  # (since arma cannot do long-term forecasts since lim E[gdp^hat] = 0 (if gdp centered))
  
  # corona forecast will give huge forecasting error => stop forecasting before corona ?
  # but since not in in-sample data => can just stop there (no problem)
  
  # each quarter: forecast 1,2,3,4 quarters ahead, using current observation to refit the model 
  # (get new parameter estimates),
  # but using the same (optimal) model order from above: ar_11
  # the h = 1,..., 4 forecasts are saved into a matrix, for each quarter, while each time a column is left empty
  # for the real results, which will be feed into later
   
  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations
  
  print(N) # 251 observations
  print(Nin) # 163 observations
  
  # initializing 
  # zeros = rep(0, (N-Nin)*2*length(forh)) # first row of result matrix 
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh))
  # result matrix: 88 rows + 1 row, since using last observation 251 for predictions
  h0 = matrix(0,  nrow = N-Nin + 1, ncol = 2) # initializing matrix for storing nowcast values
  
  # loop over each quarter from 2000 up to 2022
  for (i in Nin:(N)) {
    # estimate arma model again using new observation each time (but same coefficients)
    # data$df_trans$GDPC1[163] =  0.01629431 <=> 1999-12-01
    # all values up to 1999-12-01 are used in the first loop to estimate model
    # e.g i = Nin = 163 => use all gdp values up to 2000-03-01 (1st quarter are used)
    arma_fit = arima(gdp[1:i], order = c(ar_ord,0,ma_ord), include.mean = FALSE)
    # last value to be used to estimate model: 2021-12-01
    
    # h = 1,2,3,4
    # hence predicting 1st, 2nd, 3rd and 4th quarter 
    p = predict(arma_fit, n.ahead = max(forh)) # predict h = 1,2,3,4
    
    # feed prediction into result matrix, each h forecast into 3,5,7 column respectively (*2 .. -1),
    # starting with first row
    result[i-Nin+1,2*(1:length(forh))-1] = p$pred
    
    # h = 0: compute current fit (i.e. nowcast)
    # compute fitted values by subtracting residual of current fit (model) from current gdp value
    # when using i: first fitted value is 4th quarter 1999
    # but want fitted values only from 1st quarter 2000 onwards
    h0[i-Nin+1,1] = gdp[i] - arma_fit$residual[i] 
  }
  
  # deleting fit of 4th quarter 1999: starting with 1st quarter of 2000
  h0[1:(dim(h0)[1]-1),1] = h0[2:(dim(h0)[1]),1]
  h0[dim(h0)[1],1] = 0  # last row (is 0)
  
  
  
  result_ar = cbind(h0, result)
  colnames(result_ar) =  c("gdp forecast h=0", "gdp",
                           "gdp forecast h=1", "gdp", "gdp forecast h=2",
                           "gdp", "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  View(result_ar)
  return(result_ar)
  
}


