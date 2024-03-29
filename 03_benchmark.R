# Estimating AR & RW models as benchmarks and forecasting 
##################################################################################
# simple GDP forecast using first order difference
ar = function(gdp, ar_ord, ma_ord, h_max){
  par(mfrow = c(1,1))
  gdp_ts = ts(gdp, start = c(1959,3), frequency = 4) # create ts object
  
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
  gdp_ts = ts(gdp, start = c(1959,3), frequency = 4) # create ts object
  
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
# recursive oos forecasting scheme
##################################################################################
ar_rolling_recursive = function(gdp, ar_ord, ma_ord, h_max, forh, Fstdf, xi){
  # make gdp stationary via 1st differences if Fstdf = TRUE
  if (Fstdf == TRUE) {
    gdp_d = diff(gdp) # 1st differencing ts
    gdp_ct = gdp_d - mean(gdp_d) # centering ts
    gdp = gdp_ct
  }

  # in sample forecast: fit everything with in-sample data (training data) & then
  # directly forecast the corresponding test data (out of sample data)
  # by using a rolling window => estimate parameters iterative with each new observation
  # and forecast each season's value, up to last value N
  # for each horizon h

  # each quarter: forecast 1,2,3,4 quarters ahead, using current observation to refit the model
  # (get new parameter estimates),
  # but using the same (optimal) model order from above: ar_11
  # the h = 1,..., 4 forecasts are saved into a matrix, for each quarter, while each time a column is left empty
  # for the real results, which will be feed into later

  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations

  print(paste("N = ", N)) # 251 observations
  print(paste("first N_in = ", Nin)) # 163 observations

  # initializing
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh))
  # result matrix: 88 rows + 1 row, since using last observation 251 for predictions
  h0 = matrix(0,  nrow = N-Nin + 1, ncol = 2) # initializing matrix for storing nowcast values

  # loop over each quarter from 2000 up to 2022
  for (i in Nin:(N)) {
    
    # estimate arma model again using new observation each time 
    # data$GDPC1[163] =  0.01629431 <=> 1999.75
    # all values up to last quarter of 1999 are used in the first loop to estimate model
    # e.g i = Nin = 163 => use all gdp values up to 2000Q1 (1st quarter are used)
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

  # compute gdp values back (not differenced ts)
  if (Fstdf == TRUE) {
    for (i in (seq(1, ncol(result_ar), 2))) { # for each column: compute inverse of diff. operation
      result_ar[,i] =  diffinv(result_ar[,i] + mean(gdp_d), xi = xi)[-1]
      # xi is the starting value of the differenced series (gdp) + adding mean again (non_centered)
    }
  }

  colnames(result_ar) =  c("gdp forecast h=0", "gdp",
                           "gdp forecast h=1", "gdp", "gdp forecast h=2",
                           "gdp", "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  # View(result_ar)
  return(result_ar)
}


##################################################################################
# Rolling window approach direct approach
##################################################################################
ar_rolling = function(gdp, ar_ord, ma_ord, h_max, forh, Fstdf, xi){
  # make gdp stationary via 1st differences if not gdp growth rate
  if (Fstdf == TRUE) {
    gdp_d = diff(gdp) # 1st differencing ts
    gdp_ct = gdp_d - mean(gdp_d) # centering ts
    gdp = gdp_ct
  }
  # in sample forecast: fit everything with in-sample data (training data) & then
  # directly forecast the corresponding test data (out of sample data)
  # by using a rolling window => estimate parameters iterative with each new observatio
  # and forecast each season's value, up to last value N
  # for each horizon h
  
  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations
  print(paste("T = ", N)) 
  print(paste("T_in = ", Nin)) 
  
  # initializing
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh)+2)
  # result matrix: 88 rows + 1 row, since using last observation 251 for predictions -> only 88!!!
  
  # loop over each quarter from 2000 up to 2022,
  # doing a direct oos forecast for each horizon h
  # col_counter = seq(from = 1, to =2*length(forh)+1, by = 2)
  col_counter = 1
  for (h in c(0, forh)) {
    gdp_direct = gdp[1:(length(gdp)-h)] # y_t+h = f(y_t) (loose observation respectively for each h)
    for (i in Nin:(N)) {
      arma_fit = arima(gdp_direct[1:i], order = c(ar_ord,0,ma_ord), include.mean = FALSE)
      # compute fitted value & save
      p = gdp_direct[i] - arma_fit$residuals[i] # p = fitted(arma_fit)[i]
      result[i-Nin+1,col_counter] = p
    } 
    # browser()
    col_counter = col_counter + 2
  }
  # invert difference
  if (Fstdf == TRUE) {
    for (i in (seq(1, ncol(result), 2))) { # for each column: compute inverse of diff. operation
      result[,i] =  diffinv(result[,i] + mean(gdp_d), xi = xi)[-1]
      # xi is the starting value of the differenced series (gdp) + adding mean again (non_centered)
    }
  }
  colnames(result) =  c("gdp forecast h=0", "gdp",
                        "gdp forecast h=1", "gdp", "gdp forecast h=2",
                        "gdp", "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  return(result)
}

