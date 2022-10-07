# feed the true values into the results matrix (every 2nd column) for ar processes
feed_in = function(result, gdp, h_max, forh){
  
  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations
  
  # gdp h=0: 
  result[1:((N-Nin)),2] = gdp[(Nin+1):N]

  # gdp h=1: 
  result[1:((N-Nin)),4] = gdp[(Nin+1):N]
  # 4th column: insert gdp values for h = 1
  # since we assume that we dont have values in 2022 (for comparison) 
  # => last entry in gdp h = 1 column needs to be 0 (no value), because 
  # forecasting first quarter 2022 using last quarter in 2021 in last row
  
  # gdp h=2:
  result[1:((N-Nin)-1),6] = gdp[(Nin+2):N]
  # because forecasting 2 quarters now, last 2 rows must be 0, since
  # data of 1st quarter 2022 & 2nd quarter not available now ...
  
  # gdp h=3:
  result[1:((N-Nin)-2),8] = gdp[(Nin+3):N]
  
  # gdp h=4:
  result[1:((N-Nin)-3),10] = gdp[(Nin+4):N]
  
  # # gdp h=3,4
  # for (j in 3:max(forh)) {
  #   result[1:((N-Nin)-(j)),2*j] = gdp[(Nin+j):N]
  # }
  
  return(result)
}

##############################################################################################
# forecast evaluations
##############################################################################################
eval_forc = function(result, forh){
  # excluding nowcast h = 0
  result_1 = result[,-c(1,2)]
  
  num_hor = length(forh) # number of forecasts horizons (= 4, only considering h = 1,2,3,4 first)
  num_obs = dim(result_1)[1] # number of forecasts (88)

  me = rep(NA, num_hor + 1) # initialize mean error
  mse = rep(NA,num_hor + 1) # initialize mse (mean squared error)
  mae = rep(NA,num_hor + 1) # initialize mae (mean absolute error)
  rmse = rep(NA,num_hor + 1) # initialize rmse (root mean squared error)
  
  j = 1
  while (j <= num_hor) {
    result_2 = result_1[1:(num_obs-forh[j]+1),(2*j-1):(2*j)]
    # j = 1  => use all observations (-1 + 1)
    #  => slice result[1 bis no_obs - 1 + 1, 2*1-1 = 1 : 2*1 = 2] 
    # hence always use 1st and 2nd, 3rd and 4th column and so one
    # i.e. use all rows (since no zeros) and first and second column 
    # j = 2  => use 2nd h : h = 2
    # => slice result2[1 bis no_obs - 2 + 1, 2*2-1 = 3 : 2*1 = 4]
    # i.e. don't use last  row, since h = 2 not available anymore (0)
    
    me[j + 1] = (sum(result_2[,1] - result_2[,2]))/dim(result_2)[1] # compute me
    mse[j + 1] = (sum((result_2[,1] - result_2[,2])^2))/dim(result_2)[1] # compute mse
    mae[j+ 1] = (sum(abs(result_2[,1] - result_2[,2])))/dim(result_2)[1] # compute mae
    rmse[j+ 1] = sqrt( (sum((result_2[,1] - result_2[,2])^2))/dim(result_2)[1]) # compute rmse
    
    j = j + 1; 
  }
  # compute statistics for h = 0 (first entry in vector)
  me[1] = (sum(result[,1] - result[,2]))/dim(result)[1] # compute mean error 
  mse[1] = (sum((result[,1] - result[,2])^2))/dim(result)[1] # compute mse
  mae[1] = (sum(abs(result[,1] - result[,2])))/dim(result)[1] # compute mae
  rmse[1] = sqrt( (sum((result[,1] - result[,2])^2))/dim(result)[1]) # compute mae
  
  # get min values respectively
  min_me = which.min(me) # h = 4 forecast has min me value ??
  min_mse = which.min(mse) # h = 0 has min mse value
  min_mae = which.min(mae) #  h = 0 has min mae value
  min_rmse = which.min(rmse) #  h = 0 has min mae value
  
  
  return(list(me = me, mse=mse, mae =mae, rmse = rmse,
              smins = list(min_me = min_me,
                          min_mse = min_mse, min_mae = min_mae, min_rmse = min_rmse)))
}
# random forest evaluation
eval_forc_rf = function(result, forh){
  # excluding nowcast h = 0
  result_1 = result[,-c(1,2)]
  
  num_hor = length(forh) # number of forecasts horizons (= 4, only considering h = 1,2,3,4 first)
  num_obs = dim(result_1)[1] # number of forecasts (88)
  
  me = rep(NA, num_hor + 1) # initialize mean error
  mse = rep(NA,num_hor + 1) # initialize mse (mean squared error)
  mae = rep(NA,num_hor + 1) # initialize mae (mean absolute error)
  rmse = rep(NA,num_hor + 1) # initialize rmse (root mean squared error)
  
  j = 1
  while (j <= num_hor) {
    result_2 = result_1[,(2*j-1):(2*j)]
    h = 1
    
    me[j + 1] = (sum(result_2[,1] - result_2[,2]))/ (num_obs - h + 1) # compute me
    mse[j + 1] = (sum((result_2[,1] - result_2[,2])^2))/ (num_obs - h + 1) # compute mse
    mae[j+ 1] = (sum(abs(result_2[,1] - result_2[,2])))/ (num_obs - h + 1) # compute mae
    rmse[j+ 1] = sqrt( (sum((result_2[,1] - result_2[,2])^2))/ (num_obs - h + 1)) # compute rmse
    
    h = h + 1
    j = j + 1; 
  }
  # compute statistics for h = 0 (first entry in vector)
  me[1] = (sum(result[,1] - result[,2]))/ dim(result)[1]  # compute mean error 
  mse[1] = (sum((result[,1] - result[,2])^2))/ dim(result)[1] # compute mse
  mae[1] = (sum(abs(result[,1] - result[,2])))/ dim(result)[1] # compute mae
  rmse[1] = sqrt( (sum((result[,1] - result[,2])^2))/  dim(result)[1]) # compute mae
  
  # for (i in 1:5) {
  #   h = 0
  #   me[i] = (sum(result[,i] - result[,(i+1)]))/ (num_obs - h + 1) # compute me
  #   mse[i] = (sum((result[,i] - result[,i+1])^2))/ (num_obs - h + 1) # compute mse
  #   mae[i] = (sum(abs(result[,i] - result[,i+1])))/ (num_obs - h + 1) # compute mse
  #   rmse[i] =  sqrt((sum((result[,i] - result[,i+1])^2)) / (num_obs - h + 1) ) 
  #   h = h + 1
  # }
  
  # get min values respectively
  min_me = which.min(me) # h = 4 forecast has min me value ??
  min_mse = which.min(mse) # h = 0 has min mse value
  min_mae = which.min(mae) #  h = 0 has min mae value
  min_rmse = which.min(rmse) #  h = 0 has min mae value
  
  
  return(list(me = me, mse=mse, mae =mae, rmse = rmse,
              smins = list(min_me = min_me,
                           min_mse = min_mse, min_mae = min_mae, min_rmse = min_rmse)))
}

##############################################################################################
# converting gdp growth rate back to GDP levels
##############################################################################################
invert_growth = function(df, y_0){
  df_inv = matrix(0, nrow = nrow(df) + 1, ncol = ncol(df)) # initialize empty matrix
  # store respective base value 1999Q4 as the first entry
  for (j in 1:5) {
    df_inv[1,(2*j-1)] = y_0
  }
  # df_inv[1,1] = y_0 # h = 0 and h=1 first entry is y_0
  # df_inv[1,3] = y_0 # h = 0 and h=1 first entry is y_0
  # for (j in 3:5) {
  #   df_inv[1,(2*j-1)] = df[1,(2*j-2)] # always save gdp value of period before 
  # }
  # attach exp(forecasts) as rows below base values respectively
  for (j in 1:5) {
    # browser()
    df_inv[-1,(2*j-1)] = exp(df[,(2*j-1)]) # computing exp. transformation of growth forecasts 
  }
  #always compute percentage change from the TRUE GDP value of the period before 
  #first period
  for (j in 1:5) {
    df_inv[2,(2*j-1)] = df_inv[1,(2*j-1)] * df_inv[2,(2*j-1)]
  }
  # values after
  for (j in 1:5) {
    for (i in 3:(nrow(df)+1)) {
      # browser()
      df_inv[i,(2*j-1)] = df[i-1,2] * df_inv[i,(2*j-1)] # df[i-1,(2*j)]
    }
  }
  # insert gdp values again (2nd, 4th, 6th column etc.)
  df_inv[-1,seq(2,ncol(df),by = 2)] = df[,seq(2,ncol(df),by = 2)]
  print(df_inv)
  return(df_inv[-1,]) # dont return first row (only respective base value)
}

##############################################################################################
# converting gdp growth rate back to GDP levels: only 1 base value
##############################################################################################
invert_growth_err_acc = function(df, y_0){
  df_inv = matrix(0, nrow = nrow(df), ncol = ncol(df)) # initialize empty matrix
  for (j in 1:5) {
    df_inv[,(2*j-1)] = exp(cumsum(df[,(2*j-1)])) * y_0 # compute levels again from forecasts (h=0,...,4)
  }
  # long-version
  # # store respective base values as the first entry, always using gdp value of period before
  # df_inv[1,1] = y_0 # h = 0 and h=1 first entry is y_0
  # df_inv[1,3] = y_0 # h = 0 and h=1 first entry is y_0
  # for (j in 3:5) {
  #   df_inv[1,(2*j-1)] = df[1,(2*j-2)] # always save gdp value of period before 
  # }
  # # attach exp(forecasts) as rows below base values respectively
  # for (j in 1:5) {
  #   # browser()
  #   df_inv[-1,(2*j-1)] = exp(df[,(2*j-1)]) # computing exp. transformation of growth forecasts 
  # }
  # # always compute percentage change onto (estimated) base value from period before
  # for (j in 1:5) {
  #   for (i in 2:(nrow(df)+1)) {
  #     # browser()
  #     df_inv[i,(2*j-1)] = df_inv[i-1,(2*j-1)] * df_inv[i,(2*j-1)]
  #   }
  # }
  # ERROR ACCUMULATES
  # insert gdp values again (2nd, 4th, 6th column etc.)
  df_inv[,seq(2,ncol(df),by = 2)] = df[,seq(2,ncol(df),by = 2)]
  return(df_inv)
}

##############################################################################################
# Hodrick-Prescott Filter 
##############################################################################################
hp = function(gdp){
  gdp_ts = ts(gdp)
  # applying HP filter to remove trend and cyclical component 
  # with lambda = 1600 (for quartely data)
  hp = hpfilter(gdp_ts, freq = 1600)
  hp_res = gdp_ts - hp$trend - hp$cycle # subtracting all components for original time series
  
  ts.plot(gdp, hp$trend, hp$cycle, hp_res,  xlab="Time", ylab="GDP", type="l", 
          col = c("blue", "red", "green", "black"), main = "HP Filter GDP") 
  legend("bottomright", legend = c("gdp", "Trend component", "Cyclical component", "residuals"), 
         col = c("blue", "red", "green", "black"), lty = 1, cex = 0.5)
  # "don't use HP filter for forecasting"
  return(list("hp"= hp, "residuals" = hp_res))
}

##############################################################################################
# Theil's U and DM test
##############################################################################################

dm_tests = function(gdp, h_num,
                   result_rf, result_arma){
  # h_num = 5: having five forecast hozions
  # for horizons h = 1,...,4: 
  # first for gdp growth
  for (h in 1:(h_num-1)) {
    browser()

    # always deleting forecasts not observed (for each h)
    e_arma = gdp[h:length(gdp)] - result_arma[1:(nrow(result_arma)-h),2+((2*h)-1)] # starting with h = 1
    e_rf = gdp[h:length(gdp)] - result_rf[1:(nrow(result_rf)-h),2+((2*h)-1)]
    forecast::dm.test(e1 = e_arma, e2 = e_rf, h = h) 
  }
  
}

