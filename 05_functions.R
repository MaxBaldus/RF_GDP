# feed the true values into the results matrix (every 2nd column)
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

# forecast evaluations
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
  min_me = which.min(eval_for_ar_growth$me) # h = 4 forecast has min me value ??
  min_mse = which.min(eval_for_ar_growth$mse) # h = 0 has min mse value
  min_mae = which.min(eval_for_ar_growth$mae) #  h = 0 has min mae value
  min_rmse = which.min(eval_for_ar_growth$rmse) #  h = 0 has min mae value
  
  
  return(list(me = me, mse=mse, mae =mae, rmse = rmse, 
              mins = list(min_me = min_me, 
                          min_mse = min_mse, min_mae = min_mae, min_rmse = min_rmse))) 
}

# converting gdp growth rate back to GDP levels
invert_growth = function(df, y_0){
  df_inv = matrix(0, nrow = nrow(df), ncol = ncol(df)) # initialize empty matrix
  for (j in 1:5) {
    df_inv[,(2*j-1)] = exp(cumsum(df[,(2*j-1)])) * y_0 # compute levels again 
  }
  # insert gdp values again (2nd, 4th, 6th column etc.)
  df_inv[,seq(2,ncol(df),by = 2)] = df[,seq(2,ncol(df),by = 2)]  
  return(df_inv)
}


##############################################################################################
# Hodrick-Prescott Filter 
hp = function(gdp){
  
  # applying HP filter to remove trend and cyclical component using the HP filter
  # with lambda = 1600 (for quartely data)
  hp = hpfilter(gdp, freq = 1600)
  hp_res = gdp - hp$trend - hp$cycle # subtracting all components for original time series
  
  ts.plot(gdp, hp$trend, hp$cycle, hp_res,  xlab="Time", ylab="GDP", type="l", col = "blue", main = "HP Filter GDP") 
  legend("bottomright", legend = c("gdp", "Trend component", "Cyclical component", "residuals"), 
         col = c("blue", "red", "green", "black"), lty = 1, cex = 0.5)
  # "don't use HP filter for forecasting"
}

# helper:
# see which variables of the dataframe are in the fred description

# varlist_df %in% varlist_fred$fred # returns a boolean TRUE or FALSE value depending on whether the element is found or not
# which(varlist_df %in% varlist_fred$fred == FALSE) # gives the indices, that are false
# varlist_df[c(which(varlist_df %in% varlist_fred$fred == FALSE))]


