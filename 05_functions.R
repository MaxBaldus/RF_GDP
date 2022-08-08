# feed the true values into the results matrix
feed_in = function(result, gdp, h_max, forh){
  
  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations
  
  # gdp h=1: 
  result[1:((N-Nin)),2] = gdp[(Nin+1):N]
  # 2nd column: insert gdp values for h = 1
  # since we assume that we do'nt have values in 2022 (for comparison) 
  # => last entry in gdp h = 1 column needs to be 0 (no value), because 
  # forecasting first quarter 2022 using last quarter in 2021 in last row
  
  # gdp h=2:
  result[1:((N-Nin)-1),4] = gdp[(Nin+2):N]
  # because forecasting 2 quarters now, last row entry must be 0, since
  # data of 1st quarter 2022 not available now
  # first entry 
  
  # gdp h=3:
  result[1:((N-Nin)-2),6] = gdp[(Nin+3):N]
  
  # gdp h=4:
  result[1:((N-Nin)-3),8] = gdp[(Nin+4):N]
  
  # # gdp h=3,4
  # for (j in 3:max(forh)) {
  #   result[1:((N-Nin)-(j)),2*j] = gdp[(Nin+j):N]
  # }
  
  return(result)
}

# forecast evaluations
eval_forc = function(result, forh){
  num_hor = length(forh) # number of forecasts horizons
  num_obs = dim(result)[1] # number of forecasts
  
  me = rep(NA, num_hor) # initialize mean error
  mse = rep(NA,num_hor) # initialize mse (mean squared error)
  mae = rep(NA,num_hor) # initialize mae (mean absolute error)
  
  j = 1
  while (j <= num_hor) {
    result_2 = result[1:(num_obs-forh[j]+1),(2*j-1):(2*j)]
    # j = 1  => use first h : h = 1
    #  => slice result[1 bis no_obs - 1 + 1, 2*1-1 = 1 : 2*1 = 2] 
    # hence always use 1st and 2nd, 3rd and 4th column and so one
    # i.e. use all rows (since no zeros) and first and second column 
    # j = 2  => use 2nd h : h = 2
    # => slice result2[1 bis no_obs - 2 + 1, 2*2-1 = 3 : 2*1 = 4]
    # i.e. don't use last  row, since h = 2 not available anymore (0)
    
    me[j] = (sum(result_2[,1] - result_2[,2]))/dim(result_2)[1] # compute me
    mse[j] = (sum((result_2[,1] - result_2[,2])^2))/dim(result_2)[1] # compute mse
    mae[j] = (sum(abs(result_2[,1] - result_2[,2])))/dim(result_2)[1] # compute mae
    j = j + 1; 
  }
  return(list(me = me, mse=mse, mae =mae)) 
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


