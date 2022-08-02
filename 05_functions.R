# feed the true values into the results matrix
feed_in = function(result, gdp, h_max){
  forh = c(1,2,3,4) # forecast horizon -> include h = 0 ????????????????
  N = length(gdp) # length of time series
  Nin = N - h_max # length of in sample observations
  
  # gdp h=1: 
  result[1:((N-Nin)-1),2] = gdp[(Nin+2):N]
  # 2nd column: insert gdp values for h = 1
  # since we assume that we do'nt have values in 2022 (for comparison) 
  # => last entry in gdp h = 1 column needs to be 0 (no value)
  
  # gdp h=2:
  result[1:((N-Nin)-2),4] = gdp[(Nin+3):N]
  # because forecasting 2 quarters now, last 2 entries are 0 now

  # gdp h=3,4
  for (j in 3:max(forh)) {
    result[1:((N-Nin)-(j)),2*j] = gdp[(Nin+j+1):N]
  }
  
  View(result)
  return(result)
}





# forecast evalutions

# mse_mae_eval_forc <- function(result2, forchor){
#   # this is useful when result2 is not full
#   # e.g. we computed out of sample forecasts and not all
#   # true values are available/known.
#   
#   no_mod <- dim(result2)[2]/2 # no. of forc. horizons
#   no_obs = dim(result2)[1]
#   # number of rows of result2
#   
#   amse = rep(NA,no_mod) # absolute mse
#   amae = rep(NA,no_mod) # absolute mae
#   
#   
#   j = 1; 
#   while(j <= no_mod){
#     result3 = result2[1:(no_obs-forchor[j]+1),(2*j-1):(2*j)]
#     # j = 1  => use first h : h = 1
#     # => slice result2[1 bis no_obs - 1 + 1, 2*1-1 = 1 : 2*1 = 2] 
#     # i.e. use all rows (since no zeros) and first and second column 
#     # # j = 2  => use 2nd h : h = 5
#     # => slice result2[1 bis no_obs - 5 + 1, 2*2-1 = 3 : 2*1 = 4]
#     # i.e. don't use last 4 rows, since h > 1 not available anymore from last row onwards (since no zeros) and first
#     # and use  second column
#     # ...
#     amse[j] = (sum((result3[,1] - result3[,2])^2))/dim(result3)[1] # compute amse
#     amae[j] = (sum(abs(result3[,1] - result3[,2])))/dim(result3)[1] # compute amae
#     j = j + 1; 
#   }
#   return(list(mse=amse, mae =amae)) 
# }




# Hodrick-Prescott Filter 
##############################################################################################
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


