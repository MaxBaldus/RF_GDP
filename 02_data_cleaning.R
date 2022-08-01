# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/


inspect = function(df){
  View(df)
  # dataframe structure
  print("Dataframe structure: ")
  print(str(df)) 
  
  # NA's
  print("These columns contain NA's: ")
  print(str(df[, colSums(is.na(df)) > 0])) 
  
  # varlist_df = colnames(df) 
  # return(varlist_df)
}



clean = function(df){
  
  gdp_raw = df[-(1:2),2] # neglect first 2 rows of gdp
  
  # get the dates
  dates = as.Date(df[-(1:2),1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  df[-(1:2),1] = dates # insert dates as 1st column
  
  # handle NA's
  # replace NA entries with the median of the ts of the variable (use median to respect outliers)
  for (i in 2:dim(df)[2]) {
    df[,i] = ifelse(is.na(df[,i]), median(df[,i], na.rm = TRUE), df[,i])
    # for each column beginning with the 2nd column
    # for each NA entry => replace with median, otherwise don't replace the entry 
  }
  
  # Transformation
  # transform each variable according to the entry of first row the variable (i.e. its transform)
  
  # 7) discrete returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) {     
      df[-(1:2),v] = ( df[-(1:3),v] / df[-(1:2)- 1,v]) - 1 
    }
  }
  # 6) squared returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 6) { 
      df[-(1:3),v] = (diff(log(df[-c(1,2),v])))^2 
    }
  }
  # 5) log differences (growth rate or returns)
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 5) { 
      df[-(1:3),v] = diff(log(df[-c(1,2),v])) # apply function with differences starting with third row: loosing first observation
    }
  }
  # 4) logs (log transformation)
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 4) { 
      df[-(1:2),v] = log(df[-c(1,2),v])
    }
  }
  # 3) squared differences  
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 3) { 
      df[-(1:3),v] = (diff(df[-c(1,2),v]))^2 
    }
  }
  # 2) differences  
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 2) { 
      df[-(1:3),v] = diff(df[-c(1,2),v]) 
    }
  }
  
  df = df[-(1:3),] # delete first 2 rows after computations
  
  
  print("Transformend dataframe: ")
  print(str(df))
  
  df_trans = df
  View(df_trans)
  
  l = list(dates = dates, df_transformend = df, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}

# create in-sample dataframe
in_out_sample = function(df, gdp){
  
  # training data (in-sample) and test data (out-of-sample) for estimation via rolling window
  # in-sample contains observations from "1959-03-01" to "2017-12-01"
  # => forecasting 5 years (i.e. 4 quarter per year => 20 quartes)
  
  N = length(df[,1]) # length of dataframe
  Nin = N - 20 # length of in sample dataframe
  
  df_in = df[1:Nin,] # slice df
  View(df_in)
  
  gdp_raw_in = gdp[1:Nin]
  
  return(list(insample_dataframe = df_in, gdp_raw_in = gdp_raw_in))
  
  # windows function 
}




