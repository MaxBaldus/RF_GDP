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
  
  # get gdp time series explicitely
  gdp_raw = df[-(1:2),2] # neglect first 2 rows of gdp
  
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
  
  df = df[-(1:3),] # delete first 3 rows after computations
  
  # date class
  dates = as.Date(df[,1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  df[,1] = dates # insert dates as 1st column
  
  # only use values up to first quarter of 2022 (i.e. assuming no values from 2022 onwards)
  df_trans = df[-(dim(df)[1]),]
  
  # insert raw gdp again (as third column)
  df_trans = cbind(df_trans[,1:2], gdp_raw[2:(length(gdp_raw)-1)], df_trans[3:ncol(df_trans)])
  
  # rename GDP and GDP growth column 
  names(df_trans)[2] = "GDP_GR" 
  names(df_trans)[3] = colnames(df)[2]
  
  print("Transformend dataframe: ")
  print(str(df_trans))
  
  View(df_trans)
  
  l = list(dates = dates, df_trans = df_trans, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}

# create in-sample dataframe
in_out_sample = function(df, gdp, h_max){
  
  # training data (in-sample) and test data (out-of-sample) for estimation via rolling window
  # in-sample contains observations from "1959-03-01" up to (not including) "2000-01-01"
  # => forecasting h = 0,...,4 quarters for each year up to 2022-01-01 (last quarter 2021) 
  # i.e. 4 quarter per year => 4 * 22 = 88 quarters to forecast recursively
  
  N = length(df[,1]) # length of dataframe
  Nin = N - h_max # length of in sample dataframe
  
  df_in = df[1:Nin,] 
  # df_out = df[Nin:N,] 
  View(df_in)
  
  gdp_raw_in = gdp[1:Nin] # slice raw gdp 
  
  return(list(insample_dataframe = df_in, gdp_raw_in = gdp_raw_in))
}

clean_2 = function(df){
  # get gdp time series explicitely
  gdp_raw = df[-(1:2),2] # neglect first 2 rows of gdp
  
  # handle NA's
  # replace NA entries with the median of the ts of the variable (use median to respect outliers)
  for (i in 2:dim(df)[2]) {
    df[,i] = ifelse(is.na(df[,i]), median(df[,i], na.rm = TRUE), df[,i])
    # for each column beginning with the 2nd column
    # for each NA entry => replace with median, otherwise don't replace the entry 
  }
  
  df = df[-(1:2),] 
    
  # date class
  dates = as.Date(df[,1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  df[,1] = dates # insert dates as 1st column
    
  # only use values up to first quarter of 2022 (i.e. assuming no values from 2022 onwards)
  df_trans = df[-(dim(df)[1]),]    
  print("Transformend dataframe: ")
  print(str(df_trans))
  
  View(df_trans)
    
  l = list(dates = dates, df_trans = df_trans, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}


