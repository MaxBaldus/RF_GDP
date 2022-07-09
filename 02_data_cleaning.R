# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/


inspect = function(df){
  varlist_df = colnames(df) # column names of the dataframe
  
  print("Dataframe structure", str(df)) # dataframe structure
  print(head(df)) # head of dataframe
  
  print("These columns contain NA's", str(df[, colSums(is.na(df)) > 0])) # which columns contain NA's
  
  print("Variable Names")
  return(varlist_df)
}



clean = function(df){
  
  gdp_raw = df[-(1:2),2] # neglect first 2 rows of gdp
  
  # get the dates
  dates = as.Date(df[-(1:2),1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  df[-(1:2),1] = dates # insert dates as 1st column
  
  # handle NA's
  # -> don't want to loose rows: - if Na in column => fill NA's entry with mean/median ?? -> use median to respect outliers
  for (i in 2:dim(df)[2]) {
    df[,i] = ifelse(is.na(df[,i]), median(df[,i], na.rm = TRUE), df[,i])
    # for each column beginning with the 2nd column
    # for each NA entry => replace with median, otherwise don't replace the entry 
  }
  
  # transform each variable according to the entry of first row the variable
  # 7) discrete returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) {      # neglecting first 2 rows & starting with fourth entry => x + 2
      df[-(1:2),v] = ( df[-(1:3),v] / df[-(1:2)- 1,v]) -1 # strt with 4th entry and 
    }
  }
  
  # 6) squared returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 6) { 
      df[-(1:3),v] = (diff(log(df[-c(1,2),v])))^2 # squared returns
    }
  }
  
  # 5) - log differences (growth rate / return)
  for (v in 2:dim(df)[2]) {  # 2:247 - going over each entry
    if (df[2,v] == 5) { # if entry is 5 => apply log differences
      df[-(1:3),v] = diff(log(df[-c(1,2),v])) # apply function with differences starting with third row: loosing first observation
    }
  }
  
  # 4) logs 
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 4) { 
      df[-(1:2),v] = log(df[-c(1,2),v])# log transformation
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
  
  df = df[-(1:2),] # delete first 2 rows after computations
  
  # training data / data for estimation via rolling window
  # "1959-03-01" - "2017-12-01"
  # => forecasting 5 years 
  
  # windows function 
  
  
  
  print(str(df))
  print(head(df))
  
  l = list(dates = dates, df_clean = df, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}
