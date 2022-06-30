# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/


inspect = function(df){
  varlist_df = colnames(df) # column names of the dataframe
  
  # Varlist Fred
  # varlist_fred = fredqd_description # description of the quartely Fred data set
  # length(varlist$fred) - 248
  
  print("Dataframe structure", str(df)) # dataframe structure
  print(head(df)) # head of dataframe
  
  print("These columns contain NA's", str(df[, colSums(is.na(df)) > 0])) # which columns contain NA's
  
  print("Variable Names")
  return(varlist_df)
}



clean = function(df){
  
  gdp_raw = df_raw[-(1:2),2] # neglect first 2 rows of gdp
  
  # handle NA's
  # How to deal with NA entries??
  # e.g. df[,"TLBSNNBBDIx"] - last entry missing 
  # -> don't want to loose rows: Replace Using Mean, Median, or Mode ??
  # if Na in column => fill NA's entry with mean/mode ?? -> use median to respect outliers
  
  
  
  # transform each variable according to the entry of first row the variable
  # 7) discrete returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) { 
      for (x in dim(df)[1]) { # neglecting first 2 rows & starting with fourth entry => x + 2
        df[x+3,v] = diff( (df[x+3,v] / df[x+2,v]) -1)
      }
    }
  }
  
  
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) {      # neglecting first 2 rows & starting with fourth entry => x + 2
      df[-(1:3),v] = ( df[-(1:3),v] / df[-(1:2)- 1,v]) -1 # strt with 4th entry and 
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
      df[-(1:2),v] = diff(log(df[-c(1,2),v])) # log transformation
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
  
  # drop first 2 rows
  df = df[-c(1,2),]
  
  # get the dates seperately
  dates = as.Date(df[-1,1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  # neglecting first row due to differencing
  
  df[,1] = dates # append dates to dataframe
  
  
  # training data / data for estimation 
  # "1959-03-01" - "2017-12-01"
  # => forecasting 5 years 
  
  print(str(df))
  print(head(df))
  
  l = list(dates = dates, gdp_raw = gdp_raw, df_clean = df) # put everything into list I want to return
  return(l)
}
