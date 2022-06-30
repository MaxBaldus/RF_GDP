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
  
  gdp_raw = df[-(1:2),2] # neglect first 2 rows of gdp
  
  df_clean = as.data.frame(matrix(0, nrow = dim(df)[1]-3, ncol = dim(df[2]))) # create empty dataframe
  
  # don't have the colnames in df_clean !!!
  
  # get the dates
  dates = as.Date(df[-(1:2),1], format = "%m/%d/%Y") # character vector with dates: convert to date class 
  df_clean[,1] = dates[-1] # append dates to dataframe: leaving out first date since lost due to transformation
  
  # handle NA's
  # How to deal with NA entries??
  # e.g. df[,"TLBSNNBBDIx"] - last entry missing 
  # -> don't want to loose rows: Replace Using Mean, Median, or Mode ??
  # if Na in column => fill NA's entry with mean/mode ?? -> use median to respect outliers
  
  
  
  # transform each variable according to the entry of first row the variable
  # 7) discrete returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) { 
      df_clean[,v] = (df[-(1:3),v] / df[-(1:2),v]) -1
    }
  }
  
  
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 7) {      # neglecting first 2 rows & starting with fourth entry => x + 2
      df_clean[,v] = ( df[-(1:3),v] / df[-(1:2)- 1,v]) -1 # strt with 4th entry and 
    }
  }
  
  
  # 6) squared returns
  for (v in 2:dim(df)[2]) { 
    if (df[2,v] == 6) { 
      df_clean[,v] = (diff(log(df[-c(1,2),v])))^2 # squared returns
    }
  }
  
  # 5) - log differences (growth rate / return)
  for (v in 2:dim(df)[2]) {  # 2:247 - going over each entry
    if (df[2,v] == 5) { # if entry is 5 => apply log differences
      df_clean[,v] = diff(log(df[-c(1,2),v])) # apply function with differences starting with third row: loosing first observation
    }
  }
  
  # 4) logs 
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 4) { 
      df_clean[,v] = diff(log(df[-c(1,2),v])) # log transformation
    }
  }
  
  # 3) squared differences  
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 3) { 
      df_clean[,v] = (diff(df[-c(1,2),v]))^2 
    }
  }
  
  # 2) differences  
  for (v in 2:dim(df)[2]) {  
    if (df[2,v] == 2) { 
      df_clean[,v] = diff(df[-c(1,2),v]) 
    }
  }
  
  # training data / data for estimation 
  # "1959-03-01" - "2017-12-01"
  # => forecasting 5 years 
  
  print(str(df_clean))
  print(head(df_clean))
  
  l = list(dates = dates, df_clean = df_clean, gdp_raw = gdp_raw ) # put everything into list I want to return
  return(l)
}
