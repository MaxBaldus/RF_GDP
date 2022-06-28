# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/


inspect = function(df){
  # all helper function for inspection in here .. 
}

clean = function(df){
  
  df_raw = df
  gdp_raw = df_raw[-(1:2),2] # neglect first 2 rows of gdp
  
  # handle NA's
  
  # if Na in column => fill NA's entry with mean/mode ?? -> use median to respect outliers
  
  # make each variable in the stationary, according to the entry of first row the variable: transform accordingly
  # 5 - log differences 
  for (v in 2:dim(df)[2]) {  # 2:247
    if (df[2,v] == 5) { # if entry is 5.000 => apply log differences
      df[-(1:3),v] = diff(log(df[-c(1,2),v])) # apply function with differences starting with third row: loosing first observation
    }
  }
  
  # 4 - next number 
  
  # get the dates seperately
  dates = as.Date(df[-(1:2),1], format = "%m/%d/%Y") # character vector with dates: convert to date class (neglecting first 2 rows)
  
  # training data / data for estimation 
  # "1959-03-01" - "2017-12-01"
  # => forecasting 5 years 
  
  print("Structure of Raw Dataframe" + str(df))
  print("First rows of Raw Dataframe" + head(df))
  
  l = list(dates = dates, gdp_raw = gdp_raw, df_clean = df) # put everything into list I want to return
  return(l)
}
