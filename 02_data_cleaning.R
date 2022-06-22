# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/

inspect = function(df){
  
  df_raw = df
  ind = df_raw[2,]
  
  # make each variable stationary, according to the entry of first row the variable: transform accordingly
  
  # 5 - log differences 
  for (v in 1:length(dim(df)[2])) {  # 
    if (df[2,v] == 5.000) { # if entry is 5.000 => apply 
      df[,v] = diff(log(df[,v])) # apply function with differences 
    }
  }
  
  
  df_clean = df[-(1:2),]
  dates = as.Date(df[-(1:2),1], format = "%m/%d/%Y") # character vector with dates: convert to date class (neglecting first 2 rows)
  gdp = df_clean[,2] # neglect first 2 rows
  
  # output raw dataframe
  print(dim(df_raw))
  print(str(df_raw))
  print(head(df_raw))
  
  # output stationary df
  print(df)
  
  
  l = list(dates = dates, gdp = gdp, df = df_clean) # put everything into list I want to return
  return(l)
}
