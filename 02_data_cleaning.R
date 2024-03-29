# functions for the mccracken dataframe
##############################################################################################
# data from https://research.stlouisfed.org/econ/mccracken/fred-databases/
inspect_mc = function(df){
  # View(df)
  # dataframe structure
  print("Dataframe structure: ")
  print(str(df)) 
  
  # NA's
  print("These columns contain NA's: ")
  print(str(df[, colSums(is.na(df)) > 0])) 
  
  # varlist_df = colnames(df) 
  # return(varlist_df)
}

clean_mc = function(df){
  
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
  
  # View(df_trans)
  
  l = list(dates = dates, df_trans = df_trans, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}

# create in-sample dataframe
in_out_sample = function(df, h_max){
  # training data (in-sample) and test data (out-of-sample) for recursive estimation
  # in-sample contains observations from "1959-03-01" up to (not including) "2000-01-01"
  # => forecasting h = 0,...,4 quarters for each year up to 2022-01-01 (last quarter 2021) 
  # i.e. 4 quarter per year => 4 * 22 = 88 quarters to forecast recursively
  
  N = length(df[,1]) # length of dataframe
  Nin = N - h_max # length of in sample dataframe
  df_in = df[1:Nin,] 
  return(df_in)
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
  
  # View(df_trans)
    
  l = list(dates = dates, df_trans = df_trans, gdp_raw = gdp_raw) # put everything into list I want to return
  return(l)
}


##############################################################################################
# df by Mr. Carstensen
##############################################################################################
create_df = function(df, gdp){
  data1 = as.data.frame(matrix(0, nrow = nrow(df)-10, ncol = ncol(df))) # initialize empty matrix
  data1[,1] = openxlsx::convertToDate(as.matrix(df[-(1:10),1])) # date column
  data1[,-1] = apply(as.matrix(df)[-(1:10),-1], 2, as.numeric) # convert characters to numbers
  data1[,-1] = apply(data1[,-1], 2, function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x)}) # replace NA's with median
  # column names
  data2 = data1[,-1] # df without date column
  colnames(data2) = as.vector(as.matrix(df)[4,-1])
  data1 = cbind(data1[,1], data2)
  names(data1)[names(data1) == "data1[, 1]"] = "dates"
  # aggregate monthly data to quarterly data using mean 
  data3 = aggregate(data1[,-1], list(as.yearqtr(data1$dates)), mean)
  # stationary regressors
  data4 = apply(data3[,-1], 2, diff) # computing the first difference, loosing first observation
  data4 = as.matrix(cbind(data3[-1,1], data4))
  # gdp
  gdp_data = as.data.frame(gdp[-(1:6),])
  gdp_data$num = as.numeric(gdp_data[,2])
  GDP_GR = diff(log(gdp_data$num)) # compute growth rate
  gdp_data1 = cbind(gdp_data[-1,],GDP_GR)
  # combine dataframes
  gdp_data2 = gdp_data1[which(gdp_data1$GDPC1=="04.01.1959"):which(gdp_data1$GDPC1=="04.01.2022"),c(1,3,4)]
  data = cbind(data4[,1], gdp_data2[,2:3], data4[,-1])
  names(data)[names(data) == "data4[, 1]"] = "dates"
  names(data)[names(data) == "num"] = "GDPC1"
  data = as.data.frame(data[1:which(data[,1] == 2021.75),])
  return(data)
}
##############################################################################################
# using updated GDP data
##############################################################################################
# groups:
# Index: 1:20 = REAL ACTIVITY ; 21:47 = EMPLOYMENT ; 48:60 = HOUSING ; 
# 61:75 = INTEREST RATE ; 76:104 = INFLATION ; 105:113 = FINANCIAL MARKET ;
# 114:133 = MONEY ; 134:144 = CREDIT ; 145 = OILPRICE ; 146 = FFR
create_df_2 = function(df, gdp){
  data1 = as.data.frame(matrix(0, nrow = nrow(df)-10, ncol = ncol(df))) # initialize empty matrix
  data1[,1] = openxlsx::convertToDate(as.matrix(df[-(1:10),1])) # date column
  data1[,-1] = apply(as.matrix(df)[-(1:10),-1], 2, as.numeric) # convert characters to numbers
  # check how many variables containing NA's
  id = c()
  for (i in (2:ncol(data1))) {
    if(is.na(data1[1,i]) == TRUE){
      id = append(id, i, after = length(id))
    }
  }
  print(paste0(length(id), " variables contain NA's"))
  # replace NA's with median
  data1[,-1] = apply(data1[,-1], 2, function(x) {ifelse(is.na(x), median(x, na.rm = TRUE), x)}) 
  # column names
  data2 = data1[,-1] # df without date column
  colnames(data2) = as.vector(as.matrix(df)[4,-1])
  data1 = cbind(data1[,1], data2)
  names(data1)[names(data1) == "data1[, 1]"] = "dates"
  # aggregate monthly data to quarterly data using mean 
  data3 = aggregate(data1[,-1], list(as.yearqtr(data1$dates)), mean)
  # stationary regressors
  data4 = apply(data3[,-1], 2, diff) # computing the first difference, loosing first observation
  # check each ts with augmented df test, if 1st diff. filter yielded stationarity
  data5 = data4
  l = apply(data4, 2, tseries::adf.test)
  ind = c()
  for (i in (1:length(l))) {
    current_ts = l[i]
    # if stationarity not obtained (i.e. p-value > 0.05 <=> H0 not rejected), 
    # first difference filter is applied again 
    p_value = as.numeric(current_ts[[1]]$p.value) 
    if (p_value >= 0.05) {
      data5[-1,i] = diff(data4[,i])
      ind = append(ind, i, after = length(ind))
    } 
  }
  # print(paste0(colnames(data4)[ind], "were not stationary"))
  print(paste0(length(ind), " series were not stationary after 1st diff. filter"))
  data5 = data5[-1,] # loosing 2nd observation due two 2 filters
  # test stationarity again
  l = apply(data5, 2, tseries::adf.test)
  for (i in (1:length(l))) {
    current_ts = l[i]
    p_value = as.numeric(current_ts[[1]]$p.value)
    if (p_value >= 0.05) {
      print(paste0(colnames(data4)[i], "  still not stationary after 2st diff. filter"))
    } 
  }
  print("using returns and squared returns for the three")
  # ts which are still not stationary using transformation suggested by mccracken
  data5[, "RPI"] = diff(log(data3[-1,"RPI"])) # log differences (growth rate or returns)
  data5[, "CUSR0000SAS"] = (diff(log(data3[-1, "CUSR0000SAS"])))^2 # squared returns 
  data5[, "NONREVSL"] = diff(log(data3[-1,"NONREVSL"])) # log differences (growth rate or returns)
  # include date column
  data5 = as.matrix(cbind(data3[-c(1,2),1], data5))
  # gdp
  gdp_data = as.data.frame(gdp[-(1:6),])
  gdp_data$num = as.numeric(gdp_data[,2])
  GDP_GR = diff(log(gdp_data$num)) # compute growth rate
  gdp_data1 = cbind(gdp_data[-1,],GDP_GR)
  # combine dataframes
  gdp_data2 = gdp_data1[which(gdp_data1$GDPC1=="04.01.1959"):which(gdp_data1$GDPC1=="04.01.2022"),c(1,3,4)]
  data = cbind(data5[,1], gdp_data2[-1,2:3], data5[,-1])
  # rename some columns 
  names(data)[names(data) == "data5[, 1]"] = "dates"
  names(data)[names(data) == "num"] = "GDPC1"
  # cut of dataframe at last quarter of 2021
  data = as.data.frame(data[1:which(data[,1] == 2021.75),])
  return(data)
}