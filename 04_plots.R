# quick and dirty plotting
gdp_plot = function(gdp, title, ylab){
  
  # xts package
  # gdp = ts(data$gdp_raw, start = data$dates[1], frequency = 4) # ts does not support date object 
  # if you want the character form of date in a time series object, you should consider using xts 
  # use xts package for a nicer plot
  
  gdp_ts = ts(gdp, start = c(1959,1), frequency = 4) 
  #start parameter: first number is year of occurrence, 2nd number is first incident
  #(second number is the first incident in that period since not all series begin at January or at Sunday)
  
  ts.plot(gdp_ts, main = title ,xlab="Time", ylab=ylab, type="l") # plot ts object
  
}

# plotting forecasts with confidence bounds







# define ggplot environment => do plots again .. nice for paper .. 