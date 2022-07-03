# define ggplot environemnt => do plots again .. nice for paper .. 

gdp_plot = function(gdp){
  
  # xts package
  # gdp = ts(data$gdp_raw, start = data$dates[1], frequency = 4) # ts does not support date object 
  # if you want the character form of date in a time series object, you should consider using xts 
  # use xts package for a nicer plot
  
  # ts.plot(gdp, xlab="Time", ylab="GDP", type="l") # plot ts object
  
}