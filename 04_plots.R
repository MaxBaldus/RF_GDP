# quick and dirty gdp plotting
gdp_plot = function(gdp, title, ylab, CI){
  
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
gdp_forecast_plot = function(gdp, gdp_forecast, se, title, ylab, col, CI){
  
  # ts objects
  gdp_ts = ts(gdp, start = c(1959,1), frequency = 4) 
  gdp_forecast_ts = ts(gdp_forecast, start = c(2000,1),end = c(2022,1), frequency = 4)
  gdp_forecast_se_ts = ts(se, start = c(2000,1),end = c(2022,1), frequency = 4)
  
  ts.plot(gdp_ts, main = title ,xlab="Time", ylab=ylab, type="l") # plot ts object
  lines(gdp_forecast_ts, col = col)
  # include 95% CI if CI == TRUE
  if (CI == TRUE) {
    lines(gdp_forecast_ts + 1.96*sd(gdp_forecast_ts),lwd=2, col = "orange", lty = 3)
    lines(gdp_forecast_ts - 1.96*sd(gdp_forecast_ts),lwd=2, col = "orange", lty = 3)
    # legend
    legend("bottomleft", legend = c("Model prediction", "95% Confidence intervall"), 
           col = c(col, "orange"), lty = 1, cex = 0.5)
  }
  # zoom in: starting with first quarter in 2000 (via window function)
  # => 22 years * 4 quarters + 1 = 89
  ts.plot(window(gdp_ts, start =c(1995,1), end = c(2022,1)), main = c(title, "zoom") ,xlab="Time", ylab=ylab, type="l"#,
          # ylim = c(8000,23000)
          ) 
  lines(gdp_forecast_ts, col = col)
  # include 95% CI if CI == TRUE
  if (CI == TRUE) {
    lines(gdp_forecast_ts + 1.96*sd(gdp_forecast_ts),lwd=2, col = "orange", lty = 3)
    lines(gdp_forecast_ts - 1.96*sd(gdp_forecast_ts),lwd=2, col = "orange", lty = 3)
    # legend
    legend("topleft", legend = c("Model prediction", "95% Confidence intervall"),
           col = c(col, "orange"), lty = 1, cex = 0.5)
  }
}

gdp_growth_forecast_plot = function(gdp, gdp_forecast, se, title, ylab, col, CI){
  
  # ts objects
  gdp_ts = ts(gdp, start = c(1959,1) ,end = c(2022,1), frequency = 4)
  gdp_forecast_ts = ts(gdp_forecast, start = c(2000,1),end = c(2022,1), frequency = 4)
  gdp_forecast_se_ts = ts(se, start = c(2000,1),end = c(2022,1), frequency = 4)
  # gdp_forecast_ts = gdp_forecast
  # gdp_forecast_se_ts = se
  # 
  
  # plot
  ts.plot(gdp_ts, main = title, xlab="Time", ylab=ylab, type="l") # plot ts object
  lines(gdp_forecast_ts, col = col)
  if (CI == TRUE) {
    lines(gdp_forecast_ts + 1.96*gdp_forecast_se_ts, col = "orange", lty = 3)
    lines(gdp_forecast_ts - 1.96*gdp_forecast_se_ts,lwd=2, col = "orange", lty = 3) 
    # legend
    legend("bottomleft", legend = c("Model prediction", "95% Confidence intervall"), 
           col = c(col, "orange"), lty = 1, cex = 0.5)
  }
  
  
  # zoom in
  ts.plot(window(gdp_ts, start =c(1995,1), end = c(2022,1)), main = c(title, "zoom") ,xlab="Time", ylab=ylab, type="l") 
  lines(gdp_forecast_ts, col = col)
  if (CI == TRUE) {
    lines(gdp_forecast_ts + 1.96*gdp_forecast_se_ts,lwd=2, col = "orange", lty = 3)
    lines(gdp_forecast_ts - 1.96*gdp_forecast_se_ts,lwd=2, col = "orange", lty = 3)
    # legend
    legend("bottomleft", legend = c("Model prediction", "95% Confidence intervall"), 
           col = c(col, "orange"), lty = 1, cex = 0.5)
  }
}

# plotting errors rate using ggplot
ggplot_errors = function(df, colors){
  ggplot(data=plot.data, aes(x=ntrees, y=oob, color="OOB")) +
    geom_line(linetype="solid") +
    geom_line(aes(x=ntrees, y=test_error, color="Test Error"), linetype="solid") +
    geom_line(aes(x=ntrees, y=cv_error, color="CV Error"), linetype="solid") +
    # theme(axis.title = element_blank()) +
    # ggtitle("Error comparison") + 
    labs(x = "number of trees", y = "MSE", color = "Error Type") +
    scale_color_manual(values = colors) 
}






# define ggplot environment => do plots again .. nice for paper .. 