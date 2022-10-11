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
  gdp_ts = ts(gdp, start = c(1959,2), end = c(2022,1) ,frequency = 4) 
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
  ts.plot(window(gdp_ts, start =c(1995,1), end = c(2021,4)), main = c(title, "zoom") ,xlab="Time", ylab=ylab, type="l"#,
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

# variable importance plot 
imp_plot = function(varImpobject){
  
  imp = as.data.frame(varImpobject)
  imp$varnames = rownames(imp) # row names into column
  rownames(imp) = NULL # delete rownames  
  # create numbers for the each index of the variables
  imp$var_categ = c(rep(1, 20), rep(2, 27), rep(3, 13), rep(4, 15),
                    rep(5,29), rep(6,9), rep(7,20), rep(8,11), 9, 10)
  # add up the percentage increase in MSE for each group

  imp_groups = as.data.frame(matrix(NA, nrow = 10, ncol = 2))
  counter = 1
  slice_0 = 1
  slice_1 = 20
  for (i in c(27,13,15,29,9,20,11,1,1)) {
    imp_groups[counter,1] = sum(imp$IncNodePurity[slice_0:slice_1])
    counter = counter + 1
    slice_0 = slice_1 + 1
    slice_1 = slice_1 + i
  }
  imp_groups[10,1] = imp$IncNodePurity[length(imp$IncNodePurity)] # value of last group
  
  imp_groups[,1] * 100 # scaling for better readability
  imp_groups[,2] = seq(1,10,1) # assign number of each group
  imp_groups$varnames = c("REAL ACTIVITY", "EMPLOYMENT", "HOUSING", "INTEREST RATE",
                          "INFLATION", "FINANCIAL MARKET", "MONEY", "CREDIT", "OILPRICE",
                          "FFR") # goup names
  colnames(imp_groups) = c("IncNodePurity", "var_categ", "varnames") # new column names
  # ggplot
  ggplot(imp_groups, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity, color=as.factor(var_categ))) + 
    geom_point() +
    geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
    scale_color_discrete(name="Variable Group") +
    ylab("IncNodePurity") +
    xlab("Series ID") +
    coord_flip()
}

# forecast plots
final_forecast_plot = function(df, gdp, arma, rf_nonTunend, 
                               rf_Tunend, rf_lag, rf_ts, h, title,
                               horizon, y_name_GDP, colors, colorname){
  slice_df = 1 + (2*h) # column index
  
  # create time series objects of all forecasts and gdp
  gdp_slice =  ts(gdp[which(df$dates == 2000.00):(length(gdp))], start = c(2000,1),end = c(2021,4), frequency = 4)
  arma_slice = ts(arma[1:(nrow(arma)-h),slice_df], start = c(2000,1),end = c(2021,4), frequency = 4)
  rf_nonTunend_slice = ts(rf_nonTunend[,slice_df], start = c(2000,1),end = c(2021,4), frequency = 4)
  rf_Tunend_slice = ts(rf_Tunend[,slice_df], start = c(2000,1),end = c(2021,4), frequency = 4)
  rf_lag_slice = ts(rf_lag[,slice_df], start = c(2000,1),end = c(2021,4), frequency = 4)
  rf_ts_slice = ts(rf_ts[,slice_df], start = c(2000,1),end = c(2021,4), frequency = 4)
  # non-ts
  # gdp_slice = gdp[which(df$dates == 2000.00):(length(gdp))]
  # arma_slice = arma[1:(nrow(arma)-h),slice_df]
  # rf_nonTunend_slice = rf_nonTunend[,slice_df]
  # rf_Tunend_slice = rf_Tunend[,slice_df]
  # rf_lag_slice = rf_lag[,slice_df]
  # rf_ts_slice = rf_ts[,slice_df]
  # get the dates
  quarters_ts = time(gdp_slice)
  plot_data = as.data.frame(cbind(quarters_ts, gdp_slice, arma_slice, rf_nonTunend_slice,
                                  rf_Tunend_slice, rf_lag_slice, rf_ts_slice))
  
  # ggplot
  ggplot(data=plot_data, aes(x=quarters_ts, y=gdp_slice, color = colorname)) +
    geom_line(linetype="solid") + 
    geom_line(aes(x=quarters_ts, y=arma_slice, color="ARMA"), linetype="solid") + 
    geom_line(aes(x=quarters_ts, y=rf_nonTunend_slice, color="RF-nonTuned"), linetype="solid") +  
    geom_line(aes(x=quarters_ts, y=rf_Tunend_slice, color="RF-Tuned"), linetype="solid") + 
    geom_line(aes(x=quarters_ts, y=rf_lag_slice, color="RF-Lags"), linetype="solid") + 
    geom_line(aes(x=quarters_ts, y=rf_ts_slice, color="RF-tsBootstrapping"), linetype="solid") + 
    labs(x = "Time", y = y_name_GDP, color = horizon) +
    # xlab("Time") + ylab(y_name_GDP) + labs(color = horizon) +
    # theme(axis.title = element_blank()) +
    ggtitle(title) +
    scale_color_manual(values = colors) 

  }