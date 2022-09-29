# plain rf using OOB 
rf_plain = function(X, gdp , oos_dataframe, ntrees, mtry, Fstdf, xi){
  # using first difference:
  if (Fstdf == TRUE) {
    gdp_d = diff(gdp) # 1st differencing ts
    gdp_ct = gdp_d - mean(gdp_d) # centering ts
    gdp = gdp_ct 
    # loosing one observation: hence also need to delete first observation in regressor data
    X = X[-1,]
  }
  # train random forest on in_sample_dataframe
  rand_forest = randomForest::randomForest(x = X,
                                           y = gdp,  
                                           # data = in_sample_dataframe: needed when ~.formular is used
                                           mtry = mtry,
                                           importance = TRUE,
                                           ntrees = ntrees)
  # prediction
  rand_forest_pred = predict(rand_forest, oos_dataframe) 
  
  if (Fstdf == TRUE) {
    rand_forest_pred = diffinv(rand_forest_pred + mean(gdp_d), xi = xi)[-1]
  }
  
  return(list(forest = rand_forest, plain_forest_pred = rand_forest_pred))
  
}

##############################################################################################
# plain random forest using validation set
##############################################################################################
rf_plain_valid = function(X_train, y_train, X_test, y_test, ntrees, mtry){
  # train random forest on in_sample_dataframe
  rand_forest = randomForest::randomForest(x = X_train,
                                           y = y_train,
                                           # data = X_train, needed when ~.formular is used
                                           xtest = X_test,
                                           ytest = y_test,
                                           mtry = mtry,
                                           importance = TRUE,
                                           ntrees = ntrees)
  # plot(rand_forest)
  # prediction
  # rand_forest_pred = predict(rand_forest2, oos_dataframe) 
  return(rand_forest)
}
##############################################################################################
# forecasting GDP GROWTH with rf using rolling window with a-priori specification
# i.e. refitting rf, but always with the same hyper parameters
##############################################################################################
rf_plain_rolling = function(df, gdp, ntrees, mtry, forh) {
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 2000.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))

  # initializing
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh)+2)
  
  # loop over each quarter from 2000 up to 2022,
  # doing a direct oos forecast for each horizon h
  col_counter = 1
  for (h in c(0, forh)) {
    print(paste0("h=", h))
    X = df[1:(nrow(df)-h),-2]  # y_t+h = f(y_t,X_t), excluding GDPCR
    y = df[(1+h):nrow(df),c(1,3)] # GDP target 
    for (i in Nin:(N-h)) {
      # estimate rf again each quarter using new model (but same hyper parameters)
      # for each iteration: have a new dataframe
      X_train = X[1:(i-h),]
      y_train = y[1:(i-h),]
      # train rf using values up to current window 
      rand_forest = ranger::ranger(x = X_train[,-1],
                                   y = y_train[,-1],
                                   mtry = mtry,
                                   importance = "none",
                                   num.trees = ntrees)
      # compute fitted value (of current Nin) & save
      p = rand_forest$predictions[length(rand_forest$predictions)] 
      result[i-Nin+1,col_counter] = p
      
    }
    col_counter = col_counter + 2
    print(result)
  }
  colnames(result) =  c("gdp forecast h=0", "gdp",
                        "gdp forecast h=1", "gdp", "gdp forecast h=2", "gdp",
                        "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  
  return(result)
}
##############################################################################################
# forecasting GDP (not growth) with rf using rolling window with a-priori specification
##############################################################################################
rf_rolling_GDP = function(df, gdp, ntrees, mtry, forh, xi) {
  # first differencing
  gdp_d = diff(gdp) # 1st differencing ts
  gdp_ct = gdp_d - mean(gdp_d) # centering ts
  # loosing one observation: hence also need to delete first observation in regressor data
  df = df[-1,]
  df$GDPC1 = gdp_ct # use differencend ts for training
  
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 2000.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  # initializing
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh)+2)
  
  # loop over each quarter from 2000 up to 2022,
  # doing a direct oos forecast for each horizon h
  col_counter = 1
  for (h in c(0, forh)) {
    print(paste0("h=", h))
    X = df[1:(nrow(df)-h),-3]  # y_t+h = f(y_t,X_t), excluding GDPCR
    y = cbind(df[(1+h):nrow(df),1] ,gdp_ct[(1+h):nrow(df)]) # GDP target 
    for (i in Nin:(N-h)) {
      # estimate rf again each quarter using new model (but same hyper parameters)
      # for each iteration: have a new dataframe
      X_train = X[1:(i-h),]
      y_train = y[1:(i-h),]
      # train rf using values up to current window 
      rand_forest = ranger::ranger(x = X_train[,-1],
                                   y = y_train[,-1],
                                   mtry = mtry,
                                   importance = "none",
                                   num.trees = ntrees)
      # compute fitted value (of current Nin) & save
      p = rand_forest$predictions[length(rand_forest$predictions)] 
      result[i-Nin+1,col_counter] = p
      
    }
    col_counter = col_counter + 2
    print(result)
  }
  # compute gdp values back (not differenced ts)
  for (i in (seq(1, ncol(result), 2))) { # for each 2nd column: compute inverse of diff. operation
    result[,i] =  diffinv(result[,i] + mean(gdp_d), xi = xi)[-1]
    # xi is the starting value of the differenced series (gdp) + adding mean again (non_centered)
  }
  # # compute gdp values back (not differenced ts)
  # for (i in (seq(1, ncol(result), 2))) { # for each 2nd column: compute inverse of diff. operation
  #   result[,i] =  diffinv(result[,i], xi = xi)[-1]
  #   # xi is the starting value of the differenced series (gdp) 
  # }
  colnames(result) =  c("gdp forecast h=0", "gdp",
                        "gdp forecast h=1", "gdp", "gdp forecast h=2", "gdp",
                        "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  
  return(result)
}

##############################################################################################
# forecasting GDP (not growth) with rf using hp filter 
##############################################################################################
rf_rolling_hp = function(df, gdp, ntrees, mtry, forh, hp) {

  gdp_ct = as.vector(gdp)
  df$GDPC1 = gdp_ct # use hp residuals
  
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 2000.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  # initializing
  result = matrix(0, nrow = N-Nin + 1, ncol = 2*length(forh)+2)
  
  # loop over each quarter from 2000 up to 2022,
  # doing a direct oos forecast for each horizon h
  col_counter = 1
  for (h in c(0, forh)) {
    print(paste0("h=", h))
    X = df[1:(nrow(df)-h),-3]  # y_t+h = f(y_t,X_t), excluding GDPCR
    y = cbind(df[(1+h):nrow(df),1] ,gdp_ct[(1+h):nrow(df)]) # GDP target 
    for (i in Nin:(N-h)) {
      # estimate rf again each quarter using new model (but same hyper parameters)
      # for each iteration: have a new dataframe
      X_train = X[1:(i-h),]
      y_train = y[1:(i-h),]
      # train rf using values up to current window 
      rand_forest = ranger::ranger(x = X_train[,-1],
                                   y = y_train[,-1],
                                   mtry = mtry,
                                   importance = "none",
                                   num.trees = ntrees)
      # compute fitted value (of current Nin) & save
      p = rand_forest$predictions[length(rand_forest$predictions)] 
      result[i-Nin+1,col_counter] = p
      
    }
    col_counter = col_counter + 2
    print(result)
  }
  # compute level GDP values back by adding extract values again
  for (i in (seq(1, ncol(result), 2))) { # for each 2nd column: compute inverse of diff. operation
    result[,i] =  result[,i] + 
                  hp$trend[which(df[,1] == 2000.00):length(hp$trend)] + 
                  hp$cycle[which(df[,1] == 2000.00):length(hp$cycle)]
  }
  colnames(result) =  c("gdp forecast h=0", "gdp",
                        "gdp forecast h=1", "gdp", "gdp forecast h=2", "gdp",
                        "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  
  return(result)
}

##############################################################################
# hyper parameter tuning with ranger package
##############################################################################

### 1a) oob for GDP GROWTH
rf_ranger_oob = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree, hyper_para_list){
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 1999.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  Nin_year = seq(from = Nin, to = N, by = 4) # starting at first quarter of each new year, from 1998 up to 2022
  # print(paste0("Nin_year =", Nin_year))
  
  counter_year = 1
  # always append next 4 quarters and compute oob error for each new year (for each hyper parameter combination)
  for (year in Nin_year) {
    current_df = df[1:(year-1),]
    print(paste0("Current df up to: ", df[(year),1]))
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          # set.seed(123) - setting the seed here always gives the same error result
          forest = ranger(formula = GDP_GR ~.,
                             data = current_df[,-c(1,2)], 
                             num.trees = ntree, 
                             mtry = mtry, sample.fraction = samp_size, min.node.size = node_size)
          
          # store error for current hyper parameter combination
          rf_acc[count,"Error"] = forest$prediction.error # extract oob error
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
        }
      }
    }
    # extract optimal hyper parameter for the current year and save it into the list
    hyper_para_list[[counter_year]] = rf_acc[which.min(rf_acc[,1]),]
    print(hyper_para_list)
    counter_year = counter_year + 1
  }
  return(hyper_para_list)
}
############################################################################## 
### 1b) oob for GDP 
rf_ranger_oob_level = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree, hyper_para_list,
                               gdp){
  # first differencing
  gdp_d = diff(gdp) # 1st differencing ts
  gdp_ct = gdp_d - mean(gdp_d) # centering ts
  # loosing one observation: hence also need to delete first observation in regressor data
  df = df[-1,]
  df$GDPC1 = gdp_ct # use differencend ts for training
  
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 1999.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  Nin_year = seq(from = Nin, to = N, by = 4) # starting at first quarter of each new year, from 1999 up to 2022
  # print(paste0("Nin_year =", Nin_year))
  
  counter_year = 1
  # always append next 4 quarters and compute oob error for each new year (for each hyper parameter combination)
  for (year in Nin_year) {
    current_df = df[1:(year-1),]
    print(paste0("Current df up to: ", df[(year),1]))
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          # set.seed(123) - setting the seed here always gives the same error result
          forest = ranger(formula = GDPC1 ~.,
                          data = current_df[,-c(1,3)], 
                          num.trees = ntree, 
                          mtry = mtry, sample.fraction = samp_size, min.node.size = node_size)
          
          # store error for current hyper parameter combination
          rf_acc[count,"Error"] = forest$prediction.error # extract oob error
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          count = count + 1
        }
      }
    }
    # extract optimal hyper parameter for the current year and save it into the list
    hyper_para_list[[counter_year]] = rf_acc[which.min(rf_acc[,1]),]
    print(hyper_para_list)
    counter_year = counter_year + 1
  }
  return(hyper_para_list)
}
##############################################################################
### 2a) using test-set: GDP GROWTH
rf_hyper_test_set = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree, hyper_para_list){
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 1999.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  Nin_year = seq(from = Nin, to = (N-4), by = 4) # starting at first quarter of each new year, from 2000 to 2022
  counter_year = 1
  # always append next 4 quarters and compute test error for each new year (for each hyper parameter combination)
  for (year in (Nin_year)) {
    current_train_df = df[1:(year-1),]
    print(paste0("Current df up to: ", df[(year),1]))
    current_test_df = df[(year):(year+3),] # using the next 4 quarters for testing 
    
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          
          rf_ranger = ranger(formula = GDP_GR ~.,
                    data = current_train_df[,-c(1,2)], 
                    num.trees = ntree, 
                    mtry = mtry, sample.fraction = samp_size, min.node.size = node_size)
          # prediction
          ranger_pred = predict(rf_ranger, data = current_test_df[,-c(1,3)])
          
          # store error for current hyper parameter combination
          # since always growing up to 500 trees, always use error when ntree = 500,
          # therefore making sure error stabilized  
          # browser()
          rf_acc[count,"Error"] = mean((current_test_df$GDP_GR - ranger_pred$predictions)^2) #mse
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
        }
      }
    }
    # extract optimal hyper parameter for the current year and save it into the list
    hyper_para_list[[counter_year]] = rf_acc[which.min(rf_acc[,1]),]
    print(hyper_para_list)
    
    # browser()
    counter_year = counter_year + 1
  }
  return(hyper_para_list)
}

### 2b) using test-set: GDP GROWTH
rf_hyper_test_set_level = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree, hyper_para_list,
                                   gdp){
  # first differencing
  gdp_d = diff(gdp) # 1st differencing ts
  gdp_ct = gdp_d - mean(gdp_d) # centering ts
  # loosing one observation: hence also need to delete first observation in regressor data
  df = df[-1,]
  df$GDPC1 = gdp_ct # use differencend ts for training
  
  N = length(df[,2]) # length of time series
  Nin = N - (N - which(df[,1] == 1999.00)) # length of in sample observations 
  print(paste0("N=", N))
  print(paste0("Nin=", Nin))
  
  Nin_year = seq(from = Nin, to = (N-4), by = 4) # starting at first quarter of each new year, from 2000 to 2022
  counter_year = 1
  # always append next 4 quarters and compute test error for each new year (for each hyper parameter combination)
  for (year in (Nin_year)) {
    current_train_df = df[1:(year-1),]
    print(paste0("Current df up to: ", df[(year),1]))
    current_test_df = df[(year):(year+3),] # using the next 4 quarters for testing 
    
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          rf_ranger = ranger(formula = GDPC1 ~.,
                             data = current_train_df[,-c(1,3)], 
                             num.trees = ntree, 
                             mtry = mtry, sample.fraction = samp_size, min.node.size = node_size)
          # prediction
          ranger_pred = predict(rf_ranger, data = current_test_df[,-c(1,3)])
          
          # store error for current hyper parameter combination
          # since always growing up to 500 trees, always use error when ntree = 500,
          # therefore making sure error stabilized  
          # browser()
          rf_acc[count,"Error"] = mean((current_test_df$GDPC1 - ranger_pred$predictions)^2) #mse
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
        }
      }
    }
    View(rf_acc)
    # extract optimal hyper parameter for the current year and save it into the list
    hyper_para_list[[counter_year]] = rf_acc[which.min(rf_acc[,1]),]
    print(hyper_para_list)
    
    counter_year = counter_year + 1
  }
  return(hyper_para_list)
}




########################################################################
### hc-cv ?????
rf_ranger_oob_prewindow = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree, h_max){
  N = nrow(df) # length of time series
  Nin = N - h_max # length of in sample observations (each quarter)
  Nin_year = seq(from = Nin+1, to = N, by = 1) # incrementing each quarter
  # using each new quarter from 1990 to 2000
  
  rf_hyper = data.frame(matrix(ncol = 4, nrow = 0)) 
  colnames(rf_hyper) = c("Error", "mtry","samp_size", "node_size")
  count_hyper = 1
  
  for (mtry in mtry_grid) {
    for (samp_size in samp_size_grid) {
      for (node_size in node_size_grid) {
        # initialize accuracy matrix for current hyperparameter combination
        rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
        colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
        count = 1 # counter for the rows
        
        # fit rf for current hyper parameter combination and evaluate, iteratively
        for (year in Nin_year) {
          current_df = df[1:year,]
          # browser()
          forest = ranger(formula = GDP_GR ~.,
                             data = current_df[,-c(1,3)], ntree = ntree, 
                             mtry = mtry, samp_frac = samp_size, node_size = node_size)
          # store error for current hyper param combination, for each quarter
          rf_acc[count,"Error"] = forest$prediction.error # extract overall oob error, i.e. mse
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
        }
        # compute cv error (average_error) for the current hyper parameter combination
        rf_hyper[count_hyper, "Error"] = mean(rf_acc[,1])
        rf_hyper[count_hyper, "mtry"] = mtry
        rf_hyper[count_hyper, "samp_size"] = samp_size
        rf_hyper[count_hyper, "node_size"] = node_size
        count_hyper = count_hyper + 1
        
        if (count_hyper == 10) {
          browser()
        }
        
        }
      }
    }
    return(rf_hyper)
}

#########################################################################
# again rf with rolling window, but now using optimal parameter specification for each year
rf_optparam_rolling = function(df, gdp, ntrees, h_max, forh,
                               hyper_oob_final, hyperset_prewindow){
                               # !!!!!!!!!! 
  N = length(df[,2]) # length of time series
  Nin = N - h_max # length of in sample observations
  print(paste0("N=", N))
  print(paste0("Nin=", N))
  
  
  # initializing
  zeros = rep(0, (N-Nin)*2*length(forh)) # first row of result matrix 
  result = matrix(zeros, nrow = N-Nin + 1, ncol = 2*length(forh))
  
  p = rep(0, length(forh)) # initialize vector to store predictions in
  # first entry: h = 0, then h = 1,..., 4
  h0 = matrix(0,  nrow = N-Nin + 1, ncol = 2) # initializing matrix for storing nowcast values
  
  # loop over each quarter from 2000 up to 2022
  # starting with 1999-12-01 (data$df_trans[163 = Nin,])
  
  # first set of hyperparams
  
  count = 1
  mtry = hyperset_prewindow$mtry
  samp_size = hyper_oob_final$samp_size
  node_size = hyper_oob_final$node_size
  
  for (i in Nin:(N)) {
    # estimate rf again each time using new model (but same hyper parameters)
    # for each iteration: have a new dataframe
    X_train = df[1:i,]
    # y_train = y[1:i]
    
    
    # train rf using values up to current window (starting with )
    rand_forest = randomForest::randomForest(GDP_GR ~.,
                                             data = X_train,
                                             ntrees = ntrees,
                                             mtry = mtry, sampsize = samp_size, nodesize = node_size,
                                             importance = FALSE)
    
    # since rf can only predict one step ahead using current observations:
    # need to use -h of past data to predict h times into the future 
    X_test = X_train[(dim(X_train)[1]),-1] # use last row from current in sample data for the h = 1 prediction
    # print(X_test)
    for (j in 1:length(forh)) {
      # first loop: h = 1 (using observations in t-1, e.g. 1999Q4 in first i-loop)
      p[j] = predict(rand_forest, X_test)
      
      # use row before (-j) respectively to forecast h = 2 (subtract 2) ...
      X_test = X_train[(dim(X_train)[1]-j),-1] 
    }
    # feed in h = 1,2,3,4 predictions
    result[i-Nin+1,2*(1:length(forh))-1] = p[]
    # feed in h = 0 prediction (nowcast), i.e. the residuals
    h0[i-Nin+1,1] = rand_forest$predicted[i] 
    
    # current hyperparameters
    mtry = hyper_oob_final[[count]]$mtry
    samp_size = hyper_oob_final[[count]]$samp_size
    node_size = hyper_oob_final[[count]]$node_size
    
    count = count + 1
    
  }
  # deleting fit of 4th quarter 1999: starting with 1st quarter of 2000 (of the nowcast)
  h0[1:(dim(h0)[1]-1),1] = h0[2:(dim(h0)[1]),1]
  h0[dim(h0)[1],1] = 0  # last row (is 0)
  
  result_all = cbind(h0,result)
  colnames(result_all) =  c("gdp forecast h=0", "gdp",
                            "gdp forecast h=1", "gdp", "gdp forecast h=2", "gdp", 
                            "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  return(result_all)
}
