######################################
# plain rf using OOB 
rf_plain = function(X, gdp , oos_dataframe, ntrees, mtry, Fstdf, xi){
  # using first difference:
  if (Fstdf == TRUE) {
    gdp_d = diff(gdp) # 1st differencing ts
    gdp_ct = gdp_d - mean(gdp_d) # centering ts
    gdp = gdp_ct 
    # loosing one observation: hence also need to delete first observation in regressor data
    X = X[-1,]
    print(gdp)
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

############################################################
# plain random forest using validation set
rf_plain_valid = function(X_train, y_train, X_test, y_test, oos_dataframe, ntrees, mtry){
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
  return(list(forest = rand_forest, plain_forest_pred = 0))
}

###########################################################
# forecasting rf using rolling window with a-priori specification
# i.e. refitting rf, but always with the same hyper parameters
rf_plain_rolling = function(df, gdp, ntrees, mtry, h_max, forh) {
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
  for (i in Nin:(N)) {
    # estimate rf again each time using new model (but same hyper parameters)
    # for each iteration: have a new dataframe
    X_train = df[1:i,]
    # y_train = y[1:i]
    
    # train rf using values up to current window (starting with )
    rand_forest = randomForest::randomForest(GDP_GR ~.,
                               data = X_train,
                               # data = in_sample_dataframe[,-1],
                               mtry = mtry,
                               importance = FALSE,
                               ntrees = ntrees)
    
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
    print(p)
    # feed in h = 1,2,3,4 predictions
    result[i-Nin+1,2*(1:length(forh))-1] = p[]
    
    # feed in h = 0 prediction (nowcast), i.e. the residuals
    h0[i-Nin+1,1] = rand_forest$predicted[i] 
    
    # View(result)
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
##############################################################################
### can weg ???
# rf rolling using evaluation set: hence rf is trained on data up to "the current year,
# and then evaluated on "current year"
# then forecasts next quarters 
rf_rolling_valid_set = function(df, gdp, ntrees, mtry, h_max, forh){
  N = length(df[,2]) # length of time series
  Nin = N - h_max # length of in sample observations
  
  # initializing
  zeros = rep(0, (N-Nin)*2*length(forh)) # first row of result matrix 
  result = matrix(zeros, nrow = N-Nin + 1, ncol = 2*length(forh))
  p = rep(0, length(forh)) # initialize vector to store predictions in
  h0 = matrix(0,  nrow = N-Nin + 1, ncol = 2) # initializing matrix for storing nowcast values
  
  # problem: CANNOT DO PREDICTIONS WHEN USING a test set 
  for (i in Nin:(N)) {
    # estimate rf again each time using new model (but same hyper parameters)
    # for each iteration: have a new dataframe
    X_train = df[1:i,]
    # y_train = y[1:i]
    
    # train rf using values up to current window (starting with )
    rand_forest = randomForest::randomForest(GDP_GR ~.,
                                             data = X_train,
                                             mtry = mtry,
                                             importance = FALSE,
                                             ntrees = ntrees)
    
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
    print(p)
    # feed in h = 1,2,3,4 predictions
    result[i-Nin+1,2*(1:length(forh))-1] = p[]
    
    # feed in h = 0 prediction (nowcast), i.e. the residuals
    h0[i-Nin+1,1] = rand_forest$predicted[i] 
    
    # View(result)
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

#############################################################
# hyper parameter tuning 
# using ranger package to fit rf with pre-specified hyperparameters 
rf_ranger = function(df, ntree, mtry, node_size, samp_frac, seed){
  rf = ranger::ranger(formula = GDP_GR ~.,
                      data = df,
                      mtry = mtry,
                      min.node.size = node_size,
                      sample.fraction = samp_frac,
                      seed = 123)
  return(rf)
}
# using ranger package for hyperparameter training using oob 
rf_ranger_oob = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree){
  N = nrow(df) # length of time series
  Nin = N - h_max # length of in sample observations (each quarter)
  Nin_year = seq(from = Nin+4, to = N, by = 4) # starting at first quarter of each new year, from 2000 to 2022
  
  counter_year = 1
  # always append next 4 quarters and compute oob error for each new year (for each hyper parameter combination)
  for (year in Nin_year) {
    current_df = df[1:year,]
    View(current_df)
    
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          set.seed(123) 
          forest = rf_ranger(df = current_df[,-c(1,3)], ntree = ntree, 
                             mtry = mtry, samp_frac = samp_size, node_size = node_size)
          # store error for current hyperparam combination
          rf_acc[count,"Error"] = forest$prediction.error # extract oob error
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
          
        }
      }
    }
    View(rf_acc)
    # extract optimal hyper parameter for the current year
    hyper_oob_final[counter_year,] = rf_acc[which.min(rf_acc[,1]),]
    View(hyper_oob_final)
    # browser()
    
    counter_year = counter_year + 1
  }
  return(hyper_oob_final)
}
### using test-set and randomForest package
rf_hyper_test_set = function(df, mtry_grid, samp_size_grid, node_size_grid, ntree){
  N = nrow(df) # length of time series
  Nin = N - h_max # length of in sample observations (each quarter)
  Nin_year = seq(from = Nin, to = N, by = 4) # starting at first quarter of each new year, from 2000 to 2022

  counter_year = 1
  # always append next 4 quarters and compute oob error for each new year (for each hyper parameter combination)
  for (year in Nin_year) {
    current_train_df = df[1:year,]
    current_test_df = df[(year+1):(year+4),] # using the next 4 quarters for testing 
    View(current_train_df)
    View(current_test_df)
    
    # initialize accuracy matrix for current year
    rf_acc = data.frame(matrix(ncol = 4, nrow = 0)) 
    colnames(rf_acc) = c("Error", "mtry","samp_size", "node_size")
    count = 1 # counter for the rows
    
    # hyper parameter grids
    for (mtry in mtry_grid) {
      for (samp_size in samp_size_grid) {
        for (node_size in node_size_grid) {
          # seed = 100 # starting seed ?? incrase each iteration - or always same seed?
          forest = randomForest(GDP_GR ~., data = current_train_df[,-c(1,3)],
                                xtest = current_test_df[,-(1:3)], ytest = current_test_df$GDP_GR,
                                mtry = mtry, sampsize = samp_size, nodesize = node_size,
                                importance = FALSE,
                                ntrees = ntree)
        
          # store error for current hyperparam combination
          # since always growing up to 500 trees, always use error when ntree = 500,
          # therefore making sure error stabilized enough 
          # browser()
          rf_acc[count,"Error"] =  forest$test$mse[ntree] 
          rf_acc[count, "mtry"] = mtry
          rf_acc[count, "samp_size"] = samp_size
          rf_acc[count, "node_size"] = node_size
          
          count = count + 1
          # View(rf_acc)
          # browser()
        }
      }
    }
    View(rf_acc)
    # extract optimal hyper parameter for the current year
    hyper_oob_final[counter_year,] = rf_acc[which.min(rf_acc[,1]),]
    View(hyper_oob_final)
    browser()
    
    counter_year = counter_year + 1
  }
  return(hyper_oob_final)
}
#################################################
# again rf with rolling window, but now using optimal parameter specification for each year
rf_optparam_rolling = function(df, gdp, ntrees, mtry, h_max, forh){
                               # !!!!!!!!!! hyper_oob_final
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
  for (i in Nin:(N)) {
    # estimate rf again each time using new model (but same hyper parameters)
    # for each iteration: have a new dataframe
    X_train = df[1:i,]
    # y_train = y[1:i]
    
    # train rf using values up to current window (starting with )
    rand_forest = randomForest::randomForest(GDP_GR ~.,
                                             data = X_train,
                                             # data = in_sample_dataframe[,-1],
                                             mtry = mtry,
                                             importance = FALSE,
                                             ntrees = ntrees
                                             
                                             # extract optimal variables from year before
                                             )
    
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
    print(p)
    # feed in h = 1,2,3,4 predictions
    result[i-Nin+1,2*(1:length(forh))-1] = p[]
    
    # feed in h = 0 prediction (nowcast), i.e. the residuals
    h0[i-Nin+1,1] = rand_forest$predicted[i] 
    
    # View(result)
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
