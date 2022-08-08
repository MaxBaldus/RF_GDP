# plain rf
rf_plain = function(in_sample_dataframe, y_in, oos_dataframe, ntrees, mtry){
  
  # train random forest on in_sample_dataframe
  rand_forest = randomForest::randomForest(x = in_sample_dataframe,
                                           y = y_in,
                                           # data = in_sample_dataframe[,-1],
                                           mtry = mtry,
                                           importance = TRUE,
                                           ntrees = ntrees)
  # prediction
  rand_forest_pred = predict(rand_forest, oos_dataframe )
  
  return(list(forest = rand_forest, plain_forest_pred = rand_forest_pred))
  
}


# forecasting rf using rolling window with a-priori specification
# i.e. refitting rf, but always with the same hyperparameter
rf_plain_rolling = function(df, y, ntrees, mtry, h_max, forh) {
  N = length(df[,2]) # length of time series
  Nin = N - h_max # length of in sample observations
  
  # initializing
  zeros = rep(0, (N-Nin)*2*length(forh)) # first row of result matrix 
  result = matrix(zeros, nrow = N-Nin, ncol = 2*length(forh))
  p = rep(0, length(forh)) # initialize vector to store predictions in
  
  # loop over each quarter from 2000 up to 2022
  for (i in Nin:(N-1)) {
    # estimate rf again each time using new model 
    # for each iteration: have a new dataframe
    X_train = df[1:i,]
    y_train = y[1:i]
    # train rf using values up to current window
    rand_forest = randomForest::randomForest(x = X_train,
                               y = y_train,
                               # data = in_sample_dataframe[,-1],
                               mtry = mtry,
                               importance = FALSE,
                               ntrees = ntrees)
    
    # since rf can only predict one step ahead using current observations:
    # need to use -h of past data to predict 4 times into the future 
    X_test = X_train[dim(X_train)[1],] # use last row from current in sample data for the h = 1 prediction
    for (j in 1:length(forh)-1) {
      p[j] = predict(rand_forest, X_test)
      # use row before (-j) respectively to forecast h = 2 (subtract 1), 2nd row for h = 3, 3rd row for h = 4
      X_test = X_train[(dim(X_train)[1]-j),] 
    }
    # feed in h-step predictions
    result[i-Nin+1,2*(1:length(forh))-1] = p
  }
  
  colnames(result) =  c("gdp forecast h=1", "gdp", "gdp forecast h=2", "gdp", 
                           "gdp forecast h=3", "gdp", "gdp forecast h=4", "gdp")
  View(result)
  return(result)
}