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

# plain random forest using validation set
rf_plain_valid = function(X_train, X_test, y_test, oos_dataframe, ntrees, mtry){
  # train random forest on in_sample_dataframe
  rand_forest = randomForest::randomForest(formula = GDP_GR ~.,
                                           data = X_train, # in_sample_dataframe: needed when ~.formular is used
                                           xtest = X_test,
                                           ytest = y_test,
                                           mtry = mtry,
                                           importance = TRUE,
                                           ntrees = ntrees)
  plot(rand_forest)
  # prediction
  # rand_forest_pred = predict(rand_forest2, oos_dataframe) 
  rand_forest_pred = 0
  return(list(forest = rand_forest, plain_forest_pred = rand_forest_pred))
}

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
  # first entry: h = 1, then h = 1,..., 4
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

# hyper parameter search 