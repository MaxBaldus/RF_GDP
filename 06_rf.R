# plain rf
rf_plain = function(in_sample_dataframe, y_in, oos_dataframe, y_out, ntrees, mtry){
  
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


# forecasting using rolling window: