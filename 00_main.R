# clean environment
rm(list=ls()) # clear out all variables in current session 

# Set working directory
# wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"

# office:
wd = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"
# wd = "C:/Users/u32/Desktop/Max/RF_GDP"

#wd = ""  # enter you wd

setwd(wd)

# Load Packages
source("01_load_packages.R")  # load package file
install_and_load(a = FALSE) # set a to TRUE if you want to install packages in 01_load_packages.R file

##############################################################################################
# Load Data
df = read.csv("input/current.csv") # load dataframe 

# Inspect the raw Data
source("02_data_cleaning.R") # load data-cleaning file

inspect(df)

#Dimensions of df: 255 rows (observations) and 246 variables (excluding the date column)
#55 variables contain NA entries 
#Last value available: "2022-03-01"
#first value available: "3/1/1959"



# load ts from carstensen and exchange gdp column (ensure using same target as other participants)
# df[length(ts)-dim(df)[1]:dim(df)[1]]

# Clean Data frame
source("02_data_cleaning.R") # load data-cleaning file
data = clean(df) # list with all relevant component extracted 

# use gDP from carstensen (small differences in the decimal places)

##############################################################################################
# plotting gdp
source("04_plots.R")
gdp_plot(data$gdp_raw, title = "GDP", ylab = "GDP")
gdp_plot(data$df_trans[,2], title = "GDP growth rate", ylab = "log returns")

# in sample data
# Forecasting 88 quarters

h_max = 88 # forecast horizon: 4 quarters of 22 years (i.e. 88 quarters)

# in sample dataframe
data_in = in_out_sample(df = data$df_trans, gdp = data$gdp_raw, h_max)

# plotting in sample data
gdp_plot(data_in$gdp_raw_in,  title = "GDP in sample", ylab = "GDP") 
gdp_plot(data_in$insample_dataframe[,2], title = "GDP growth in sample", ylab = "gdp growth")

# using ggplot

##############################################################################################
# estimating benchmark models and doing a simple forecast (h = 1, ..., 88) 
# analyzing parameter combinations and finding out best coefficients to use in rolling window approach
source("03_benchmark.R") 

# using entire data series
ar(data$gdp_raw, 1,1, h_max)
# Dickey Fuller test for the 1st different: p = 0.01 < 0.05 => can reject H0 (ts stationary)
# Using AR(1,1) model based on acf and pacf
# coefficients of the AR model do not seem to be significant
# residuals of AR model seem to be white noise according to LjungBoxPierce test, 
# but qq-plot shows somewhat systematic deviation
# 2 large outliers due to covid pandemic:

################## using only in-sample data and non-transformend GDP
# in the following: using 1st order differencing to make GDP stationary

ar_21 = ar(data_in$gdp_raw_in,2,1, h_max)
# acf yields ma of order 2, pacf indicate long memory?
# using ar(2,1) -> two significant coefficient, but with ar(1,1) parameters significant
# no significant parameter
# residuals seems to be white noise (qq-plot): confirmend by Ljung Box pierce test
# rss      aic     aicc      bic
# 463800.3 7.990269 7.991434 8.047209

# now using ma order of 3 and ar order of 1
ar_13 = ar(data_in$gdp_raw_in, ar_ord = 1, ma_ord = 3 , h_max)
# ar1 only significant parameter value
#  rss      aic     aicc      bic
# 463789.4 8.002515 8.004391 8.078436
# slightly higher information criteria (worse than ar_21)

# trying ar_11
ar_11 = ar(data_in$gdp_raw_in, ar_ord = 1, ma_ord = 1 , h_max)
# both parameters significant now: information criteria about the same
# rss      aic     aicc      bic
# 464805.7 7.980165 7.980782 8.018125
# p-value mostly larger than 0.01 => residuals seem to be white noise


# fitting a random walk model
rw = ar(data_in$gdp_raw_in, 0,0, h_max)
# residuals no white-noise: ar_models better

###################### estimating and forecasting GDP growth
ar_22_growth = ar_growth(data_in$insample_dataframe$GDP_GR, ar_ord = 2, ma_ord = 2, h_max)
# pacf suggests ar order of 2, acf suggests ma order of 2
# but ma2 coefficient not significant 
# errors seem to be white noise
# rss       aic      aicc       bic
# 0.0119434 -9.472247 -9.470371 -9.396326

# fitting ar_21_model
ar_21_growth = ar_growth(data_in$insample_dataframe$GDP_GR, ar_ord = 2, ma_ord = 1, h_max)
# now all parameter very significant. 
# erros on the edge on being white noise or non-white noise
# information criteria a little bit higher (slightly)
# rss       aic      aicc       bic
# 0.01211922 -9.469903 -9.468738 -9.412963

ar_11_growth = ar_growth(data_in$insample_dataframe$GDP_GR, ar_ord = 1, ma_ord = 1, h_max)
# both parameters significant
# slightly best information criteria (lowest)
# rss       aic      aicc       bic
# 0.01280727 -9.426953 -9.426336 -9.388993
# erros slightly white-noise


rw_growth = ar_growth(data_in$insample_dataframe$GDP_GR, ar_ord = 0, ma_ord = 0, h_max)
# white-noise hypothesis can be rejected (errors not white-noise)

## using ar_11_growth in the following 

# plotting predictions
source("04_plots.R")
# plot growth predictions
gdp_growth_forecast_plot(data$df_trans$GDP_GR, gdp_forecast = ar_11_growth$predicitons$pred, se = ar_11_growth$predicitons$se, 
                         "oos_growth_forecasts_ar11", ylab = "gdp growth", col = "blue", CI = TRUE)
gdp_growth_forecast_plot(data$df_trans$GDP_GR, gdp_forecast = rw_growth$predicitons$pred, se = rw_growth$predicitons$se, 
                         "oos_growth_forecasts_rw", ylab = "gdp growth", col = "red", CI = TRUE)
# plot GDP predictions
gdp_forecast_plot(data$gdp_raw, ar_11$predicitons_inverted, title = "oos_forecasts_ar11", ylab = "gdp", col = "blue", CI = TRUE,
                  se = ar_11$predicitons$se) 
gdp_forecast_plot(data$gdp_raw, rw$predicitons_inverted, "oos_forecasts_rw", ylab = "gdp", col = "red",
                  CI = TRUE, se = rw$predicitons$se)


## using ts decomposition


##############################################################################################
# estimating benchmark models recursively: using ar11 based on results above
source("03_benchmark.R") 
source("05_functions.R")

# procedure:
# constructing result table: the 1st, 3rd, 5th contains the h = 1,2,3,4 forecasts, 
# then the true gdp values are inserted in every 2nd column next to forecast column
# then computing forecast errors based on result table

forh = c(1,2,3,4) # forecast horizons (not including forecast h = 0)

# computing gdp GROWTH forecasts iteratively
result_ar_growth = ar_rolling(data$df_trans$GDP_GR, ar_ord = 1, ma_ord = 1, 
                                     h_max, forh, Fstdf = FALSE)

# feed in true values
result_ar_growth = feed_in(result = result_ar_growth, gdp = data$df_trans$GDP_GR, h_max, forh)
View(result_ar_growth)

# plotting the different forecasts
source("04_plots.R")
for (j in 1:5) {
  gdp_growth_forecast_plot(data$df_trans$GDP_GR, gdp_forecast = result_ar_growth[,(2*j-1)], 
                           se = sd(result_ar_growth[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_ar11, h=",j-1), 
                           ylab = "GDP growth", col = "blue", 
                           CI = FALSE)
  print(head(result_ar_growth[,(2*j-1)])) # printing first forecast
  print(sd(result_ar_growth[,(2*j-1)])) # printing standard error 
}
# standard errors and forecasts are pretty similar
# corona outburst is weaker predicted, the larger the forecast horizon

# Forecast evaluation
# evaluate forecasts using: ME, MAE, MSE
# and possibly some tests:Diebold - Marino, Superior predictive ability test, model confidence sets
source("05_functions.R")
eval_for_ar_growth = eval_forc(result_ar_growth[1:(dim(result_ar_growth)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_ar_growth)

which.min(eval_for_ar_growth$me) # h = 4 forecast has min me value ??
which.min(eval_for_ar_growth$mse) # h = 0 has min mse value
which.min(eval_for_ar_growth$mae) #  h = 0 has min mae value

################################ forecasting GDP iteratively (not growth)
# using ar11 model from above: 
source("03_benchmark.R") 
result_ar = ar_rolling(data$df_trans$GDPC1, ar_ord = 1, ma_ord = 1, 
                       h_max, forh, Fstdf = TRUE, 
                       xi = data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"] )
# invert forecasts back into original scale (since used 1st difference of GDP)
# xi is first value that is predicted, i.e. 2000Q1, which serves as y_0 when re-differencing GDP

# feed in true values
result_ar = feed_in(result = result_ar, gdp = data$df_trans$GDPC1[-1], h_max, forh)
View(result_ar)

# plotting the different forecasts
source("04_plots.R")
for (j in 1:5) {
  gdp_forecast_plot(data$df_trans$GDPC1, gdp_forecast = result_ar[,(2*j-1)], 
                           se = sd(result_ar[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_ar11, h=",j-1), 
                           ylab = "GDP", col = "blue", 
                           CI = TRUE)
  print(head(result_ar[,(2*j-1)])) # printing first forecast
  print(sd(result_ar[,(2*j-1)])) # printing standard error 
}
eval_for_ar = eval_forc(result_ar[1:(dim(result_ar)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_ar)

which.min(eval_for_ar$me) # h = 4 forecast has min me value ??
which.min(eval_for_ar$mse) # h = 0 has min mse value
which.min(eval_for_ar$mae) #  h = 0 has min mae value
###########################################################################
## estimate plain rf and forecast

# create train (in_sample) and test (out_of_sample) dataframe for GDP growth
X_in = data_in$insample_dataframe[,-(2:3)] # regressors
y_in = data_in$insample_dataframe$GDP_GR # growth rate
X_out = data$df_trans[( (dim(data$df_trans)[1]-h_max + 1):(dim(data$df_trans)[1]) ), -(2:3)] 
# last 88 rows, excluding data and gdp data 
# y_out = data$df_trans[(dim(data$df_trans)[1]-h_max + 1:dim(data$df_trans)[1]),2]
ntree = 500
source("06_rf.R")
# fit plain rf using OOB (out-of-bag error) as test-error estimate 
set.seed(123) # set seed to ensure bagging same variables in training process
rf_plain_growth = rf_plain(X = X_in[,-1], 
                    gdp = y_in,
                    oos_dataframe = X_out[,-1], # unseen dataset for prediction
                    mtry = round(sqrt(dim(data_in$insample_dataframe[,-(1:3)])[2])), 
                    # number of predictors used is square root of overall predictors available
                    ntrees = ntree,
                    Fstdf = FALSE, 
                    xi = data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"])
# plot growth forecast of plain rf
gdp_growth_forecast_plot(data$df_trans$GDP_GR, gdp_forecast = rf_plain_growth$plain_forest_pred, 
                         se = sd(rf_plain_growth$plain_forest_pred), 
                         "oos_growth_forecasts_rf_plain", ylab = "gdp growth", col = "green", 
                         CI = TRUE)
plot(rf_plain_growth$forest) # OOB sample error 
which.min(rf_plain_growth$forest$mse) # number of trees that minimize OOB sample error: 395
# importance plot
randomForest::varImpPlot(rf_plain_growth$forest)
# %increase in MSE and increase in node purity 
# using ggplot2 

### converting GDP back, i.e. inverting the diff(log(x)) transformation (get levels from growth rate)
source("05_functions.R")
gdp_inverted = exp(cumsum(rf_plain_growth$plain_forest_pred)) * data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"]
gdp_forecast_plot(data$df_trans$GDPC1, gdp_forecast = gdp_inverted, 
                  se = sd(gdp_inverted),
                  "oos_growth_forecasts_rf_plain", ylab = "GDP", 
                  col = "green", CI = FALSE)
# error accumulates => the larger the horizon, the more and more GDP is overestimated 


#################################################################
# now using validation set approach / LAST BLOCK EVALUATION
# fit model in first part of the series => then evaluate on later part,
# all inside training data: X_in goes from 1959-06-01 to 1999-12-01, which are approx. 40 years => use first 32 years
# as training data and data from 1991 onwards as test-data set
X_in_train = data_in$insample_dataframe[1:which(X_in$sasdate == "1991-03-01"),-c(2,3)] 
# data up to 1991Q1 including GDP_growth & GDP
View(X_in_train)
X_in_test = data_in$insample_dataframe[which(X_in$sasdate == "1991-06-01"):nrow(X_in),-c(2,3)] 
# data onwards excluding GDP_growth
View(X_in_test)
y_in_train = data_in$insample_dataframe$GDP_GR[1:which(X_in$sasdate == "1991-03-01")] 
y_in_test = data_in$insample_dataframe$GDP_GR[which(X_in$sasdate == "1991-03-01"):nrow(X_in)] 

source("06_rf.R")
# using validation set: training rf on training set (excluding data column) and testing on test-data
set.seed(123)
rf_plain_vs = rf_plain_valid(X_in_train[,-1], y_in_train, X_in_test[,-1], y_in_test, 
                             oos_dataframe = X_out[,-1], ntrees = ntree, 
                             mtry = round(sqrt(dim(data_in$insample_dataframe[,-(1:3)])[2])) )
plot(rf_plain_vs$forest) # OOB sample error 
class(rf_plain_vs$forest)
# [1] "randomForest.formula" "randomForest"
# predict(rf_plain_vs$forest$call, X_out[,-1]) # rf cannot predict when using a validation set 
class(rf_plain_vs$forest)
rf_plain_vs$forest$predicted # predicted values of inpute data based on out-of-bag samples
rf_plain_vs$forest$mse # mean square error: sum of squared residuals divided by n -> for the OOB samples respectively 
# compute other measures with rf$prediction 
rf_plain_vs$forest$test$mse # test mean squared error, for each tree number  

#############################################################################
# now using cross-validation / BLOCKED CROSS VALIDATION 
# but cannot completely shuffle df, here I need to use step-wise approach
# Therefore the df is sliced, using next 4 quarters (1 year) each time to validate,
# starting with 1970-03-01 
# then: estimate rf and compute test error for the next 4 quarters (1 year)
# store $mse (so that in the end have mse for all forests with different number of trees)
# take average mse for each ntree in the end
cv_plain = matrix(0, nrow = ntree, ncol = length(seq(which(X_in$sasdate == "1970-03-01"), (nrow(X_in)-3), by = 4))+3)  
# initialize matrix to store test error: ncol equals number of cv test sets (33)
c = 1 # initialize running index
for (y in (seq(which(X_in$sasdate == "1970-03-01"), (nrow(X_in)-3), by = 4) )) {
  X_in_slice = X_in[1:(y-1),] # current training set: first iteration, data up to 1970-03-01
  y_in_slice = y_in[1:(y-1)]
  X_test_cv = X_in[y:(y+3),] # use all 4 quarters of next year respectively for test set
  y_test_cv = y_in[y:(y+3)]
  # View(X_in_slice)
  # View(X_test_cv)
  rf_plain_vs = rf_plain_valid(X_train =  X_in_slice[,-1], y_train = y_in_slice, 
                               X_test =  X_test_cv[,-1], y_test = y_test_cv, 
                               oos_dataframe = X_out[,-1], ntrees = ntree, 
                               mtry = round(sqrt(dim(data_in$insample_dataframe[,-(1:3)])[2])) )
  
  cv_plain[,c] = rf_plain_vs$forest$test$mse # store the test_error for each window
  c = c + 1
  # View(cv_plain)
  # browser()
}
# compute for each row (i.e. number of tree used in forest) the average of all test-set MSE's
cv_ntree_plain = apply(cv_plain, 1, mean) # apply mean to to matrix cv_plain by row 

######################################################
# compare oob-error, validation-error and cross-validation error using ggplot:
source("04_plots.R")
plot.data = data.frame(ntrees = 1:ntree, oob = rf_plain_growth$forest$mse, 
                       test_error = rf_plain_vs$forest$test$mse, cv_error = cv_ntree_plain) 
colors = c("OOB" = "black", "Test Error" = "blue", "CV Error" = "red")
ggplot_errors(df = plot.data, colors = colors)

# number of trees that yields lowest Error respectively:
which.min(rf_plain_growth$forest$mse) # number of trees that minimize OOB sample error: 395
which.min(rf_plain_vs$forest$mse) # number of trees that minimize OOB sample error: 395
which.min(cv_ntree_plain) # number of trees that minimize OOB sample error: 395
# seems to stabilize btw. 300 to 500 

#############################################################
# estimate GDP using plain rf without making all time series in the dataset stationary!! 
# 1) not first differencing GDP: completely underestimates
# 2) using transformed dataframe and GDP: underestimates even further
# 3) first differencing and plan_X : overestimates and captures almost nothing
# 4) using plain df and not centered GDP: totally underestimates, but gets bumps etc. 
source("02_data_cleaning.R")
df_plain = clean_2(df)
df_in = in_out_sample(df_plain$df_trans, gdp = df_plain$df_trans$GDPC1, h_max = h_max)
X_in_plain = df_in$insample_dataframe[,-(1:2)]
y_in_plain = df_in$gdp_raw_in
X_out_plain = df_plain$df_trans[( (dim(df_plain$df_trans)[1]-h_max + 1):(dim(df_plain$df_trans)[1]) ), -(1:2)]
 
source("06_rf.R")
rf_plain_GDP = rf_plain(X = X_in_plain,
                        gdp = y_in_plain,
                        oos_dataframe = X_out_plain, #
                        mtry = ncol(df_in$insample_dataframe)/3,
                        # number of predictors used is square root of predictors
                        ntrees = 500,
                        Fstdf = FALSE,
                        xi = data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"])
# plot growth forecast of plain rf
gdp_forecast_plot(df_plain$df_trans$GDPC1, gdp_forecast = rf_plain_GDP$plain_forest_pred,
                         se = sd(rf_plain_GDP$plain_forest_pred),
                         "oos_growth_forecasts_rf_plain", ylab = "GDP",
                         col = "green", CI = FALSE)
# shows: rf cannot capture trends (i.e. extrapolote) when using NON-STATIONARY ts data

##############################################################
# estimate rf with rolling window approach: 
# using a-priori hyper parameter model specification
# and training new forest each iteration
source("06_rf.R")
# using OOB error 
rf_plain_rolling = rf_plain_rolling(df = data$df_trans[,-c(1,3)], # exclude data column and GDPC1
                                    gdp = data$df_trans$GDP_GR,
                                    mtry = (ncol(data$df_trans) - 3)/3, # predictors used is square root of predictors
                                    ntrees = 500, 
                                    h_max, forh)
View(rf_plain_rolling)

# feed in true values
result_rf = feed_in(result = rf_plain_rolling, gdp = data$df_trans$GDP_GR, h_max, forh)
View(result_rf)

# plotting the different forecasts
source("04_plots.R")
for (j in 1:5) {
  gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = result_rf[,(2*j-1)], 
                           se = sd(result_rf[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(result_ar[,(2*j-1)])) # printing first forecast
  print(sd(result_ar[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf = eval_forc(result_rf[1:(dim(result_rf)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf)
which.min(eval_for_rf$me) # h = 3 forecast has min me value
which.min(eval_for_rf$mse) # h = 1 has min mse value
which.min(eval_for_rf$mae) #  h = 1 has min mae value
# compare to ar11
print(eval_for_ar)
print(eval_for_rf)
# linear models better so far

##### converting GDP back (not growth) when rolling window approach is used  
source("05_functions.R")
result_rf_GDP = feed_in(result = rf_plain_rolling, gdp = data$df_trans$GDPC1, h_max, forh) # insert GDP level values
View(result_rf_GDP)
# convert growth rates back
result_rf_GDP = invert_growth(result_rf_GDP,
                       y_0 = data$df_trans$GDPC1[data$df_trans$sasdate == "1999-12-01"])
View(result_rf_GDP)

# plot GDP level forecasts
for (j in 1:5) {
  gdp_growth_forecast_plot(data$df_trans$GDPC1, gdp_forecast = result_rf_GDP[,(2*j-1)], 
                           se = sd(result_rf_GDP[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
                           ylab = "GDP", col = "green", 
                           CI = FALSE)
  print(head(result_rf_GDP[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_GDP[,(2*j-1)])) # printing standard error 
}
# forecast evaluation
eval_for_rf_GDP = eval_forc(result_rf_GDP[1:(dim(result_rf_GDP)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf_GDP)

which.min(eval_for_rf_GDP$me) # h = 4 forecast has min me value
which.min(eval_for_rf_GDP$mse) # h = 2 has min mse value
which.min(eval_for_rf_GDP$mae) #  h = 2 has min mae value

# compare to ar11
print(eval_for_ar)
print(eval_for_rf_GDP)

#############################################################
# hyper parameter tuning: find out optimal hyperparameter for each year in the forecasting window
# these values respectively are then used again in the rolling-window forecasting scheme 
# for comparison reasons: the oob error, test error and cv error are used

# hyper parameters to tune: 
# number of variables to choose from for each tree-split: grid from 1 to p 
# sample size: #observations used for training of each tree => low sample size means less correlation, but higher bias
# and v.v.: larger sample size might yield more variance: using range of 60%-80%
# nodesize: to control complexity of each tree: determines number of observations in terminal nodes: few observations means
# large trees (deep trees) => larger variance, smaller bias 
# don't need to tune number of trees: as seen before, ntree stabilizes at around 200: up to 500 trees is sufficient

mtry_grid = seq(5, ncol(data$df_trans[,-c(1,2,3)]), 2) # start with 5 split variables, than increase up to p (bagging)
samp_size_grid = c(0.55, 0.632, 0.7, 0.8)
node_size_grid = seq(3,9, 2)

### 1) OOB error: 
# for each new year: fit forest to each hyper parameter combination -> evaluate on current year
# save oob error into matrix: rows are the years, columns are the parameter combinations
# for lowest error of the year: store ntree the yielded lowest error for the parameter combination

hyper_oob_final = matrix(0, nrow = 22, ncol = 4) # initialize matrix to store opt. hyper parameter combination in
colnames(hyper_oob_final) = c("OOB Error", "mtry", "samp_size", "node_size")
rownames(hyper_oob_final) = sprintf("%d", seq(2000, 2021, by = 1))
View(hyper_oob_final)

source("06_rf.R")
start_time = Sys.time()
# for the tuning: using the ranger package: C++ implementation of Breiman rf => computationally more efficient
hyper_oob_final = rf_ranger_oob(df = data$df_trans, mtry_grid, samp_size_grid, node_size_grid, 500)
saveRDS(hyper_oob_final, file = "output/hyperparams_oob.rda") # save hyper_oob_final 
# readRDS("output/hyperparams_oob.rda")
end_time = Sys.time()
print(paste("estimation time", end_time-start_time))


### 2) using test_error Procedure / LAST BLOCK EVALUATION
# again for each year, grid of the hyper parameter combination is computed
# now not using oob error, but using test set, which are the next 4 quarters of the next year
# fist rf is fit on data up to test data (e.g. up to 2000 in first iteration)
# and evaluated on first year later (since max h = 4), using test error
# then the optimal parameters (parameter combination that yields smallest test set error)
# are saved for the respective year, 
# then the training horizon is increased by last year (last test test), 
# again yielding optimal parameters the next year, and so on

source("06_rf.R")
hyper_test_final = rf_hyper_test_set(df = data$df_trans, mtry_grid, samp_size_grid, node_size_grid, 500)


################################################################
# now using cross_validation, i.e. BLOCKED CROSS VALIDATION
# same approach as when using last bock evaluation, but now 
# always using each quarter of next test year -> then compute average of all the test_mse's,
# for each hyper parameter combination 



#################################################################
# optimal hyper parameter search for the years from start of the ts up to 2000Q1
# these values are used for the first rolling window 
# -> use cv on entire series and then average optimal parameters?



##########################################################################
# 2) again using rolling window and training new forest each prediction, but this time
# with the optimal number of parameters for each year, using the oob errors

