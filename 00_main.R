# 1) set up the environment
##############################################################################################
rm(list=ls()) # clear out all variables in current session 

# Set working directory
wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"
# office:
# wd = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"
# wd = "C:/Users/u32/Desktop/Max/RF_GDP"
# wd = "C:/Users/Guest/Desktop/Max/RF_GDP"

setwd(wd)

# Load Packages
source("01_load_packages.R")
a = FALSE # set a to true if You want to install packages
install_and_load(a = a) 
##############################################################################################
# 2) prepare the data
##############################################################################################
# Load data by McCracken
df_mc = read.csv("input/current.csv") # load the spreadsheet data 
source("02_data_cleaning.R") # load data-cleaning file
inspect_mc(df_mc)
data_mc = suppressWarnings(clean_mc(df_mc))  # list with all relevant components extracted

# df by Carstensen 
df = read_excel("input/SW_Updated_2022.xlsx", sheet = "Monthly Data")
gdp = read_excel("input/SW_Updated_2022.xlsx", sheet = "US GDP")
source("02_data_cleaning.R")
data = create_df(df, gdp)

# updated df 16.09.'22
df_2 = read_excel("input/SW_Updated_2022_2.xlsx", sheet = "Monthly Data")
source("02_data_cleaning.R")
data = suppressWarnings(create_df_2(df_2, gdp)) 
# if p_value of adf test = 0.01, it is actually smaller 
# the raised Warning: p-value smaller than printed p-value, is surpressed
View(data)

##############################################################################################
# plotting gdp
##############################################################################################
source("04_plots.R")
gdp_plot(data$GDPC1, title = "GDP", ylab = "GDP")
gdp_plot(data$GDP_GR, title = "GDP growth rate", ylab = "log returns")

# Forecasting 88 quarters
h_max = 88 # forecast horizon: 4 quarters of 22 years (i.e. 88 quarters)
# in sample dataframe, i.e. cutting of the df at 2000Q1´
source("02_data_cleaning.R")
data_in = in_out_sample(data, h_max)

# plotting in sample data
gdp_plot(data_in$GDPC1,  title = "GDP in-sample", ylab = "GDP") 
gdp_plot(data_in$GDP_GR, title = "GDP growth in-sample", ylab = "gdp growth")

# using ggplot

##############################################################################################
# estimating benchmark models and doing a simple recursive forecast (h = 1, ..., 88) 
# analyzing parameter combinations and finding out best coefficients to use in rolling window approach
##############################################################################################
source("03_benchmark.R") 
set.seed(501)
# using entire data series
ar_full = ar(data$GDPC1, 1,1, h_max)
# Dickey Fuller test for the 1st difference: p = 0.01 < 0.05 => can reject H0 (ts stationary)
# Using AR(1,1) model based on acf and pacf
# coefficients of the AR model do not seem to be significant
# residuals of AR model seem to be white noise according to LjungBoxPierce test, 
# but qq-plot shows somewhat systematic deviation, 
# mostly because of the 2 large outliers due to covid pandemic:

# using only in-sample data (up to 2000Q1) and non-transformend GDP
# making GDP stationary by using 1st differencing in the following

ar_21 = ar(data_in$GDPC1,2,1, h_max)
# acf yields ma of order 2, pacf indicating long memory?
# using ar(2,1) -> two insignificant coefficients, only ar1 significant 
# residuals seems to be white noise (qq-plot): confirmed by Ljung Box pierce test (df = 4)
# rss      aic     aicc      bic
# [1,] 471103.4 8.012274 8.013453 8.069451

# now using ma order of 3 and ar order of 1
ar_13 = ar(data_in$GDPC1, ar_ord = 1, ma_ord = 3 , h_max)
# ar1 and ma1 only significant parameter value
# rss      aic     aicc      bic
# [1,] 470301.8 8.022916 8.024816 8.099153
# slightly higher information criteria (worse than ar_21)

# trying ar_11
ar_11 = ar(data_in$GDPC1, ar_ord = 1, ma_ord = 1 , h_max)
# both parameters significant now: information criteria about the same (slightly lower)
# rss      aic     aicc     bic
# [1,] 471604.8 8.000992 8.001617 8.03911
# for df=4: p-value of chi-square: 0.71, value slightly larger => H0: iid ~ wn
# (H0: data (residuals) iid not rejected)

# fitting a random walk
rw = ar(data_in$GDPC1, ar_ord = 0, ma_ord = 0 , h_max)
# rss      aic     aicc      bic
# [1,] 589542.6 8.199506 8.199506 8.199506
# error not white noise

# estimating and forecasting GDP growth with linear model
# model selection applied to GDP growth
ar_22_growth = ar_growth(data_in$GDP_GR, ar_ord = 2, ma_ord = 2, h_max)
# pacf suggests ar order of 2, acf suggests ma order of 2
# but ma2 coefficient not significant 
# errors seem not to be white noise: H0: errors ~ iid mostly rejected 
# rss       aic      aicc       bic
# [1,] 0.01202231 -9.459205 -9.457306 -9.382968

# # fitting ar_21_model  -- not working???: non-stationary AR part from CSS ???
# ar_21_growth = ar_growth(data_in$GDP_GR, ar_ord = 2, ma_ord = 1, h_max)
# # now all parameter very significant. 
# # erros on the edge on being white noise or non-white noise
# # information criteria a little bit higher (slightly)
# # rss       aic      aicc       bic
# # 0.01211922 -9.469903 -9.468738 -9.412963

ar_11_growth = ar_growth(data_in$GDP_GR, ar_ord = 1, ma_ord = 1, h_max)
# both parameters significant
# slightly best information criteria (lowest)
# rss       aic      aicc       bic
# [1,] 0.01240076 -9.452902 -9.452277 -9.414784
# errors not white-noise, H0 rejected in most cases

ar_33_growth = ar_growth(data_in$GDP_GR, ar_ord = 3, ma_ord = 3, h_max)
# all parameters significant
# rss       aic     aicc       bic
# [1,] 0.0111721 -9.507858 -9.50401 -9.393502
# penalized information criteria smaller than ar_11 & ar_22
# errors white noise for lag = 6

ar_32_growth = ar_growth(data_in$GDP_GR, ar_ord = 3, ma_ord = 2, h_max)
# ma1 coefficients not significant
# rss       aic      aicc       bic
# [1,] 0.01129182 -9.509545 -9.506757 -9.414249
# error (df = 4) slightly white noise (df = 4)

rw_growth = ar_growth(data_in$GDP_GR, ar_ord = 0, ma_ord = 0, h_max)
# white-noise hypothesis can be rejected (errors not white-noise)
##############################################################################################
# using ar_33_growth and ar_11 in the following 
# plotting recursive predictions 
##############################################################################################
source("04_plots.R")
# plot growth predictions
gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = ar_33_growth$predicitons$pred, se = ar_11_growth$predicitons$se, 
                         "oos_growth_forecasts_ar11", ylab = "gdp growth", col = "blue", CI = TRUE)
# plot GDP predictions
gdp_forecast_plot(data$GDPC1, ar_11$predicitons_inverted, title = "oos_forecasts_ar11", ylab = "gdp", 
                  col = "blue", CI = TRUE,
                  se = ar_11$predicitons$se) 

##############################################################################################
# estimating benchmark models recursively: using ar_33 for growth based on results above
# standard errors and forecasts are pretty similar
# corona outburst is weaker predicted, the larger the forecast horizon
##############################################################################################
forh = c(1,2,3,4) # h = 1,...,4 forecast horizons

# direct oos forecaste gdp growth 
source("03_benchmark.R")
set.seed(501)
result_ar_growth = ar_rolling_recursive(data$GDP_GR, ar_ord = 3, ma_ord = 3, 
                                        h_max, forh, 
                                        Fstdf = FALSE, xi = 0)
head(result_ar_growth)
# feed in true values
source("05_functions.R")
#result_ar_growth = feed_in(result = result_ar_growth, gdp = data$GDP_GR, h_max, forh)
result_ar_growth = feed_in(result = result_ar_growth, gdp = data$GDP_GR, h_max, forh)
head(result_ar_growth)
# evaluate forecasts using: ME, MAE, MSE, RMSE
eval_for_ar_growth = eval_forc(result_ar_growth[1:(dim(result_ar_growth)[1]-1),], forh) 
print(eval_for_ar_growth)
# $me
# [1]  0.0007360142 -0.0002127133  0.0004712723  0.0005321455  0.0005774997
# 
# $mse
# [1] 0.0001726372 0.0003165993 0.0002532807 0.0002525814 0.0002523337
# 
# $mae
# [1] 0.005876412 0.007493321 0.007670458 0.007817123 0.007493611
# 
# $rmse
# [1] 0.01313915 0.01779324 0.01591479 0.01589281 0.01588502

# forecasting GDP iteratively (not growth)
##############################################################################################
# using ar11 model from above: 
source("03_benchmark.R") 
set.seed(501)
result_ar = ar_rolling_recursive(data$GDPC1, ar_ord = 1, ma_ord = 1, 
                       h_max, forh, Fstdf = TRUE, 
                       xi = data$GDPC1[data$dates == 2000.00])
# invert forecasts back into original scale (since used 1st difference of GDP)
# xi is first value that is predicted, i.e. 2000Q1, which serves as y_0 when re-differencing GDP

# feed in true values
result_ar = feed_in(result = result_ar, gdp = data$GDPC1[-1], h_max, forh)
head(result_ar)
# evaluate
eval_for_ar = eval_forc(result_ar[1:(dim(result_ar)[1]-1),], forh) 
print(eval_for_ar)
# $me
# [1]   76.99747  -63.22942 -131.27297 -251.01090 -337.36484
# 
# $mse
# [1]  73105.53 322056.29 160912.89 235013.23 259458.05
# 
# $mae
# [1] 185.0300 290.2748 281.7893 345.6882 398.2318
# 
# $rmse
# [1] 270.3803 567.5000 401.1395 484.7816 509.3702

##############################################################################################
# plotting the different forecasts
##############################################################################################
# plotting growth forecasts
source("04_plots.R")
for (j in 1:5) {
  gdp_forecast_plot(data$GDP_GR, gdp_forecast = result_ar_growth[,(2*j-1)], 
                    se = sd(result_ar_growth[,(2*j-1)]), 
                    title = paste0("oos_GDP_forecasts_ar11, h=",j-1), 
                    ylab = "GDP", col = "blue", 
                    CI = TRUE)
  print(head(result_ar_growth[,(2*j-1)])) # printing first forecasts
  print(sd(result_ar_growth[,(2*j-1)])) # printing standard error 
}
# plotting GDP
source("04_plots.R")
for (j in 1:5) {
  gdp_forecast_plot(data$GDPC1, gdp_forecast = result_ar[,(2*j-1)], 
                           se = sd(result_ar[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_ar11, h=",j-1), 
                           ylab = "GDP", col = "blue", 
                           CI = TRUE)
  print(head(result_ar[,(2*j-1)])) # printing first forecasts
  print(sd(result_ar[,(2*j-1)])) # printing standard error 
}

# using ggplot2
##############################################################################################
# estimate plain rf and forecast growth rate values from 2000 up to 2022
##############################################################################################
# create train (in_sample) and test (out_of_sample) dataframe for GDP growth
X_in = data_in[,-(2:3)] # regressors
y_in = data_in$GDP_GR # growth rate
X_out = data[( (dim(data)[1]-h_max + 1):(dim(data)[1]) ), -(2:3)] 

ntree = 500

source("06_rf.R")
# fit plain rf using OOB (out-of-bag error) as test-error estimate 
set.seed(501) 
rf_plain_growth = rf_plain(X = X_in[,-1], 
                    gdp = y_in,
                    oos_dataframe = X_out[,-1], # unseen dataset for prediction
                    mtry = round(sqrt(dim(data_in[,-(1:3)])[2])), 
                    # number of predictors used is square root of overall predictors available
                    ntrees = ntree,
                    Fstdf = FALSE, 
                    xi = data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"])

# plot growth forecast of plain rf
gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_plain_growth$plain_forest_pred, 
                         se = sd(rf_plain_growth$plain_forest_pred), 
                         "oos_growth_forecasts_rf_plain", ylab = "gdp growth", col = "green", 
                         CI = TRUE)



### converting GDP back, i.e. inverting the diff(log(x)) transformation (get levels from growth rate)
# and plotting
source("05_functions.R")
gdp_inverted = exp(cumsum(rf_plain_growth$plain_forest_pred)) * data$GDPC1[data$dates == 2000.00]
gdp_forecast_plot(data$GDPC1, gdp_forecast = gdp_inverted, 
                  se = sd(gdp_inverted),
                  "oos_growth_forecasts_rf_plain", ylab = "GDP", 
                  col = "green", CI = FALSE)
# error accumulates => the larger the horizon, the more and more GDP is overestimated 
par(mfrow = c(1,1)) # reset window
##############################################################################################
# oob error 
##############################################################################################
source("06_rf.R")
# fit plain rf using OOB (out-of-bag error) as test-error estimate for entire dataset
set.seed(501) 
rf_plain_growth = rf_plain(X = data[,-c(1,2,3)], 
                           gdp = data$GDP_GR,
                           oos_dataframe = X_out[,-1], # unseen dataset for prediction
                           mtry = round(sqrt(dim(data_in[,-(1:3)])[2])), 
                           # number of predictors used is square root of overall predictors available
                           ntrees = ntree,
                           Fstdf = FALSE, 
                           xi = data$df_trans$GDPC1[data$df_trans$sasdate == "2000-03-01"])
plot(rf_plain_growth$forest$mse) # OOB sample error 
which.min(rf_plain_growth$forest$mse) # number of tree that minimize OOB sample error: 479
# importance plot
randomForest::varImpPlot(rf_plain_growth$forest)
# %increase in MSE and increase in node purity 

##############################################################################################
# now trying out validation-set approach / LAST BLOCK EVALUATION
# fit model in first part of the series => then evaluate on later part,
# X_in goes from 1959Q3 to 1999Q4, which are approx. 40 years 
# use data from 2000Q1 up to last value as one test set
##############################################################################################
X_in = data_in[,-(2:3)] # regressors
y_in = data_in$GDP_GR # growth rate
X_out = data[( (dim(data)[1]-h_max + 1):(dim(data)[1]) ), -(2:3)] 
y_out = data[( (dim(data)[1]-h_max + 1):(dim(data)[1]) ), 3] 
source("06_rf.R")
# using validation set: training rf on training set (excluding data column) and testing on test-data
set.seed(501)
rf_plain_vs = rf_plain_valid(X_in[,-1], y_in, X_out[,-1], y_out, 
                             ntrees = ntree, 
                             mtry = round(sqrt(dim(data_in[,-(1:3)])[2])) )
plot(rf_plain_vs$test$mse, type = "lty") # test error
##############################################################################################
# now using cross-validation / BLOCKED CROSS VALIDATION 
# but cannot completely shuffle df, here I need to use step-wise approach
# Therefore the df is sliced, using next 4 quarters (1 year) each time to validate,
# starting with 2000Q1
# then: estimate rf and compute test error for the next 4 quarters (1 year)
# store $mse (so that in the end have mse for all forests with different number of trees)
# take average mse for each ntree in the end
##############################################################################################
cv_plain = matrix(0, nrow = ntree, ncol = length(seq(which(data$dates == 2000.00), (nrow(data)-3), by = 4)))  
# initialize matrix to store test error: ncol equals number of cv test sets (33).
c = 1 # initialize running index
set.seed(501)
for (y in (seq(which(data$dates == 2000.00), (nrow(data)-3), by = 4) )) {
  X_in_slice = data[1:(y-1),-(2:3)] # current training set
  y_in_slice = data[1:(y-1),3]
  X_test_cv = data[y:(y+3),-(2:3)] # use all 4 quarters of next year respectively for test set
  y_test_cv = data[y:(y+3),3]
  
  rf_plain_vs = rf_plain_valid(X_train =  X_in_slice[,-1], y_train = y_in_slice, 
                               X_test =  X_test_cv[,-1], y_test = y_test_cv, ntrees = ntree, 
                               mtry = round(sqrt(dim(data_in[,-(1:3)])[2])) )
  
  cv_plain[,c] = rf_plain_vs$test$mse # store the test_error for each window
  c = c + 1

}
# compute for each row (i.e. number of tree used in forest) the average of all test-set MSE's
cv_ntree_plain = apply(cv_plain, 1, mean) # apply mean to to matrix cv_plain by row 
plot(cv_ntree_plain, type = "lty") # test error
##############################################################################################
# compare oob-error, validation-error and cross-validation error
##############################################################################################
source("04_plots.R")
plot.data = data.frame(ntrees = 1:ntree, oob = rf_plain_growth$forest$mse, 
                       test_error = rf_plain_vs$test$mse, cv_error = cv_ntree_plain) 
colors = c("OOB" = "black", "Test Error" = "blue", "CV Error" = "red")
ggplot_errors(df = plot.data, colors = colors)

# number of trees that yields lowest Error respectively:
which.min(rf_plain_growth$forest$mse) # number of trees that minimize OOB sample error: 395
which.min(rf_plain_vs$test$mse) # number of trees that minimize OOB sample error: 395
which.min(cv_ntree_plain) # number of trees that minimize OOB sample error: 395
# oob error stabilizes very quickly 
# CV error also, but higher level
# test-error seems to approach oob error, but way more sluggish in the beginning 

##############################################################################################
# trying out estimating GDP using plain rf without making all time series in the dataset stationary
##############################################################################################
source("02_data_cleaning.R")
df_plain = clean_2(df_mc)
df_in = in_out_sample(df_plain$df_trans, h_max = h_max)
X_in_plain = df_in[,-(1:2)]
y_in_plain = df_in$GDPC1
X_out_plain = df_plain$df_trans[( (dim(df_plain$df_trans)[1]-h_max + 1):(dim(df_plain$df_trans)[1]) ), -(1:2)]
 
source("06_rf.R")
rf_plain_GDP = rf_plain(X = X_in_plain,
                        gdp = y_in_plain,
                        oos_dataframe = X_out_plain, 
                        mtry = ncol(df_in)/3,
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
##############################################################################################
# estimate first difference gdp 
##############################################################################################
# without de-meaning
# create train (in_sample) and test (out_of_sample) dataframe for GDP growth
X_in = data_in[-1,-(2:3)] # regressors
y_in = diff(data_in$GDPC1) # growth rate
X_out = data[( (dim(data)[1]-h_max + 1):(dim(data)[1]) ), -(2:3)] 
source("06_rf.R")
# fit plain rf using OOB (out-of-bag error) as test-error estimate 
set.seed(123) 
rf_plain_growth = rf_plain(X = X_in[,-1], 
                           gdp = y_in,
                           oos_dataframe = X_out[,-1], # unseen dataset for prediction
                           mtry = round(sqrt(dim(data_in[,-(1:3)])[2])), 
                           # number of predictors used is square root of overall predictors available
                           ntrees = ntree,
                           Fstdf = FALSE, 
                           xi = data$GDPC1[data$dates == 2000.00])
# plot growth forecast of plain rf
gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = diffinv(rf_plain_growth$plain_forest_pred,
                                                            xi = data$GDPC1[data$dates == 2000.00]), 
                         se = diffinv(rf_plain_growth$plain_forest_pred,
                                      xi = data$GDPC1[data$dates == 2000.00]), 
                         "oos_GDP_forecasts_rf_plain", ylab = "gdp growth", col = "green", 
                         CI = TRUE)

### estimate first difference gdp with de-meaning
X_in = data_in[,-(2:3)] # regressors
y_in = data_in$GDPC1 # growth rate
X_out = data[( (dim(data)[1]-h_max + 1):(dim(data)[1]) ), -(2:3)] 
set.seed(123) 
rf_plain_growth = rf_plain(X = X_in[,-1], 
                           gdp = y_in,
                           oos_dataframe = X_out[,-1], # unseen dataset for prediction
                           mtry = round(sqrt(dim(data_in[,-(1:3)])[2])), 
                           # number of predictors used is square root of overall predictors available
                           ntrees = ntree,
                           Fstdf = TRUE, 
                           xi = data$GDPC1[data$dates == 2000.00])
gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = rf_plain_growth$plain_forest_pred, 
                         se = sd(rf_plain_growth$plain_forest_pred), 
                         "oos_GDP_forecasts_rf_plain", ylab = "gdp growth", col = "green", 
                         CI = FALSE)
# not really a difference from eye-balling
##############################################################################################
# estimate rf for GDP GROWTH with rolling window approach: 
# using a-priori hyper parameter model specification
# and training new forest each iteration
##############################################################################################
source("06_rf.R")
# using OOB error 
set.seed(501)
rf_plain_rolling = rf_plain_rolling(df = data, # exclude data column and GDPC1
                                    gdp = data$GDP_GR,
                                    mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
                                    ntrees = 500, forh)

# feed in true gdp growth values
source("05_functions.R")
result_rf = feed_in(result = rf_plain_rolling, gdp = data$GDP_GR, h_max, forh)
head(result_rf)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = result_rf[,(2*j-1)], 
                           se = sd(result_rf[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(result_rf[,(2*j-1)])) # printing first forecast
  print(sd(result_rf[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf = eval_forc(result_rf[1:(dim(result_rf)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf)
### results using NOT updated df with some series not stationary
# $me
# [1] 0.0007788418 0.0007276938 0.0012682047 0.0009999453 0.0013073248
# 
# $mse
# [1] 0.0001095273 0.0002904196 0.0002377018 0.0002294070 0.0002231057
# 
# $mae
# [1] 0.002965371 0.006724274 0.006798541 0.006835114 0.006910838
# 
# $rmse
# [1] 0.01046553 0.01704170 0.01541758 0.01514619 0.01493672
### RESULTS using new df with alle series stationary
# $me
# [1] 0.0007306682 0.0009956746 0.0018627365 0.0017896122 0.0017346334
# 
# $mse
# [1] 0.0001105420 0.0002827077 0.0002430072 0.0002231808 0.0002180222
# 
# $mae
# [1] 0.002984076 0.006503464 0.006849856 0.006540405 0.006502038
# 
# $rmse
# [1] 0.01051390 0.01681391 0.01558869 0.01493924 0.01476558
# compare to ar11
print(eval_for_ar_growth)

# rf better already w.r.t rmse 
##############################################################################################
# converting GDP back (not growth) when rolling window approach is used  
##############################################################################################
source("05_functions.R")
result_rf_GDP = feed_in(result = rf_plain_rolling, gdp = data$GDPC1, h_max, forh) # insert GDP level values
head(result_rf_GDP)
# convert growth rates back
result_rf_GDP = invert_growth(result_rf_GDP,
                       y_0 = data$GDPC1[data$dates == 2000.00])

# plot GDP level forecasts
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_GDP[,(2*j-1)], 
                           se = sd(result_rf_GDP[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
                           ylab = "GDP", col = "green", 
                           CI = FALSE)
  print(head(result_rf_GDP[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_GDP[,(2*j-1)])) # printing standard error 
}
# forecast evaluation
eval_for_rf_GDP = eval_forc(result_rf_GDP[1:(dim(result_rf_GDP)[1]-1),], forh) 
print(eval_for_rf_GDP)
### results using NOT updated df with some series not stationary
# $me
# [1]   91.35457   86.03237   18.95288  -62.13409 -127.54753
# 
# $mse
# [1]  18569.87  18802.28  75378.10 114982.48 153434.54
# 
# $mae
# [1] 112.6587 109.0633 113.7162 178.9970 253.8699
# 
# $rmse
# [1] 136.2713 137.1214 274.5507 339.0907 391.7072
### RESULTS using new df with alle series stationary
# $me
# [1]   90.36555   90.87928   28.72847  -49.21344 -120.64445
# 
# $mse
# [1]  17951.60  19086.12  76775.09 110339.21 148834.90
# 
# $mae
# [1] 112.3445 113.5487 114.1585 172.2232 250.7959
# 
# $rmse
# [1] 133.9836 138.1525 277.0832 332.1735 385.7913
# compare to ar11 
print(eval_for_ar)

### using only 1 base value 1999Q4
source("05_functions.R")
result_rf_GDP = feed_in(result = rf_plain_rolling, gdp = data$GDPC1, h_max, forh) # insert GDP level values
head(result_rf_GDP)
# convert growth rates back
result_rf_GDP_acc = invert_growth_err_acc(result_rf_GDP,
                              y_0 = data$GDPC1[data$dates == 2000.00])
head(result_rf_GDP_acc)
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_GDP_acc[,(2*j-1)], 
                           se = sd(result_rf_GDP_acc[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
                           ylab = "GDP", col = "green", 
                           CI = FALSE)
  print(head(result_rf_GDP[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_GDP[,(2*j-1)])) # printing standard error 
}
# forecast evaluation when using only 1 base value
eval_for_rf_GDP_acc = eval_forc(result_rf_GDP_acc[1:(dim(result_rf_GDP_acc)[1]-1),], forh) 
print(eval_for_rf_GDP_acc)
### using updated df
# $me
# [1]  232.5311 1323.4426 1932.2854 1595.4208 1301.5541
# 
# $mse
# [1]  197907.1 2555613.8 5424384.6 3716195.1 2476830.2
# 
# $mae
# [1]  294.0588 1323.4426 1934.2176 1600.9620 1308.4918
# 
# $rmse
# [1]  444.8675 1598.6287 2329.0308 1927.7435 1573.7948
# compare to ar11 
print(eval_for_ar)
##############################################################################################
# Rolling window for GDP using first difference to make GDPC1 stationary 
##############################################################################################
source("06_rf.R")
# using OOB error 
set.seed(501)
rf_rolling_GDP = rf_rolling_GDP(df = data, # exclude data column and GDPC1
                                    gdp = data$GDPC1,
                                    mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
                                    ntrees = 500, forh,
                                    xi = data$GDPC1[data$dates == 2000.00])
# feed in true gdp growth values
source("05_functions.R")
result_rf_GDPC1 = feed_in(result = rf_rolling_GDP, gdp = data$GDPC1, h_max, forh)
head(result_rf_GDPC1)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_GDPC1[,(2*j-1)], 
                           se = sd(result_rf_GDPC1[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
                           ylab = "GDP", col = "green", 
                           CI = FALSE)
  print(head(result_rf_GDPC1[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_GDPC1[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_GDPC1 = eval_forc(result_rf_GDPC1[1:(dim(result_rf)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf_GDPC1)
# compare to ar11
print(eval_for_ar)
#### not updated data
### only differencing (not centering)
# $me
# [1] 145.8778 132.8890 371.4914 116.6855 157.7792
# 
# $mse
# [1]  95672.04 112589.51 254682.73 101806.82 125954.06
# 
# $mae
# [1] 220.4982 230.4682 387.3811 215.9425 251.0537
# 
# $rmse
# [1] 309.3090 335.5436 504.6610 319.0718 354.9001
### also centering
# $me
# [1] 121.7596 100.3618 299.6852 123.2911 151.2846
# 
# $mse
# [1]  82679.85  89207.14 194493.41 112883.39 126508.68
# 
# $mae
# [1] 198.7364 198.5867 328.5712 233.5597 253.7029
# 
# $rmse
# [1] 287.5410 298.6756 441.0141 335.9812 355.6806
######
## updated data with centering the data
# $me
# [1] 134.5109  71.5181 354.2657 263.8894 185.9121
# 
# $mse
# [1]  88997.62  96090.39 244725.43 176914.16 145829.61
# 
# $mae
# [1] 207.7026 200.4991 380.6041 300.1480 275.8487
# 
# $rmse
# [1] 298.3247 309.9845 494.6973 420.6116 381.8764
# rf for some h better, for some h worse .. 
# ##############################################################################################
# Rolling window for GDP using first Hodrick Prescott Filter
##############################################################################################
source("05_functions.R")
test = hp(data$GDPC1)
source("06_rf.R")
set.seed(501)
rf_rolling_GDP_hp = rf_rolling_hp(df = data, # exclude data column and GDPC1
                                gdp = test$residuals,
                                mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
                                ntrees = 500, forh,
                                hp = test$hp)
# forecasted values is e.g. 1.849404e-13, meaning when adding trend and cycle back, yielding
# same values as gdp, because filter too strong ("filters everything out") 

##############################################################################################
# hyper parameter tuning: find out optimal hyper parameters for each year in the forecasting window
# these values respectively are then used again in the rolling-window forecasting scheme 
# for comparison reasons: the oob error and cv error are used
##############################################################################################
# hyper parameters to tune: 
# number of variables to choose from for each tree-split: grid from 1 to p 
# sample size: #observations used for training of each tree => low sample size means less correlation, but higher bias
# and v.v.: larger sample size might yield more variance: using range of 60%-80%
# nodesize: to control complexity of each tree: determines number of observations in terminal nodes: few nodes means
# large trees (deep trees) => larger variance, smaller bias 
# don't need to tune number of trees: as seen before, ntree stabilizes at around 200: up to 500 trees is sufficient

mtry_grid = seq(5, ncol(data[,-c(1,2,3)]), 2) # start with 5 split variables, than increase up to p (bagging) by 2
samp_size_grid = c(0.55, 0.632, 0.7, 0.8)
node_size_grid = seq(3,9, 2)

### 1) OOB error: 
# for each new year: fit forest to each hyper parameter combination -> evaluate on current year
# save oob error into matrix: rows are the years, columns are the parameter combinations
# for lowest error of the year: store ntree the yielded lowest error for the parameter combination

### 1a) GDP GROWTH
hyper_oob_final_growth = list()

source("06_rf.R")
Sys.time()
start_time = Sys.time()
# for the tuning: using the ranger package: C++ implementation of Breiman rf => computationally more efficient
hyper_oob_final_growth = rf_ranger_oob(df = data, mtry_grid, samp_size_grid, node_size_grid, 
                                500, hyper_para_list = hyper_oob_final_growth)

saveRDS(hyper_oob_final_growth, file = "output/hyperparams_oob_growth.rda") # save hyper_oob_final 
end_time = Sys.time()
print(paste("estimation time: ", end_time-start_time))
# read hyperparameters
hyper_oob_final_growth = readRDS("output/hyperparams_oob_growth.rda")

### 1b) GDP
hyper_oob_final_level = list()
source("06_rf.R")
Sys.time()
start_time = Sys.time()
# for the tuning: using the ranger package: C++ implementation of Breiman rf => computationally more efficient
hyper_oob_final_level = rf_ranger_oob_level(df = data, mtry_grid, samp_size_grid, node_size_grid, 
                                       500, hyper_para_list = hyper_oob_final_level,
                                       gdp = data$GDPC1)

saveRDS(hyper_oob_final_level, file = "output/hyperparams_oob_level.rda") # save hyper_oob_final 
end_time = Sys.time()
print(paste("estimation time: ", end_time-start_time))
# read hyperparameters
hyper_oob_final_level = readRDS("output/hyperparams_oob_level.rda")

### 2) using test_error Procedure / LAST BLOCK EVALUATION
# again for each year, grid of the hyper parameter combination is computed
# now computing test-error by using test-set, which are the next 4 quarters of the next year
# fist rf is fit on data up to test data (e.g. up to 1999 in the first iteration)
# and evaluated on first year later (since max h = 4), using test error
# then the optimal parameters (parameter combination that yields smallest test set error)
# are saved for the respective year, 
# then the training horizon is increased by last year (last test test), 
# again yielding optimal parameters the next year, and so on

### 2a) test-error GDP GROWTH
hyper_test_final= list()

source("06_rf.R")
set.seed(123)
Sys.time()
start_time = Sys.time()
hyper_test_final = rf_hyper_test_set(df = data, mtry_grid, samp_size_grid, node_size_grid, 500, 
                                     hyper_para_list = hyper_test_final)
saveRDS(hyper_test_final, file = "output/hyper_test_final.rda")
end_time = Sys.time()
print(paste("estimation time", end_time-start_time))

# read hyperparameters
hyper_test_final = readRDS("output/hyper_test_final.rda")

### 2b) test-error GDP
hyper_test_final_level= list()

source("06_rf.R")
set.seed(123)
Sys.time()
start_time = Sys.time()
hyper_test_final_level = rf_hyper_test_set_level(df = data, mtry_grid, samp_size_grid, node_size_grid, 500, 
                                     hyper_para_list = hyper_test_final_level, gdp = data$GDPC1)
saveRDS(hyper_test_final_level, file = "output/hyper_test_final_level.rda")
end_time = Sys.time()
print(paste("estimation time", end_time-start_time))
# read hyperparameters
hyper_test_final_level = readRDS("output/hyper_test_final_level.rda")


##############################################################################################
# now using cross_validation, i.e. BLOCKED CROSS VALIDATION
# same approach as when using last bock evaluation, but now 
# always using each quarter of next test year as test_set (one row)
# -> then compute average of all the 4 test MSE's,
# for each hyper parameter combination 
# really needed? I.E. NEEDED TO  ESTIMATE TEST ERROR BETTER? OR IS LAST BLOCK EVALUATION ENOUGH?

##############################################################################################
# again using rolling window and training new forest for each new window, but this time,
# using the tuned hyper parameter from each year before, using the oob error
##############################################################################################
### 1a) GDP GROWTH
source("06_rf.R")
set.seed(501)
rf_rolling_growth_hyperopt = rf_plain_rolling_hyperopt(df = data, # exclude data column and GDPC1
                                    gdp = data$GDP_GR, ntrees = 500, forh,
                                    hyper_para_list = hyper_oob_final_growth)
source("05_functions.R")
result_rf_hyperopt = feed_in(result = rf_rolling_growth_hyperopt, gdp = data$GDP_GR, h_max, forh)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = result_rf_hyperopt[,(2*j-1)], 
                           se = sd(result_rf_hyperopt[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(result_rf_hyperopt[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_hyperopt[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt = eval_forc(result_rf_hyperopt[1:(dim(result_rf_hyperopt)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf_hyperopt)
# $me
# [1] 0.0006834487 0.0010013556 0.0019845451 0.0017247639 0.0019248821
# 
# $mse
# [1] 0.0001036816 0.0002855315 0.0002387426 0.0002219672 0.0002194144
# 
# $mae
# [1] 0.002494474 0.006451542 0.007030872 0.006572144 0.006608819
# 
# $rmse
# [1] 0.01018242 0.01689768 0.01545130 0.01489856 0.01481264
print(eval_for_rf) 
# yielding not really superior results  

### 1b) GDP
source("06_rf.R")
set.seed(501)
rf_rolling_hyperopt_level = rf_GDP_rolling_hyperopt(df = data, gdp = data$GDPC1, ntrees = 500, forh,
                                                    hyper_para_list = hyper_oob_final_level, 
                                                    xi = data$GDPC1[data$dates == 2000.00])
source("05_functions.R")
result_rf_hyperopt_level = feed_in(result = rf_rolling_hyperopt_level, gdp = data$GDPC1, h_max, forh)
head(result_rf_hyperopt_level)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_hyperopt_level[,(2*j-1)], 
                           se = sd(result_rf_hyperopt_level[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(result_rf_hyperopt_level[,(2*j-1)])) # printing first forecast
  print(sd(result_rf_hyperopt_level[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt_level = eval_forc(result_rf_hyperopt_level[1:(dim(result_rf_hyperopt_level)[1]-1),], forh) 
# using all but last row (since no comparable data)
print(eval_for_rf_hyperopt_level)
# $me
# [1]  66.36024 109.36703 364.13748 277.93388 215.75250
# 
# $mse
# [1]  72297.85  97080.65 244710.53 186902.77 169132.36
# 
# $mae
# [1] 176.1194 216.5640 373.0317 319.5101 305.9868
# 
# $rmse
# [1] 268.8826 311.5777 494.6822 432.3225 411.2570
print(eval_for_rf) 


### 2a) GDP GROWTH with test set
### 2b) GDP with test set


##############################################################################################
# computing Theil's U and Diebold Mariano Test for all benchmark and forest models
##############################################################################################
source("05_functions.R")
# DM test GDP GROWTH, for each h 
dm_tests(gdp = data$GDP_GR[which(data$dates == 2000.00):length(data$dates)], 
        h_num = 5,
        result_rf = result_rf, result_arma = result_ar_growth)







##############################################################################################
# using rangerts, i.e. ordered boostrapping

# 1) rolling window for GDP and GDP growth -> already better ??
# 2) hyperparameter tuning for GDP and GDP growth 

### to do:
# delete some View()
# plots in paper with qqplot 
# Teil's U and DM test
# using rangerts !!!
# comparing forecasting errors when not including corona 
# using different filter for gdp (hodrick prescott..) -> no results
# use lagged gdp values...
# again forecasting with rf: now using also the quartely data from mccracken (if not the same??)
