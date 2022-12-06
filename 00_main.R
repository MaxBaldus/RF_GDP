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

# # df by Carstensen 
# df = read_excel("input/SW_Updated_2022.xlsx", sheet = "Monthly Data")
gdp = read_excel("input/SW_Updated_2022.xlsx", sheet = "US GDP")
# source("02_data_cleaning.R")
# data = create_df(df, gdp)

# updated df 16.09.'22
df_2 = read_excel("input/SW_Updated_2022_2.xlsx", sheet = "Monthly Data")
source("02_data_cleaning.R")
data = suppressWarnings(create_df_2(df_2, gdp)) 
# if p_value of adf test = 0.01, it is actually smaller 
# the raised Warning: p-value smaller than printed p-value, is surpressed
str(data)

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
par(mfrow = c(1,1)) # reset window
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
source("05_functions.R")
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
### for GDP plain using first difference to make it stationary and centering
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
# plotting the different ARMA forecasts
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

##############################################################################################
# estimate plain rf and forecast growth rate values from 2000 up to 2022 once
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
# fit plain rf using OOB (out-of-bag error) as test-error estimate for entire data set
# Importance plot 
# oob error 
##############################################################################################
source("06_rf.R")
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
imp = randomForest::varImpPlot(rf_plain_growth$forest)
# %increase in MSE and increase in node purity 

# variable importance plot using ggplot
source("04_plots.R")
imp_plot(imp) # importance plot of the aggreagted node-purity per category

# save plot once
# pdf(file = "output/Importance.pdf")
# imp_plot(imp)
# dev.off()
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

# save as vector-image (only once)
# pdf(file = "output/Errors.pdf")
# ggplot_errors(df = plot.data, colors = colors)
# dev.off()

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
# estimate rf directly for GDP GROWTH with rolling window approach: 
# using a-priori hyper parameter model specification
# and training new forest each iteration
##############################################################################################
source("06_rf.R")
# using OOB error 
set.seed(501)
rf_plain_rolling_fc = rf_plain_rolling(df = data, # exclude data column and GDPC1
                                    gdp = data$GDP_GR,
                                    mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
                                    ntrees = 500, forh)
head(rf_plain_rolling_fc)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_plain_rolling_fc[,(2*j-1)], 
                           se = sd(rf_plain_rolling_fc[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_plain_rolling_fc[,(2*j-1)])) # printing first forecast
  print(sd(rf_plain_rolling_fc[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
# eval_for_rf = eval_forc(result_rf[1:(dim(result_rf)[1]-1),], forh) 
eval_for_rf = eval_forc_rf(rf_plain_rolling_fc, forh)
print(eval_for_rf)
# $me
# [1] 0.0007618491 0.0008347875 0.0016160759 0.0013445981 0.0013970164
# 
# $mse
# [1] 0.0001044614 0.0002875815 0.0003004401 0.0002476021 0.0002368996
# 
# $mae
# [1] 0.002833917 0.006583654 0.006699152 0.006844096 0.006818618
# 
# $rmse
# [1] 0.01022063 0.01695823 0.01733321 0.01573538 0.01539154
# 
# $smins
# $smins$min_me
# [1] 1
# 
# $smins$min_mse
# [1] 1
# 
# $smins$min_mae
# [1] 1
# 
# $smins$min_rmse
# [1] 1

# compare to ar11
# print(eval_for_ar_growth)

# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_growth = dm_tests(gdp = data$GDP_GR[which(data$dates == 2000.00):length(data$dates)], 
                  h_num = 5,
                  result_rf = rf_plain_rolling_fc, result_arma = result_ar_growth)
print(dm_growth)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# for h = 1 -> rf better , for other h H0

### Theil's U
source("05_functions.R")
U = theils_U(eval_rf = eval_for_rf, eval_arma = eval_for_ar_growth, 5)
print(U)
# 0.7778764 0.9530716 1.0891255 0.9900943 0.9689348
# h = 0,1,3, 4: rf is slightly better (< 1) 
##############################################################################################
# converting GDP back (not growth) when rolling window approach was used  
##############################################################################################
# source("05_functions.R")
# result_rf_GDP = feed_in(result = rf_plain_rolling, gdp = data$GDPC1, h_max, forh) # insert GDP level values
# # convert growth rates back
# result_rf_GDP = invert_growth(result_rf_GDP,
#                        y_0 = data$GDPC1[data$dates == 2000.00])
# 
# # plot GDP level forecasts
# for (j in 1:5) {
#   gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_GDP[,(2*j-1)], 
#                            se = sd(result_rf_GDP[,(2*j-1)]), 
#                            title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
#                            ylab = "GDP", col = "green", 
#                            CI = FALSE)
#   print(head(result_rf_GDP[,(2*j-1)])) # printing first forecast
#   print(sd(result_rf_GDP[,(2*j-1)])) # printing standard error 
# }
# # forecast evaluation
# eval_for_rf_GDP = eval_forc(result_rf_GDP[1:(dim(result_rf_GDP)[1]-1),], forh) 
# 
# ### using only 1 base value 1999Q4
# source("05_functions.R")
# result_rf_GDP = feed_in(result = rf_plain_rolling, gdp = data$GDPC1, h_max, forh) # insert GDP level values
# # convert growth rates back
# result_rf_GDP_acc = invert_growth_err_acc(result_rf_GDP,
#                               y_0 = data$GDPC1[data$dates == 2000.00])
# head(result_rf_GDP_acc)
# for (j in 1:5) {
#   gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = result_rf_GDP_acc[,(2*j-1)], 
#                            se = sd(result_rf_GDP_acc[,(2*j-1)]), 
#                            title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
#                            ylab = "GDP", col = "green", 
#                            CI = FALSE)
#   print(head(result_rf_GDP[,(2*j-1)])) # printing first forecast
#   print(sd(result_rf_GDP[,(2*j-1)])) # printing standard error 
# }
# # forecast evaluation when using only 1 base value
# eval_for_rf_GDP_acc = eval_forc(result_rf_GDP_acc[1:(dim(result_rf_GDP_acc)[1]-1),], forh) 
##############################################################################################
# Rolling window for GDP using FIRST DIFFERENCE to make GDPC1 stationary 
##############################################################################################
source("06_rf.R")
set.seed(501)
rf_rolling_GDP_fc = rf_rolling_GDP(df = data, # exclude data column and GDPC1
                                    gdp = data$GDPC1,
                                    mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
                                    ntrees = 500, forh,
                                    xi = data$GDPC1[data$dates == 2000.00])
head(rf_rolling_GDP_fc)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = rf_rolling_GDP_fc[,(2*j-1)], 
                           se = sd(rf_rolling_GDP_fc[,(2*j-1)]), 
                           title = paste0("oos_GDP_forecasts_rf_plain, h=",j-1), 
                           ylab = "GDP", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_GDP_fc[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_GDP_fc[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_GDPC1 =  eval_forc_rf(rf_rolling_GDP_fc, forh) 
print(eval_for_rf_GDPC1)
# $me
# [1] 112.94882  74.32936 408.30905 406.10988 344.19053
# 
# $mse
# [1]  86845.24 102603.49 287274.46 265064.38 206724.12
# 
# $mae
# [1] 208.7397 204.6258 440.8239 414.1692 353.4884
# 
# $rmse
# [1] 294.6952 320.3178 535.9799 514.8440 454.6692
# 
# $smins
# $smins$min_me
# [1] 2
# 
# $smins$min_mse
# [1] 1
# 
# $smins$min_mae
# [1] 2
# 
# $smins$min_rmse
# [1] 1

# compare to ar11
# print(eval_for_ar)
# rmse: rf for h = 1 and h = 4 better
# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_GDP = dm_tests(gdp = data$GDPC1[which(data$dates == 2000.00):length(data$dates)], 
         h_num = 5,
         result_rf = rf_rolling_GDP_fc, result_arma = result_ar)
print(dm_GDP)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# rf better for all h = 1,...,4

### Theil's U
source("05_functions.R")
U_gdp = theils_U(eval_rf = eval_for_rf_GDPC1, eval_arma = eval_for_ar, 5)
print(U_gdp)
# 1.0899282 0.5644366 1.3361435 1.0620123 0.8926105
# But: U only for h = 1 < 1, otherwise arma is better

###############################################################################################
# Rolling window for GDP using first Hodrick Prescott Filter
##############################################################################################
# source("05_functions.R")
# test = hp(data$GDPC1)
# source("06_rf.R")
# set.seed(501)
# rf_rolling_GDP_hp = rf_rolling_hp(df = data, # exclude data column and GDPC1
#                                 gdp = test$residuals,
#                                 mtry = (ncol(data) - 3)/3, # predictors used is square root of predictors
#                                 ntrees = 500, forh,
#                                 hp = test$hp)
# forecasted values is e.g. 1.849404e-13, meaning when adding trend and cycle back, yielding
# same values as gdp, because filter too strong ("filters everything out") 

##############################################################################################
# hyper parameter tuning: find out optimal hyper parameters for each year in the forecasting window
# these values respectively are then used again in the rolling-window forecasting scheme 
# for comparison reasons: the oob error and test error (h=1) are used
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

# saveRDS(hyper_oob_final_growth, file = "output/hyperparams_oob_growth.rda") # save hyper_oob_final 
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

# saveRDS(hyper_oob_final_level, file = "output/hyperparams_oob_level.rda") # save hyper_oob_final 
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
#saveRDS(hyper_test_final, file = "output/hyper_test_final.rda")
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
# saveRDS(hyper_test_final_level, file = "output/hyper_test_final_level.rda")
end_time = Sys.time()
print(paste("estimation time", end_time-start_time))
# read hyperparameters
hyper_test_final_level = readRDS("output/hyper_test_final_level.rda")


##############################################################################################
# now using cross_validation, i.e. BLOCKED CROSS VALIDATION
# same approach as when using last bock evaluation, but now 
# k = 10 => always using ten time 4 quarter iteratively for each combination to train and test
# 1) start: train on 1:1987Q4, test on 1988 => train on 1:1988Q4, test on 1989 ... up to 1998 being test-set
# this yields first hyper para combination for predicting 2000Q1 (even for h = 4 using 1999Q1)
# 2) start: train on 1: 1988Q4, test on 1989 => up to test set: 1999 => yields hyper para for 2001Q1 with 2000Q1
# for each combination: average error (as in YOON -> k = 10)
# but: not needed for rf, can use ´oob error 

##############################################################################################
# again using rolling window and training new forest for each new window, but this time
# using the tuned hyper parameter from each year before, using the oob error
##############################################################################################
# read hyper parameter list
hyper_oob_final_growth = readRDS("output/hyperparams_oob_growth.rda") # oob growth
hyper_oob_final_level = readRDS("output/hyperparams_oob_level.rda") # oob GDP

hyper_test_final = readRDS("output/hyper_test_final.rda") # test growth
hyper_test_final_level = readRDS("output/hyper_test_final_level.rda") # test GDP

### 1) oob error 
### 1a) GDP GROWTH
source("06_rf.R")
set.seed(501)
rf_rolling_growth_hyperopt = rf_plain_rolling_hyperopt(df = data, # exclude data column and GDPC1
                                    gdp = data$GDP_GR, ntrees = 500, forh,
                                    hyper_para_list = hyper_oob_final_growth)
head(rf_rolling_growth_hyperopt)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_rolling_growth_hyperopt[,(2*j-1)], 
                           se = sd(rf_rolling_growth_hyperopt[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_growth_hyperopt[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_growth_hyperopt[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt = eval_forc_rf(rf_rolling_growth_hyperopt, forh)
print(eval_for_rf_hyperopt)
# $me
# [1] 0.0006128271 0.0009768571 0.0019614065 0.0014173337 0.0014897087
# 
# $mse
# [1] 0.0001041700 0.0002754375 0.0002841820 0.0002454053 0.0002318177
# 
# $mae
  # [1] 0.002472820 0.006514005 0.006633912 0.006812798 0.006871087
# 
# $rmse
# [1] 0.01020637 0.01659631 0.01685770 0.01566542 0.01522556
# 
# print(eval_for_rf) 
# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_growth_opt = dm_tests(gdp = data$GDP_GR[which(data$dates == 2000.00):length(data$dates)], 
                     h_num = 5,
                     result_rf = rf_rolling_growth_hyperopt, result_arma = result_ar_growth)
print(dm_growth_opt)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# H0 cannot be rejected => both same forecast accuracy

### Theil's U
source("05_functions.R")
U_growth_opt = theils_U(eval_rf = eval_for_rf_hyperopt, eval_arma = eval_for_ar_growth, 5)
print(U_growth_opt)
# 0.7767907 0.9327313 1.0592471 0.9856922 0.9584857
# h = 0,1,3 and 4: rf is slightly better (< 1) 
# Theil's U about the same

# U against rf_without hyper parameter:
U_oob = theils_U(eval_rf = eval_for_rf_hyperopt, eval_arma = eval_for_rf, 5)
print(U_oob)
# 0.9986043 0.9786582 0.9725666 0.9955538 0.9892160
# rf with optimal hyper params slightly better!


### 1b) GDP
source("06_rf.R")
set.seed(501)
rf_rolling_hyperopt_level = rf_GDP_rolling_hyperopt(df = data, gdp = data$GDPC1, ntrees = 500, forh,
                                                    hyper_para_list = hyper_oob_final_level, 
                                                    xi = data$GDPC1[data$dates == 2000.00])
#result_rf_hyperopt_level = feed_in(result = rf_rolling_hyperopt_level, gdp = data$GDPC1, h_max, forh)
head(rf_rolling_hyperopt_level)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = rf_rolling_hyperopt_level[,(2*j-1)], 
                           se = sd(rf_rolling_hyperopt_level[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_hyperopt_level[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_hyperopt_level[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt_GDP = eval_forc_rf(rf_rolling_hyperopt_level, forh)
print(eval_for_rf_hyperopt_GDP)
# $me
# [1]  71.94328 108.55501 511.75831 403.70682 422.44429
# 
# $mse
# [1]  75593.92 101330.27 377990.37 270310.97 284377.95
# 
# $mae
# [1] 180.6950 217.6207 512.2799 412.0462 425.4367
# 
# $rmse
# [1] 274.9435 318.3242 614.8092 519.9144 533.2710
#
# print(eval_for_rf_GDPC1) 

# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_GDP_oob = dm_tests(gdp = data$GDPC1[which(data$dates == 2000.00):length(data$dates)], 
                     h_num = 5,
                     result_rf = rf_rolling_hyperopt_level, result_arma = result_ar)
print(dm_GDP_oob)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# H0 can be rejected for all h => rf also better

### Theil's U
source("05_functions.R")
U_GDP_opt = theils_U(eval_rf = eval_for_rf_hyperopt_GDP, eval_arma = eval_for_ar, 5)
print(U_GDP_opt)
# 1.0168767 0.5609236 1.5326569 1.0724713 1.0469221
# only for h = 1 rf better, otherwise worse

# U against rf_without hyper parameter:
U_GDP_oob = theils_U(eval_rf = eval_for_rf_hyperopt_GDP, eval_arma = eval_for_rf_GDPC1, 5)
print(U_GDP_oob)
# 0.9329759 0.9937761 1.1470751 1.0098483 1.1728768
# for h = 1,2 slightly better, for other forecasts worse 




### 2a) GDP GROWTH with test set
### 2b) GDP with test set
##############################################################################################
# using lagged gdp values as regressors: 4 lags
##############################################################################################
### 1) GDP growth
source("06_rf.R")
set.seed(501)
rf_rolling_growth_hyperopt_lag = rf_plain_rolling_hyperopt_lag(df = data, # exclude data column and GDPC1
                                                       gdp = data$GDP_GR, ntrees = 500, forh,
                                                       hyper_para_list = hyper_oob_final_growth)
head(rf_rolling_growth_hyperopt_lag)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_rolling_growth_hyperopt_lag[,(2*j-1)], 
                           se = sd(rf_rolling_growth_hyperopt_lag[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_growth_hyperopt_lag[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_growth_hyperopt_lag[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt_lag = eval_forc_rf(rf_rolling_growth_hyperopt_lag, forh)
print(eval_for_rf_hyperopt_lag)
# $me
# [1] 0.0006840696 0.0009652729 0.0020104620 0.0014769741 0.0015933435
# 
# $mse
# [1] 0.0001044584 0.0002841731 0.0002843995 0.0002477063 0.0002333367
# 
# $mae
# [1] 0.002523138 0.006624914 0.006612777 0.006833809 0.006962310
# 
# $rmse
# [1] 0.01022049 0.01685743 0.01686415 0.01573869 0.01527536
# print(eval_for_rf_hyperopt) 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_growth_opt_lag = dm_tests(gdp = data$GDP_GR[which(data$dates == 2000.00):length(data$dates)], 
                         h_num = 5,
                         result_rf = rf_rolling_growth_hyperopt_lag, result_arma = result_ar_growth)
print(dm_growth_opt_lag)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# h = 0 -> H0 rejected now 

### Theil's U
source("05_functions.R")
U_growth_opt_lag = theils_U(eval_rf = eval_for_rf_hyperopt_lag, eval_arma = eval_for_ar_growth, 5)
print(U_growth_opt_lag)
# 0.7778656 0.9474067 1.0596523 0.9903025 0.9616208
# Values slightly higher compared to eval_for_rf_hyperopt without lag

# U against rf_without hyper parameter:
U_oob_lag = theils_U(eval_rf = eval_for_rf_hyperopt_lag, eval_arma = eval_for_rf_hyperopt, 5)
print(U_oob_lag)
# 1.001384 1.015734 1.000383 1.004677 1.003271
# no improvement

### 2) GDP
source("06_rf.R")
set.seed(501)
rf_rolling_hyperopt_level_lag = rf_GDP_rolling_hyperopt_lag(df = data, gdp = data$GDPC1, ntrees = 500, forh,
                                                    hyper_para_list = hyper_oob_final_level, 
                                                    xi = data$GDPC1[data$dates == 2000.00])
#result_rf_hyperopt_level = feed_in(result = rf_rolling_hyperopt_level, gdp = data$GDPC1, h_max, forh)
head(rf_rolling_hyperopt_level_lag)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDPC1, gdp_forecast = rf_rolling_hyperopt_level_lag[,(2*j-1)], 
                           se = sd(rf_rolling_hyperopt_level_lag[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_hyperopt_level_lag[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_hyperopt_level_lag[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt_GDP_lag = eval_forc_rf(rf_rolling_hyperopt_level_lag, forh)
print(eval_for_rf_hyperopt_GDP_lag)
# $me
# [1]  71.31652 138.44853 493.99935 415.64034 429.92680
# 
# $mse
# [1]  75941.8 119837.5 357556.8 283785.0 295636.5
# 
# $mae
# [1] 182.8559 245.5787 494.6524 424.0400 433.3401
# 
# $rmse
# [1] 275.5754 346.1755 597.9605 532.7147 543.7247

source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_GDP_oob_lag = dm_tests(gdp = data$GDPC1[which(data$dates == 2000.00):length(data$dates)], 
                      h_num = 5,
                      result_rf = rf_rolling_hyperopt_level_lag, result_arma = result_ar)
print(dm_GDP_oob_lag)
# Ho: same forecast accuracy ; H1: rf is more accurate (lower error) than ar
# H0 can be rejected for all h => rf also better

### Theil's U
source("05_functions.R")
U_GDP_opt_lag = theils_U(eval_rf = eval_for_rf_hyperopt_GDP_lag, eval_arma = eval_for_ar, 5)
print(U_GDP_opt_lag)
# 1.0192139 0.6100008 1.4906548 1.0988756 1.0674449
# only for h = 1 rf better, otherwise worse

# U against rf_without hyper parameter:
U_GDP_oob_lag = theils_U(eval_rf = eval_for_rf_hyperopt_GDP_lag, eval_arma = eval_for_rf_hyperopt_GDP, 5)
print(U_GDP_oob_lag)
# 1.0022984 1.0874936 0.9725953 1.0246201 1.0196029
# for h = 1,2 slightly better, for other forecasts worse 

##############################################################################################
# using rangerts, i.e. ordered boostrapping
# using optimal oob hyper parameters from before for each year 
# instead of samp_size, block size is now used
# block_size determines the number of observations per block , default value is 10
# again HYPER PARAMETER TRAINING to get optimal block size (instead samp.size now )
##############################################################################################
mtry_grid = seq(5, ncol(data[,-c(1,2,3)]), 2) # start with 5 split variables, than increase up to p (bagging) by 2
block_size_grid = seq(4,80,4) # from 1 year (4 quarters) to 20 years (80 quarters) per block
node_size_grid = seq(3,9, 2)
### 1) GDP GROWTH oob error

hyper_oob_final_growth_ts = list()

source("06_rf.R")
Sys.time()
start_time = Sys.time()
# for the tuning: using the ranger package: C++ implementation of Breiman rf => computationally more efficient
hyper_oob_final_growth_ts = rf_ranger_oob_ts(df = data, mtry_grid, block_size_grid, node_size_grid, 
                                       500, hyper_para_list = hyper_oob_final_growth_ts)

# saveRDS(hyper_oob_final_growth_ts, file = "output/hyperparams_oob_growth_ts.rda") # save hyper_oob_final 
end_time = Sys.time()
print(paste("estimation time: ", end_time-start_time))


### 1b) GDP
hyper_oob_final_level_ts = list()
source("06_rf.R")
Sys.time()
start_time = Sys.time()
# for the tuning: using the ranger package: C++ implementation of Breiman rf => computationally more efficient
hyper_oob_final_level_ts = rf_ranger_oob_level_ts(df = data, mtry_grid, block_size_grid, node_size_grid, 
                                            500, hyper_para_list = hyper_oob_final_level_ts,
                                            gdp = data$GDPC1)

# saveRDS(hyper_oob_final_level_ts, file = "output/hyperparams_oob_level_ts.rda") # save hyper_oob_final 
end_time = Sys.time()
print(paste("estimation time: ", end_time-start_time))

##############################################################################################
# using optimal hyper parameters now
##############################################################################################
# read opt. hyperparams
hyper_oob_final_growth_ts = readRDS("output/hyperparams_oob_growth_ts.rda") # oob growth
hyper_oob_final_level_ts = readRDS("output/hyperparams_oob_level_ts.rda") # oob GDP

# 1a) GDP GROWTH
source("06_rf.R")
set.seed(501)
rf_rolling_growth_hyperopt_ts = rf_plain_rolling_hyperopt_ts(df = data, # exclude data column and GDPC1
                                                       gdp = data$GDP_GR, ntrees = 500, forh,
                                                       hyper_para_list = hyper_oob_final_growth_ts)
head(rf_rolling_growth_hyperopt_ts)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_rolling_growth_hyperopt_ts[,(2*j-1)], 
                           se = sd(rf_rolling_growth_hyperopt_ts[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_growth_hyperopt_ts[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_growth_hyperopt_ts[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_for_rf_hyperopt_ts = eval_forc_rf(rf_rolling_growth_hyperopt_ts, forh)
print(eval_for_rf_hyperopt_ts)
# $me
# [1] 0.0004869886 0.0010508694 0.0017684765 0.0012594442 0.0011234613
# 
# $mse
# [1] 0.0001023278 0.0002909402 0.0003048436 0.0002595310 0.0002477697
# 
# $mae
# [1] 0.002733769 0.006495636 0.006563469 0.007026814 0.007160545
# 
# $rmse
# [1] 0.01011572 0.01705697 0.01745977 0.01610996 0.01574070

# print(eval_for_rf_hyperopt) 

# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_growth_ts = dm_tests(gdp = data$GDP_GR[which(data$dates == 2000.00):length(data$dates)], 
                     h_num = 5,
                     result_rf = rf_rolling_growth_hyperopt_ts, result_arma = result_ar_growth)
print(dm_growth_ts)



### Theil's U
source("05_functions.R")
U_growth_opt_ts = theils_U(eval_rf = eval_for_rf_hyperopt_ts, eval_arma = eval_for_ar_growth, 5)
print(U_growth_opt_ts)
# 0.7698917 0.9586209 1.0970780 1.0136638 0.9909150


### 1b) GDP 
source("06_rf.R")
set.seed(501)
rf_rolling_GDP_hyperopt_ts = rf_GDP_rolling_hyperopt_ts(df = data, # exclude data column and GDPC1
                                                        gdp = data$GDPC1, ntrees = 500, forh,
                                                        hyper_para_list = hyper_oob_final_growth_ts,
                                                        xi = data$GDPC1[data$dates == 2000.00])
head(rf_rolling_GDP_hyperopt_ts)
# plotting the different forecasts
source("04_plots.R")
par(mfrow = c(1, 1))
for (j in 1:5) {
  gdp_growth_forecast_plot(data$GDP_GR, gdp_forecast = rf_rolling_GDP_hyperopt_ts[,(2*j-1)], 
                           se = sd(rf_rolling_GDP_hyperopt_ts[,(2*j-1)]), 
                           title = paste0("oos_growth_forecasts_rf_plain, h=",j-1), 
                           ylab = "gdp growth", col = "green", 
                           CI = FALSE)
  print(head(rf_rolling_GDP_hyperopt_ts[,(2*j-1)])) # printing first forecast
  print(sd(rf_rolling_GDP_hyperopt_ts[,(2*j-1)])) # printing standard error 
}
# evaluate forecasts
source("05_functions.R")
eval_GDP_rf_hyperopt_ts = eval_forc_rf(rf_rolling_GDP_hyperopt_ts, forh)
print(eval_GDP_rf_hyperopt_ts)
# $me
# [1] -41.20441  91.09950 341.45399 459.37751 386.44674
# 
# $mse
# [1]  58349.91  89784.95 227007.47 334007.03 272165.80
# 
# $mae
# [1] 144.6251 195.0433 380.3036 465.3443 412.3371
# 
# $rmse
# [1] 241.5573 299.6414 476.4530 577.9334 521.6951

# Theil's U and DM TEST 
source("05_functions.R")
### DM test GDP GROWTH forecasts, for each h 
dm_GDP_ts = dm_tests(gdp = data$GDPC1[which(data$dates == 2000.00):length(data$dates)], 
                        h_num = 5,
                        result_rf = rf_rolling_GDP_hyperopt_ts, result_arma = result_ar)
print(dm_GDP_ts)


### Theil's U
source("05_functions.R")
U_GDP_opt_ts = theils_U(eval_rf = eval_GDP_rf_hyperopt_ts, eval_arma = eval_for_ar, 5)
print(U_GDP_opt_ts)
# 0.8933980 0.5280024 1.1877490 1.1921521 1.0241963


##############################################################################################
# plot forecasts of all models for each horizon using ggplot
# for each horizon: all models and gdp 
# once for growth, once for GDP
##############################################################################################
# GDP Growth
source("04_plots.R")
# Growth
# legend
colors_growth = c("GDP growth" = "black", "ARMA" = "blue", "RF-nonTuned" = "green",
           "RF-Tuned" = "purple", "RF-Lags" = "pink", "RF-tsBootstrapping" = "orange")
# h = 0
#pdf(file = "output/Growth_h0.pdf")
final_forecast_plot(data, gdp = data$GDP_GR, arma = result_ar_growth, 
                    rf_nonTunend = rf_plain_rolling_fc, 
                    rf_Tunend = rf_rolling_growth_hyperopt, 
                    rf_lag = rf_rolling_growth_hyperopt_lag, 
                    rf_ts = rf_rolling_growth_hyperopt_ts,
                    h = 0,
                    title = "US GDP Growth Forecasts",
                    horizon = "Horizon h = 0",
                    y_name_GDP = "Growth rate in %",
                    colors = colors_growth,
                    colorname = "GDP growth")
#dev.off()
# h = 1
#pdf(file = "output/Growth_h1.pdf")
final_forecast_plot(data, gdp = data$GDP_GR, arma = result_ar_growth, 
                    rf_nonTunend = rf_plain_rolling_fc, 
                    rf_Tunend = rf_rolling_growth_hyperopt, 
                    rf_lag = rf_rolling_growth_hyperopt_lag, 
                    rf_ts = rf_rolling_growth_hyperopt_ts,
                    h = 1,
                    title = "US GDP Growth Forecasts",
                    horizon = "Horizon h = 1",
                    y_name_GDP = "Growth rate in %",
                    colors = colors_growth,
                    colorname = "GDP growth")
#dev.off()
# h = 2
#pdf(file = "output/Growth_h2.pdf")
final_forecast_plot(data, gdp = data$GDP_GR, arma = result_ar_growth, 
                    rf_nonTunend = rf_plain_rolling_fc, 
                    rf_Tunend = rf_rolling_growth_hyperopt, 
                    rf_lag = rf_rolling_growth_hyperopt_lag, 
                    rf_ts = rf_rolling_growth_hyperopt_ts,
                    h = 2,
                    title = "US GDP Growth Forecasts",
                    horizon = "Horizon h = 2",
                    y_name_GDP = "Growth rate in %",
                    colors = colors_growth,
                    colorname = "GDP growth")
#dev.off()
# h = 3
#pdf(file = "output/Growth_h3.pdf")
final_forecast_plot(data, gdp = data$GDP_GR, arma = result_ar_growth, 
                    rf_nonTunend = rf_plain_rolling_fc, 
                    rf_Tunend = rf_rolling_growth_hyperopt, 
                    rf_lag = rf_rolling_growth_hyperopt_lag, 
                    rf_ts = rf_rolling_growth_hyperopt_ts,
                    h = 3,
                    title = "US GDP Growth Forecasts",
                    horizon = "Horizon h = 3",
                    y_name_GDP = "Growth rate in %",
                    colors = colors_growth,
                    colorname = "GDP growth")
#dev.off()
# h = 4
#pdf(file = "output/Growth_h4.pdf")
final_forecast_plot(data, gdp = data$GDP_GR, arma = result_ar_growth, 
                    rf_nonTunend = rf_plain_rolling_fc, 
                    rf_Tunend = rf_rolling_growth_hyperopt, 
                    rf_lag = rf_rolling_growth_hyperopt_lag, 
                    rf_ts = rf_rolling_growth_hyperopt_ts,
                    h = 4,
                    title = "US GDP Growth Forecasts",
                    horizon = "Horizon h = 4",
                    y_name_GDP = "Growth rate in %",
                    colors = colors_growth,
                    colorname = "GDP growth")
#dev.off()

### GDP
colors_GDP = c("GDP" = "black", "ARMA" = "blue", "RF-nonTuned" = "green",
                  "RF-Tuned" = "purple", "RF-Lags" = "pink", "RF-tsBootstrapping" = "orange")
# h = 0
#pdf(file = "output/GDP_h0.pdf")
final_forecast_plot(data, gdp = data$GDPC1, arma = result_ar, 
                    rf_nonTunend = rf_rolling_GDP_fc, 
                    rf_Tunend = rf_rolling_hyperopt_level, 
                    rf_lag = rf_rolling_hyperopt_level_lag, 
                    rf_ts = rf_rolling_GDP_hyperopt_ts,
                    h = 0,
                    title = "US GDP Forecasts",
                    horizon = "Horizon h = 0",
                    y_name_GDP = "US GDP (Billions of Dollars)",
                    colors = colors_GDP,
                    colorname = "GDP")
#dev.off()
# h = 1
#pdf(file = "output/GDP_h1.pdf")
final_forecast_plot(data, gdp = data$GDPC1, arma = result_ar, 
                    rf_nonTunend = rf_rolling_GDP_fc, 
                    rf_Tunend = rf_rolling_hyperopt_level, 
                    rf_lag = rf_rolling_hyperopt_level_lag, 
                    rf_ts = rf_rolling_GDP_hyperopt_ts,
                    h = 1,
                    title = "US GDP Forecasts",
                    horizon = "Horizon h = 1",
                    y_name_GDP = "US GDP (Billions of Dollars)",
                    colors = colors_GDP,
                    colorname = "GDP")
#dev.off()
# h = 2
#pdf(file = "output/GDP_h2.pdf")
final_forecast_plot(data, gdp = data$GDPC1, arma = result_ar, 
                    rf_nonTunend = rf_rolling_GDP_fc, 
                    rf_Tunend = rf_rolling_hyperopt_level, 
                    rf_lag = rf_rolling_hyperopt_level_lag, 
                    rf_ts = rf_rolling_GDP_hyperopt_ts,
                    h = 2,
                    title = "US GDP Forecasts",
                    horizon = "Horizon h = 2",
                    y_name_GDP = "US GDP (Billions of Dollars)",
                    colors = colors_GDP,
                    colorname = "GDP")
#dev.off()
# h = 3
#pdf(file = "output/GDP_h3.pdf")
final_forecast_plot(data, gdp = data$GDPC1, arma = result_ar, 
                    rf_nonTunend = rf_rolling_GDP_fc, 
                    rf_Tunend = rf_rolling_hyperopt_level, 
                    rf_lag = rf_rolling_hyperopt_level_lag, 
                    rf_ts = rf_rolling_GDP_hyperopt_ts,
                    h = 3,
                    title = "US GDP Forecasts",
                    horizon = "Horizon h = 3",
                    y_name_GDP = "US GDP (Billions of Dollars)",
                    colors = colors_GDP,
                    colorname = "GDP")
#dev.off()
# h = 4
#pdf(file = "output/GDP_h4.pdf")
final_forecast_plot(data, gdp = data$GDPC1, arma = result_ar, 
                    rf_nonTunend = rf_rolling_GDP_fc, 
                    rf_Tunend = rf_rolling_hyperopt_level, 
                    rf_lag = rf_rolling_hyperopt_level_lag, 
                    rf_ts = rf_rolling_GDP_hyperopt_ts,
                    h = 4,
                    title = "US GDP Forecasts",
                    horizon = "Horizon h = 4",
                    y_name_GDP = "US GDP (Billions of Dollars)",
                    colors = colors_GDP,
                    colorname = "GDP")
#dev.off()
# the larger the horizon, the less coming back from financial crises 

