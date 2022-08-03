# clean enviroment
rm(list=ls()) # clear out all variables in current session 

# Set wrking directory

# wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"
wd = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"

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

#Dimensions of df: 255 rows (observcations) and 246 variables (excluding the date column)
#55 variables contain NA entries 
#Last value available: "2022-03-01"
#first value available: "3/1/1959"

# Clean Data frame
source("02_data_cleaning.R") # load data-cleaning file
data = clean(df) # list with all relevant component extracted 

##############################################################################################
# plotting gdp
source("04_plots.R")
gdp_plot(data$gdp_raw, title = "GDP", ylab = "GDP")
gdp_plot(data$df_trans[,2], title = "GDP growth rate", ylab = "log returns")

# in sample data
# Forecasting 88 quarters
h_max = 88 # forecast horizon: 4 quarters of 22 years (i.e. 88 quarters)

data_in = in_out_sample(df = data$df_trans, gdp = data$gdp_raw, h_max)
gdp_plot(data_in$gdp_raw_in,  title = "GDP in sample", ylab = "GDP")

##############################################################################################
# estimating benchmark models and doing simple forecast (one iteration)
source("03_benchmark.R") 

# using entire data series
ar(data$gdp_raw, 1,1, h_max)
# Dickey Fuller test for the 1st different: p = 0.01 < 0.05 => can reject H0 (ts stationary)
# Using AR(1,1) model based on acf and pacf
# coefficients of the AR model do not seem to be significant
# residuals of AR model seem to be white noise according to LjungBoxPierce test, 
# but qq-plot shows somewhat systematic deviation
# 2 large outliers due to covid pandemic:

### using only in-sample data
# 1st order differencing gdp
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
# p-value mostly larger than 0.01 => residuals seem to be white niose


# fitting a random walk model
rw = ar(data_in$gdp_raw_in, 0,0, h_max)
# residuals no white-noise: ar_models better

## forecasting gdp growth
ar_22_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 2, ma_ord = 2, h_max)
# pacf suggests ar order of 2, acf suggests ma order of 2
# but ma2 coefficient not significant 
# errors seem to be white noise
# rss       aic      aicc       bic
# 0.0119434 -9.472247 -9.470371 -9.396326

# fitting ar_21_model
ar_21_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 2, ma_ord = 1, h_max)
# now all parameter very significant. 
# erros on the edge on being white noise or non-white noise
# information criteria a little bit higher (slightly)
# rss       aic      aicc       bic
# 0.01211922 -9.469903 -9.468738 -9.412963

ar_11_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 1, ma_ord = 1, h_max)
# both parameters significant
# slightly best information criteria (lowest)
# rss       aic      aicc       bic
# 0.01280727 -9.426953 -9.426336 -9.388993
# erros slightly white-noise


rw_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 0, ma_ord = 0, h_max)
# white-noise hypothesis can be rejected (errors not white-noise)

## using ar_11_growth in the following 

# plotting predictions
source("04_plots.R")
# plot growth predictions
gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = ar_11_growth$predicitons$pred, se = ar_11_growth$predicitons$se, 
                         "oos_growth_forecasts_ar11", ylab = "gdp growth", col = "blue")
gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = rw_growth$predicitons$pred, se = rw_growth$predicitons$se, 
                         "oos_growth_forecasts_rw", ylab = "gdp growth", col = "red")
# plot GDP predictions
gdp_forecast_plot(data$gdp_raw, ar_11$predicitons_inverted, "oos_forecasts_ar11", ylab = "gdp", col = "blue") 
gdp_forecast_plot(data$gdp_raw, rw$predicitons_inverted, "oos_forecasts_rw", ylab = "gdp", col = "red")


## using ts decomposition


##############################################################################################
# estimating benchmark models recursively: using ar11 based on results above
source("03_benchmark.R") 
source("05_functions.R")
# procedure:
# construcing result table: each 2nd column contains h = 1,2,3,4 forecast, 
# insert true gdp values in every 2nd column next to forecast column
# compute forecast errors based on result table

forh = c(1,2,3,4) # forecast horizon -> include h = 0 ????????????????

# computing forecasts iteratively
result_ar = ar_growth_rolling(data$df_trans[,2], ar_ord = 2, ma_ord = 2, h_max, forh)

# feed in true values
result_ar = feed_in(result = result_ar, gdp = data$df_trans[,2], h_max, forh)
View(result_ar)

# plotting the different forecasts
source("04_plots.R")
for (j in 1:4) {
  gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = result_ar[,(2*j-1)], 
                           se = sd(result_ar[,(2*j-1)]), 
                           paste0("oos_growth_forecasts_ar11_h=",j), 
                           ylab = "gdp growth", col = "blue")
  print(head(result_ar[,(2*j-1)]))
  print(sd(result_ar[,(2*j-1)]))
}


# Forecast evaluation
# evaluate forecasts using: ME, MAE, MSE
# and possibly some tests:Diebold - Marino, Superior predictive ability test, model confidence sets
source("05_functions.R")
eval_forc(result_ar, forh)

############################################################################
# estimate plain rf and forecast

# create train (in_sample) and test (out_of_sample) dataframe
X_in = data_in$insample_dataframe[,-(1:2)]
y_in = data_in$insample_dataframe[,2]

X_out = data$df_trans[( (dim(data$df_trans)[1]-h_max + 1):(dim(data$df_trans)[1]) ), -(1:2)] # last 88 rows
y_out = data$df_trans[(dim(data$df_trans)[1]-h_max + 1:dim(data$df_trans)[1]),2]

source("06_rf.R")
rf_plain = rf_plain(X_in, y_in, oos_dataframe =X_out, 
                    # test_data is out_of_sample data
                    mtry = sqrt(dim(X_in)[2]), # preditors used is square root of predictors
                    ntrees = 8000)

length(rf_plain$plain_forest_pred)
rf_plain$plain_forest_pred

