# clean enviroment
rm(list=ls()) # clear out all variables in current session 

# Set wrking directory

wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"
#wd = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"

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
data_in = in_out_sample(df = data$df_trans, gdp = data$gdp_raw)
gdp_plot(data_in$gdp_raw_in,  title = "GDP in sample", ylab = "GDP")

##############################################################################################
# estimating benchmark models and doing simple forecast (one iteration)
source("03_benchmark.R") 

# using entire data series
ar(data$gdp_raw, 1,1)
# Dickey Fuller test for the 1st different: p = 0.01 < 0.05 => can reject H0 (ts stationary)
# Using AR(1,1) model based on acf and pacf
# coefficients of the AR model do not seem to be significant
# residuals of AR model seem to be white noise according to LjungBoxPierce test, but qq-plot shows somewhat systematic deviation
# 2 large outliers due to covid pandemic:

# using only in-sample data
ar_21 = ar(data_in$gdp_raw_in,2,1)
# acf yields ma of order 2, pacf indicate long memory?
# using ar(2,1) -> to singifincant coefficient, but with ar(1,1) parameters significant
# residuals seems to be white noise: confirmend by Ljung Box pierce test and normal qq-plot

# fitting a random walk model
rw = ar(data_in$gdp_raw_in, 0,0)

# forecasting gdp growth
ar_22_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 2, ma_ord = 2)
# pacf suggests ar order of 2, acf suggests ma order of 2
# but ma2 coefficient not significant 
# errors seem to be white noise

rw_growth = ar_growth(data_in$insample_dataframe[,2], ar_ord = 0, ma_ord = 0)

# plotting predictions
source("04_plots.R")
# plot growth predictions
gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = ar_22_growth$predicitons$pred, se = ar_22_growth$predicitons$se, 
                         "oos_growth_forecasts_ar22", ylab = "gdp growth", col = "blue")
gdp_growth_forecast_plot(data$df_trans[,2], gdp_forecast = rw_growth$predicitons$pred, se = rw_growth$predicitons$se, 
                         "oos_growth_forecasts_rw", ylab = "gdp growth", col = "red")
# plot GDP predictions
gdp_forecast_plot(data$gdp_raw, ar_21$predicitons_inverted, "oos_forecasts_ar21", ylab = "gdp", col = "blue") 
gdp_forecast_plot(data$gdp_raw, rw$predicitons_inverted, "oos_forecasts_rw", ylab = "gdp", col = "red")


# Dickey Fuller test for stl
# using stl somehow??
# - trend removed via 1.st order differencing
# - PACF and ACF suggest using a ARMA[1,1] model

##############################################################################################
# estimating benchmark models recursively 
source("03_benchmark.R") 






# Forecast evaluation
# -> evaluate forecasts using:MAE, MSE
# and possibly some tests:Diebold - Marino, Superior predictive ability test, model confidence sets
# -> into extra file:help functions ?  ? 
  
  
  