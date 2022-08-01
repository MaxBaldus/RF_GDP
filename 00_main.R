# clean enviroment
rm(list=ls()) # clear out all variables in current session 

# Set wrking directory

#wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"
wd = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"

#wd = ""  # enter you wd

setwd(wd)

# Load Packages
source("01_load_packages.R")  # load package file
install_and_load(a = FALSE) # set a to TRUE if you want to install packages in 01_load_packages.R file


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

# plotting gdp
source("04_plots.R")
gdp_plot(data$gdp_raw, title = "GDP", ylab = "GDP")
gdp_plot(data$df_transformend[,2], title = "GDP growth rate", ylab = "log returns")

# in sample data
data_in = in_out_sample(df = data$df_transformend, gdp = data$gdp_raw)
gdp_plot(data_in$gdp_raw_in,  title = "GDP in sample", ylab = "GDP")

# estimating benchmark models
source("03_benchmark.R") 

# using entire data series
ar(data$gdp_raw, 1,1)

# Dickey Fuller test for the 1st different: p = 0.01 < 0.05 => can reject H0 (ts stationary)
# Using AR(1,1) model based on acf and pacf
# coefficients of the AR model do not seem to be significant
# residuals of AR model seem to be white noise, but qq-plot shows somewhat systematic deviation
# 2 large outliers with due to covid pandemic:

# using only in-sample data
ar(data_in$gdp_raw_in,2,1)
# acf yields ma of order 2, pacf indicate long memory?
# using ar(2,1)


benchmarks_fit = ar(data$gdp_raw)

# Dickey Fuller test for stl

rw_fit = benchmarks_fit$rw_fit
arma_fit = benchmarks_fit$arma_fit

rw_fit
arma_fit

# - trend removed via 1.st order differencing
# - PACF and ACF suggest using a ARMA[1,1] model

# Rolling window -> also for RW..





# Forecast evaluation
# -> evaluate forecasts using:MAE, MSE
# and possibly some tests:Diebold - Marino, Superior predictive ability test, model confidence sets
# -> into extra file:help functions ?  ? 
  
  
  