---
  title: "R Notebook"
output:
  ---
  
  ### put everything in R file ?? 
  
  Remove everything
```{r}
rm(list=ls()) # clear out all variables in current session 
setwd("~")
```

Set wd
```{r}
wd = "~/Dokumente/CAU/WS_22_23/Seminar/Code/RF_GDP"
# wd_office = "C:/Users/admin/Desktop/Max/RF_GDP/RF_GDP"
# wd = ""  # enter you wd

setwd(wd)
```

Load Packages
```{r}
source("01_load_packages.R")  # load package file

install(a = FALSE) # set a to TRUE if you want to install packages in 01_load_packages.R file
load_pack() # loading all packages after installation
```

Load Data
```{r}
df = read.csv("input/current.csv") # load dataframe 
```

Inspect the raw Data
```{r}
source("02_data_cleaning.R") # load data-cleaning file
inspect(df)
```

Dimensions of df: 255 rows and 246 variables (excluding the date column)
55 variables contain NA's 

Clean Data frame
```{r}
source("02_data_cleaning.R") # load data-cleaning file
data = clean(df) # list with all relevant component extracted 
```


helper:
```{r}
# see which variables of the dataframe are in the fred description
varlist_df %in% varlist_fred$fred # returns a boolean TRUE or FALSE value depending on whether the element is found or not
which(varlist_df %in% varlist_fred$fred == FALSE) # gives the indices, that are false
varlist_df[c(which(varlist_df %in% varlist_fred$fred == FALSE))]
```
Last value available: "2022-03-01"



AR & RW's with different windows as benchmark:
  ```{r}
source("03_benchmark.R") # load data file

benchmarks_fit = ar(gdp_raw)

rw_fit = benchmarks_fit$rw_fit
arma_fit = benchmarks_fit$arma_fit

rw_fit
arma_fit

```
- trend removed via 1.st order differencing
- PACF and ACF suggest using a ARMA[1,1] model

Rolling window -> also for RW..





Forecast evaluation
-> evaluate forecasts using:
  MAE, MSE
and possibly some tests: Diebold-Marino, Superior predictive ability test, model confidence sets
-> into extra file: help functions??
  
  
  