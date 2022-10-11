# install packages
install_and_load = function(a){
  # install packages
  if(a == TRUE){
    install.packages("randomForest")
    install.packages("tseries")
    install.packages("tsapp")
    install.packages("forecast")
    install.packages("ggplot2")
    install.packages("ranger")
    install.packages("readxl")
    install.packages("openxlsx")
    install.packages("tidyverse")
    install.packages("mFilter")
    install.packages("dplyr")
    # installing from github
    install.packages("usethis")
    install.packages("devtools")
    library(devtools) # devools needed to install directly from github
    devtools::install_github("hyanworkspace/rangerts", quiet = T) # boostrapping for ts
    
  } 
  # load packages
  library(randomForest)
  library(tsapp)
  library(tseries)
  library(forecast)
  library(ggplot2)
  library(ranger)
  library(readxl)
  library(tidyverse)
  library(mFilter)
  library(rangerts)
  library(dplyr)
  
  return("Packages loaded")
}

