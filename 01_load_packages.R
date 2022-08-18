# install packages
install_and_load = function(a){
  # install packages
  if(a == TRUE){
    install.packages("randomForest")
    install.packages("tseries")
    install.packages("tsapp")
    install.packages("forecast")
    install.packages("ggplot2")
    #install.packages("mFilter")
    
    # for using github:
    #install.packages("usethis")
    #install.packages("devtools")
    #library(devtools) # devools needed to install directly from github
    #install_github("cykbennie/fbi") # R Package to read QD and MD data -> install from github
    
  } 
  # load packages
  
  library(randomForest)
  library(tsapp)
  library(tseries)
  library(forecast)
  library(ggplot2)
  
  #library(mFilter)
  # github
  #library(devtools)
  #library(fbi)
  return("Packages loaded")
}

