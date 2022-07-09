# install packages
install = function(a){
  
  if(a == TRUE){
    
    install.packages("usethis")
    install.packages("devtools")
    library(devtools) # devools needed to install directly from github
    install_github("cykbennie/fbi") # R Package to read QD and MD data -> install from github
    install.packages("randomForest")
    # install.packages("xts")
    install.packages("mFilter")
    install.packages("tsapp")
    
    
    return("Packages installed")
    
  } else{
    return("No Packages installed")
  }
}

# load packages
load_pack = function(){
  
  library(devtools)
  library(fbi)
  library(randomForest)
  # library(xts)
  library(mFilter)
  library(tsapp)
  
  
  return("Packages loaded")
}


