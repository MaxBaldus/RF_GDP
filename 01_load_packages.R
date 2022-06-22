# install packages
install = function(a){
  
  if(a == TRUE){
    
    install.packages("devtools")
    install_github("cykbennie/fbi") # R Package to read QD and MD data
    install.packages("randomForest")
    install.packages("xts")
    
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
  library(xts)
  
  return("Packages loaded")
}

