# install packages
install = function(a){
  
  if(a == TRUE){
    install.packages("randomForest")
    
    return("Packages installed")
  }
}

# load packages
load_pack = function(){
  
  library(randomForest)
  
  return("Packages loaded")
}


