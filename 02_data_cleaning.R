# load data from Fred 

# everything as a function 
inspect = function(df){
  # df = read.csv("input/..")
  
  l = list(head(df), str(df)) # put everything into list 
  return(l)
}

#read_csv()   
# print(head(cars))
# print(str(cars))

plot(cars)