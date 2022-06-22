# Inspecting GDP and estimating ar model 

gdp = ts(data$gdp, start = data$dates[1], frequency = 4) # ts does not support date obejct 
# if you want the character form of date in a time series object, you should consider using xts 
# use xts package for a nicer plot


gdp = ts(data$gdp, start = c(1959,3), frequency = 4) # ts does not support date object 
# The first number in the start parameter is the number of the period depending on the frequency, 
# while the second number is the first incident in that period (as not all series begin at January or at Sunday).

plot(gdp)

# inspecting gdp
# making gdp stationary
# ar process as benchmark?

# non-stationary : follow 1_lecture_globtemp & Tut1 

# add in inspection with periodogram etc. to make gdp stationary .. (if time and needed in the end)

# rw forecast s