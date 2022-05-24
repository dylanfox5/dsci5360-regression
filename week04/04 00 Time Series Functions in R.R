#### Time Series Functions in R ####

### Base R ###
# Generate some data
dat <- c(1, 2, 5, 3, 1)
dat
#Make a time series
tsdat <- ts(dat, start = 2010)   #assumed frequency is 1 year
tsdat    

#To specify a quarterly frequency
tsdat <- ts(dat, start = c(2010, 3), frequency = 4)   #quarterly data beginning in quarter III of 2010
tsdat

#Viewing the sampled times
time(tsdat)

#Commands related to ts() include as.ts() and is.ts()
is.ts(tsdat)

#Using part of a time series: window()
x <- window(tsdat, start=c(2010, 3), end=c(2011, 1))
x

### in astsa  ###
#install.packages("astsa")
library(astsa)
tsplot(tsdat)

#Attibutes of a ts datafile
tsp(UnempRate)
### [1] 1948.000     2016.833   12.000
#       start         end        freq
#       Jan 1948     Nov 2016    monthly

## Lagging and Differencing

#Lag

x <- ts(1:5)
cbind(x, lag(x), lag(x,-1))   # Lag() is forward, Lag( , -1) backward

ts.intersect(x, lag(x), lag(x,-1))

#differencing

diff(x)       # difference between every second point
diff(x, 2)
diff(diff(x))
diff(x, diff=2)
