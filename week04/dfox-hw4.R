### dsci 5360 week 4 HW
library(xts)
library(astsa)

## problem 1
set.seed(55)
par(mfrow=c(2,1))

w = rnorm(100)

x = filter(w, filter=c(0, -0.9), method="recursive")
tsplot(x, main="autoregression", col=4)

v = filter(w, sides=2, filter=rep(1/4,4))
tsplot(v, col=4, main="moving average")

# the autoregression plot shows a lot of the noise and extremes of the data
# the moving average plot normalizes the y-value at 0 and shows less
# of the data extremes but it's evident that it follows the same trend

## problem 2
data(EQ5)
data(EXP6)

# a
tsp(EQ5)
tsp(EXP6)

# both files have the same start and end date, as well as frequency

# b
par(mfrow=c(2,1))
tsplot(EQ5)
tsplot(EXP6)

# c
par(mfrow=c(1,1))
tsplot(EQ5, col=4)
lines(EXP6, col=2)

# d
# these data sets are different when it comes to the cause of seismic activity. with that in mind,
# we can see that these time series plots make sense. there is more data of seismic activity
# with earthquakes as the cause because humans have been aware of this much longer than nuclear explosions.
# we can see that illustrated early on when there is more noise within the earthquake data. in addition, 
# we can see where nuclear explosions became much more prevalent in our society as technology advanced.

## problem 3
# stationarity is important because it allows us to provide accurate statistical predictions reliably. 
# it means that over the course of a time series data set, the statistical properties are regular and do not
# change. this allows us to make those accurate predictions

## problem 4

# a
# due to B0 and B1 are fixed coefficients and W1 is white noise, Xt is stationary.

# b
# yt = (B0 + B1t + wt) - (B0 + B1t-1 + wt-1)
# yt = B1t - B1t-1 + wt - wt-1

# c
# vt = 1/3 (Xt-1 + Xt + Xt+1)
# vt = 1/3 [(B0 + B1t-1 + wt-1) + (B0 + B1t + wt) + (B0 + B1t+1 + wt+1)]
# vt = 1/3 [3B0 + B1t-1 + B1t + B1t+1 + wt-1 + wt + wt+1]
# vt = B0 + B1t

## problem 5
w = rnorm(500)
tsplot(w, col=4, main="white noise")

(r = round( acf1(w, 6, plot=FALSE), 2))
par(mfrow=c(1,2))
tsplot(lag(w,-1), w, col=4, type='p', xlab='lag(w,-1)')
legend("topleft", legend=r[1], bg="white", adj=.45, cex = 0.85)
tsplot(lag(w,-6), w, col=4, type='p', xlab='lag(w,-6)')
legend("topleft", legend=r[6], bg="white", adj=.25, cex = 0.8)
par(mfrow=c(1,1))

acf(w, lag.max=4, plot=FALSE) 

w = rnorm(50)
tsplot(w, col=4, main="white noise")

(r = round( acf1(w, 6, plot=FALSE), 2))
par(mfrow=c(1,2))
tsplot(lag(w,-1), w, col=4, type='p', xlab='lag(w,-1)')
legend("topleft", legend=r[1], bg="white", adj=.45, cex = 0.85)
tsplot(lag(w,-6), w, col=4, type='p', xlab='lag(w,-6)')
legend("topleft", legend=r[6], bg="white", adj=.25, cex = 0.8)
par(mfrow=c(1,1))

acf(w, lag.max=4, plot=FALSE) 
