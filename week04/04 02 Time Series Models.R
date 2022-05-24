# Time Series Models
library(xts)
library(astsa)

# Noise
w = rnorm(500) # 500 N(0,1) variates
tsplot(w, col=4, main="white noise")

#Moving Average
v = filter(w, sides=2, filter=rep(1/3,3)) # moving average
tsplot(v, ylim=c(-3,3), col=4, main="moving average")

#Autoregression
set.seed(90210)
w = rnorm(250 + 50) # 50 extra to avoid startup problems
x = filter(w, filter=c(1.5,-.75), method="recursive")[-(1:50)]
tsplot(x, main="autoregression", col=4)

# Random Walk
set.seed(314159265)
w  = rnorm(200) 
x  = cumsum(w)
wd = w +.3 
xd = cumsum(wd)
tsplot(xd, ylim=c(-2,80), main="random walk", ylab="", col=4)
clip(0, 200, 0, 80)
abline(a=0, b=.3, lty=2, col=4) # drift
lines(x, col=6)
clip(0, 200, 0, 80)
abline(h=0, col=6, lty=2)



# fNMR
# cs = 2*cos(2*pi*(1:500)/50 + .6*pi)    # as in the text *** The Signal
cs = 2*cos(2*pi*(1:500+15)/50)           # same thing     *** The Signal
w  = rnorm(500,0,1)                                     #*** The Noise
par(mfrow=c(3,1))   
tsplot(cs, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)))
tsplot(cs + w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,1)))
tsplot(cs + 5*w, ylab="", main = expression(x[t]==2*cos(2*pi*t/50+.6*pi)+N(0,25)))
par(mfrow=c(1,1))
