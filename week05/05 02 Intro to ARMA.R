#### ARMA 
library(xts)
library(astsa)

# AR
par(mfrow=c(2,1))
tsplot(sarima.sim(ar= .9, n=100), ylab="x", col=4, main=expression(AR(1)~~~phi==+.9))
tsplot(sarima.sim(ar=-.9, n=100), ylab="x", col=4, main=expression(AR(1)~~~phi==-.9))
par(mfrow=c(1,1))

#Causality
psi = ARMAtoMA(ar = c(1.5, -.75), ma = 0, 50)
par(mfrow=c(2,1))
tsplot(psi, col=4, type='o', pch=19, ylab=expression(psi-weights), xlab='Index', 
       main=expression(AR(2)~~~phi[1]==1.5~~~phi[2]==-.75))
set.seed(8675309)
simulation = sarima.sim(ar=c(1.5,-.75), n=144, S=12)
tsplot(simulation, ylab=expression(X[~t]), col=4, xlab='Year', lwd=2)

#MA
par(mfrow = c(2,1))
tsplot(sarima.sim(ma= .9, n=100), col=4, ylab="x", main=expression(MA(1)~~~theta==+.9))
tsplot(sarima.sim(ma=-.9, n=100), col=4, ylab="x", main=expression(MA(1)~~~theta==-.9))
par(mfrow=c(1,1))

# Redundancy and Estimation
set.seed(8675309)         
x = rnorm(150, mean=5)    # generate iid N(5,1)s
arima(x, order=c(1,0,1))  # estimation

# Checking for Redundancy
AR = c(1, -.3, -.4) # original AR coefs on the left
polyroot(AR)
MA = c(1, .5)       # original MA coefs on the right
polyroot(MA)

# Causal and Invertible Forms
round( ARMAtoMA(ar=.8, ma=-.5, 10), 2) # first 10 psi-weights
round( ARMAtoAR(ar=.8, ma=-.5, 10), 2) # first 10 pi-weights
ARMAtoMA(ar=1, ma=0, 20)

# ACF of and AR(2)
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
tsplot(ACF, type="h", xlab="lag")
abline(h=0, col=8)

# PACF
ACF  = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=1:2)
tsplot(ACF, type="h", xlab="lag", ylim=c(-.8,1))
abline(h=0, col=8)
tsplot(PACF, type="h", xlab="lag", ylim=c(-.8,1))
abline(h=0, col=8) 
par(mfrow=c(1,1))

# Preliminary Analysis of Recruitment
acf2(rec, 48)     # will produce values and a graphic
(regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE))
regr$asy.se.coef  # standard errors of the estimates
