# Correlation and Stationary Time Sereis
library(xts)
library(astsa)

# ACF for a moving average
ACF = c(0,0,0,1,2,3,2,1,0,0,0)/3
LAG = -5:5
tsplot(LAG, ACF, type="h", lwd=3, xlab="LAG")   
#abline(h=0)
points(LAG[-(4:8)], ACF[-(4:8)], pch=20)
axis(1, at=seq(-5, 5, by=2))

# Price of Salmon
summary(fit <- lm(salmon~time(salmon), na.action=NULL))
tsplot(salmon, col=4, ylab="USD per KG", main="Salmon Export Price")
abline(fit)

# Using CCF for Prediction
x = rnorm(100)
y = lag(x,-5) + rnorm(100)
ccf2(y, x, ylab="CCovF", type="covariance")

# Estimating ACF
(r = round( acf1(soi, 6, plot=FALSE), 2)) # sample acf values
# [1] 0.60 0.37 0.21 0.05 -0.11 -0.19
par(mfrow=c(1,2))
tsplot(lag(soi,-1), soi, col=4, type='p', xlab='lag(soi,-1)')
legend("topleft", legend=r[1], bg="white", adj=.45, cex = 0.85)
tsplot(lag(soi,-6), soi, col=4, type='p', xlab='lag(soi,-6)')
legend("topleft", legend=r[6], bg="white", adj=.25, cex = 0.8)
par(mfrow=c(1,1))

# Example
set.seed(101011)
x1 = sample(c(-2,2), 11, replace=TRUE) # simulated coin tosses
x2 = sample(c(-2,2), 101, replace=TRUE)
y1 = 5 + filter(x1, sides=1, filter=c(1,-.5))[-1]
y2 = 5 + filter(x2, sides=1, filter=c(1,-.5))[-1]
tsplot(y1, type="s", col=4, xaxt="n", yaxt="n") # y2 not shown
axis(1, 1:10); axis(2, seq(2,8,2), las=1)
points(y1, pch=21, cex=1.1, bg=6)

tsplot(y2, type="s", col=4, xaxt="n", yaxt="n") # y1 not shown
axis(1, 1:100); axis(2, seq(2,8,2), las=1)
points(y2, pch=21, cex=1.1, bg=6)

acf(y1, lag.max=4, plot=FALSE) 
acf(y2, lag.max=4, plot=FALSE)

# Joint SOI and Recruitment

par(mfrow=c(3,1))
acf1(soi, 48, main="Southern Oscillation Index")
acf1(rec, 48, main="Recruitment")
ccf2(soi, rec, 48, main="SOI vs Recruitment")
par(mfrow=c(1,1))

# Prewhitening a Time Sereis

set.seed(1492)
num = 120
t = 1:num
X = ts( 2*cos(2*pi*t/12) + rnorm(num), freq=12 )
Y = ts( 2*cos(2*pi*(t+5)/12) + rnorm(num), freq=12 )
Yw = resid(lm(Y~ cos(2*pi*t/12) + sin(2*pi*t/12), na.action=NULL))
par(mfrow=c(3,2))
tsplot(X)
tsplot(Y)
acf1(X, 48)
acf1(Y, 48)
ccf2(X, Y, 24)
ccf2(X, Yw, 24, ylim=c(-.6,.6))
par(mfrow=c(1,1))
