### EDA for Time Series
library(xts)
library(astsa)

# Norwegian Salmon
summary(fit <- lm(salmon~time(salmon), na.action=NULL))
tsplot(salmon, col=4, ylab="USD per KG", main="Salmon Export Price")
abline(fit)
time(salmon)
head(salmon, 1)

# Example of Pollution, Temperature and Mortality

##-- separate
par(mfrow=c(3,1))
tsplot(cmort, main="Cardiovascular Mortality", col=6, type="o", pch=19, ylab="")
tsplot(tempr, main="Temperature", col=4, type="o", pch=19, ylab="")
tsplot(part, main="Particulates", col=2, type="o", pch=19, ylab="")

##-- together 
dev.new()
tsplot(cmort, ylab="", ylim=c(20,130), col=astsa.col(6,.8))
lines(tempr, col=astsa.col(4,.9))
lines(part, col=astsa.col(2,.8))
legend("topright", legend=c("Mortality", "Temperature", "Pollution"), lty=1, lwd=2, col=c(6,4,2), bg="white")

##-- scatterplot matrix
dev.new()  
panel.cor <- function(x, y, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), 2)
  text(0.5, 0.5, r, cex = 1.75)
}
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part), col=4, lower.panel=panel.cor)

######## Checking colinearty 
par(mfrow=2:1)
plot(tempr, tempr^2)
cor(tempr, tempr^2)

temp  = tempr-mean(tempr)
plot(temp, temp^2)
cor(temp, temp^2)

#  Regression
temp  = tempr-mean(tempr)  # center temperature    
temp2 = temp^2             # square it  
trend = time(cmort)        # time

fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)

summary(fit)       # regression results
summary(aov(fit))  # ANOVA table   (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 

num = length(cmort)                                     # sample size
AIC(fit)/num - log(2*pi)                                # AIC 
BIC(fit)/num - log(2*pi)                                # BIC   

# Including Lagging Variables
par(mfrow=c(1,1))
fish = ts.intersect( rec, soiL6=lag(soi,-6) )
summary(fit1 <- lm(rec~ soiL6, data=fish, na.action=NULL))
tsplot(resid(fit1), col=4) # residuals

#install.packages("dynlm")
library(dynlm)
summary(fit2 <- dynlm(rec~ L(soi,6)))

#### EDA and Trend Stationary
# Detrending 
fit = lm(salmon~time(salmon), na.action=NULL) # the regression
# plot transformed data
tsplot(resid(fit), main="detrended salmon price")
acf1(resid(fit), 48, main="detrended salmon price")

# Differencing
tsplot(diff(salmon), main="differenced salmon price")
acf1(diff(salmon), 48, main="differenced salmon price")

#comparison
fit = lm(salmon~time(salmon), na.action=NULL) # the regression
par(mfrow=c(2,1)) # plot transformed data
tsplot(resid(fit), main="detrended salmon price")
tsplot(diff(salmon), main="differenced salmon price")
dev.new()
par(mfrow=c(2,1)) # plot their ACFs
acf1(resid(fit), 48, main="detrended salmon price")
acf1(diff(salmon), 48, main="differenced salmon price")
par(mfrow=c(1,1))

### Global Temperature Data
par(mfrow=c(2,1))
tsplot(diff(gtemp_land), col=4, main="differenced global tmeperature")
mean(diff(gtemp_land)) # drift since 1880

acf1(diff(gtemp_land))
mean(window(diff(gtemp_land), start=1980)) # drift since 1980
par(mfrow=c(1,1))

### Lagplots
lag1.plot(soi, 12, col=4, cex = 1)
lag2.plot(soi, rec, 8, col=4, cex=1)

# Regression with Lag
library(zoo)   # zoo allows easy use of the variable names
dummy = ifelse(soi<0, 0, 1) 
fish = as.zoo(ts.intersect(rec, soiL6=lag(soi,-6), dL6=lag(dummy,-6)))
summary(fit <- lm(rec~ soiL6*dL6, data=fish, na.action=NULL))
plot(fish$soiL6, fish$rec, panel.first=Grid(), col='dodgerblue3')
points(fish$soiL6, fitted(fit), pch=3, col=6) 
lines(lowess(fish$soiL6, fish$rec), col=4, lwd=2)
tsplot(resid(fit))
acf1(resid(fit))      # and obviously not noise        


### Smoothing
# Moving Average
w = c(.5, rep(1,11), .5)/12
soif = filter(soi, sides=2, filter=w)
tsplot(soi, col=astsa.col(4,.7), ylim=c(-1, 1.15))
lines(soif, lwd=2, col=4)
# insert
par(fig = c(.65, 1, .75, 1), new = TRUE)
w1 = c(rep(0,20), w, rep(0,20))
plot(w1, type="l", ylim = c(-.02,.1), xaxt="n", yaxt="n", ann=FALSE)
par(oldpar)
dev.off() # reset of graphing parameters

# Kernel
tsplot(soi, col=astsa.col(4,.7), ylim=c(-1, 1.15))
lines(ksmooth(time(soi), soi, "normal", bandwidth=12), lwd=2, col=4)
# insert
par(fig = c(.65, 1, .75, 1), new = TRUE)
gauss <- function(x) { 1/sqrt(2*pi) * exp(-(x^2)/2) }
curve(gauss(x), -3, 3, xaxt="n", yaxt="n", ann=FALSE)
dev.off() # reset of graphing parameters
# 
SOI = ts(soi, freq=1)
tsplot(SOI, col=8) # the time scale matters (not shown)
lines(ksmooth(time(SOI), SOI, "normal", bandwidth=1), lwd=2, col=4)

# Lowess
tsplot(soi, col=astsa.col(4,.6))
lines(lowess(soi, f=.05), lwd=2, col=4) # El Niño cycle
# lines(lowess(soi), lty=2, lwd=2, col=2) # trend (with default span)
#- or -#
##-- trend with CIs using loess --##
lo = predict(loess(soi ~ time(soi)), se=TRUE)
trnd = ts(lo$fit, start=1950, freq=12) # put back ts attributes
lines(trnd, col=6, lwd=2)
L  = trnd - qt(0.975, lo$df)*lo$se
U  = trnd + qt(0.975, lo$df)*lo$se
xx = c(time(soi), rev(time(soi)))
yy = c(L, rev(U))
polygon(xx, yy, border=8, col=gray(.6, alpha=.4) )

# As a function of another series
tsplot(tempr, cmort, type='p', pch=19, xlab="Temperature", ylab="Mortality", col=4)
lines(lowess(tempr,cmort), col=6, lwd=2)

#### Classical Structural Modeling'
x = window(hor, start=2002)
plot(decompose(x))
plot(stl(x, s.window="per"))
plot(stl(x, s.window=15))  
##

culer = c(5, 4, 2, 6)
x = window(hor, start=2002)
par(mfrow = c(4,1), cex.main=1)
out = stl(x, s.window=15)$time.series
tsplot(x, main='Hawaiian Occupancy Rate', ylab='% rooms', col=8)
text(x, labels=1:4, col=culer, cex=1.25)
tsplot(out[,1], main="Seasonal", ylab='% rooms',col=8)
text(out[,1], labels=1:4, col=culer, cex=1.25)
tsplot(out[,2], main="Trend", ylab='% rooms', col=8)
text(out[,2], labels=1:4, col=culer, cex=1.25)
tsplot(out[,3], main="Noise", ylab='% rooms', col=8)
text(out[,3], labels=1:4, col=culer, cex=1.25)

