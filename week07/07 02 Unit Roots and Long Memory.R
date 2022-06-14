# 07 02 Unit Root Testing and Long Memory
library(astsa)
library(xts)

# Unit Roots
acf1(cumsum(rnorm(634)), 100, main="Series: random walk")
acf1(log(varve), 100, ylim=c(-.1,1))

#Glacial Varve example
# install.packages("tseries")
library(tseries)
adf.test(log(varve), k=0) # DF test
adf.test(log(varve))      # ADF test
pp.test(log(varve))       # PP test


# Long Memory and Fracotpma; Differenceing
round(ARMAtoMA(ar=c(1.23, -.23), ma=c(1, -.89), 20), 3)

#install.packages("arfima")
library(arfima)
summary(varve.fd <- arfima(log(varve), order = c(0,0,0)))
# residuals
innov = resid(varve.fd)
acf1(resid(sarima(log(varve),1,1,1, details=FALSE)$fit), main="ARIMA(1,1,1)")
acf1(innov[[1]], main="Frac Diff")


