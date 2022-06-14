#### 07 01 GARCH and the Family

library(astsa)
library(xts)

# DJIA

djia_return = diff(log(djia$Close))[-1]
plot(djia_return, col=4)
acf1(djia_return)
acf1(djia_return^2)

#GNP ARCH ACF/PACF

res = resid( sarima(diff(log(gnp)), 1,0,0, details=FALSE)$fit )
acf2(res^2, 20)

#install.packages("fGarch")
library(fGarch)
gnpr = diff(log(gnp))
summary( garchFit(~arma(1,0) + garch(1,0), data = gnpr) )

# GARCH
djiar = diff(log(djia$Close))[-1]
acf2(djiar)    
u = resid( sarima(djiar, 1,0,0, details=FALSE)$fit )
acf2(u^2) 

summary(djia.g <- garchFit(~arma(1,0)+garch(1,1), data=djiar, cond.dist="std"))
plot(djia.g, which =3)  


#APARCH

lapply( c("xts", "fGarch"), library, char=TRUE) # load 2 packages in one line - amazing!
djiar = diff(log(djia$Close))[-1]
summary(djia.ap <- garchFit(~arma(1,0)+aparch(1,1), data=djiar, cond.dist="std"))
plot(djia.ap)   # to see all plot options 


