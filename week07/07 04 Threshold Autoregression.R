## 07 04 Threshold Autoregression

library(astsa)
library(xts)

# Threshold Autoregression

tsplot(flu, type="c", ylab="Influenza Deaths per 10,000")
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(flu, pch=Months, cex=.8, font=4, col=c(4,2,3,6))

dflu = diff(flu)
plot(dflu, type = "p", col=2)

lag1.plot(dflu, corr=FALSE) # scatterplot with lowess fit
abline(v = 0.1)
thrsh = .05 # threshold
Z = ts.intersect(dflu, lag(dflu,-1), lag(dflu,-2), lag(dflu,-3), lag(dflu,-4) )
ind1 = ifelse(Z[,2] < thrsh, 1, NA) # indicator < thrsh
ind2 = ifelse(Z[,2] < thrsh, NA, 1) # indicator >= thrsh
X1 = Z[,1]*ind1
X2 = Z[,1]*ind2

summary(fit1 <- lm(X1~ Z[,2:5]) ) # case 1
summary(fit2 <- lm(X2~ Z[,2:5]) ) # case 2
D = cbind(rep(1, nrow(Z)), Z[,2:5]) # design matrix
p1 = D %*% coef(fit1) # get predictions
p2 = D %*% coef(fit2)
prd = ifelse(Z[,2] < thrsh, p1, p2)



tsplot(prd, ylim=c(-.5,.5), ylab=expression(nabla~flu[~t]), lwd=2, col=4)
prde1 = sqrt(sum(resid(fit1)^2)/df.residual(fit1))
prde2 = sqrt(sum(resid(fit2)^2)/df.residual(fit2))
prde = ifelse(Z[,2] < thrsh, prde1, prde2)
x = time(dflu)[-(1:4)]
xx = c(x, rev(x))
yy = c(prd - 2*prde, rev(prd + 2*prde))
polygon(xx, yy, border=8, col=gray(.8, .3))
abline(h=.05, col=4, lty=6)
points(dflu, pch=16, col=2)
#

par(mar=c(2.5,2.5,0,0)+.5, mgp=c(1.6,.6,0))
U = matrix(Z, ncol=5) # Z was created in the analysis above
culer = c(rgb(0,1,0,.4), rgb(0,0,1,.4))
culers = ifelse(U[,2]<.05, culer[1], culer[2])
plot(U[,2], U[,1], panel.first=Grid(), pch=21, cex=1.1, bg=culers, 
     xlab=expression(nabla~flu[~t-1]), ylab=expression(nabla~flu[~t]))
lines(lowess(U[,2], U[,1], f=2/3), col=6)
abline(v=.05, lty=2, col=4)

##- alternate method
# install.packages('tsDyn')
library(tsDyn)
# vignette("tsDyn") # for package details
(u = setar(dflu, m=4, thDelay=0, th=.05)) # fit model and view results
(u = setar(dflu, m=4, thDelay=0)) # let program fit threshold (=.036)
AIC(u) # if you want to try other models; m=3 works well too
plot(u) # graphics - ?plot.setar for information

