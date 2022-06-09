### 06 01 ARMA and ARIMA  

library(xts)
library(astsa)

# Yule-Walker Estimates
rec.yw = ar.yw(rec, order=2)
rec.yw$x.mean   # mean estimate
rec.yw$ar       # phi parameter estimates
sqrt(diag(rec.yw$asy.var.coef)) # standard errors
rec.yw$var.pred # error variance estimate

# Glacial Varve Series

tsplot(varve, main="", ylab="", col=4, margin=0)
mtext("varve", side=3, line=.5, cex=1.2, font=2, adj=0)
tsplot(log(varve), main="", ylab="", col=4, margin=0)
mtext("log(varve)", side=3, line=.5, cex=1.2, font=2, adj=0)
qqnorm(varve, main="", col=4); qqline(varve, col=2, lwd=2)
qqnorm(log(varve), main="", col=4); qqline(log(varve), col=2, lwd=2)   

tsplot(diff(log(varve)), col=4, ylab=expression(nabla~log~X[~t]), main="Transformed Glacial Varves")
dev.new()
acf2(diff(log(varve)))

# Gauss Newton

x = diff(log(varve))       # data
r = acf1(x, 1, plot=FALSE) # acf(1)
c(0) -> w -> z -> Sc -> Sz -> Szw -> para # initialize all variables
num = length(x)            # 633

## Gauss-Newton Estimation
para[1] = (1-sqrt(1-4*(r^2)))/(2*r)  # MME to start (not very good)
niter   = 12             
for (j in 1:niter){
  for (t in 2:num){ w[t] = x[t]   - para[j]*w[t-1]
  z[t] = w[t-1] - para[j]*z[t-1]
  }
  Sc[j]  = sum(w^2)
  Sz[j]  = sum(z^2)
  Szw[j] = sum(z*w)
  para[j+1] = para[j] + Szw[j]/Sz[j]
}
## Results
cbind(iteration=1:niter-1, thetahat=para[1:niter], Sc, Sz)

#Gauss - Newton Sum of Squares 

## Plot conditional SS and results
c(0) -> w -> cSS
th = -seq(.3, .94, .01)
for (p in 1:length(th)){
  for (t in 2:num){ w[t] = x[t] - th[p]*w[t-1] 
  }
  cSS[p] = sum(w^2)
}

dev.new()
tsplot(th, cSS, ylab=expression(S[c](theta)), xlab=expression(theta))
abline(v=para[1:12], lty=2, col=4) # add previous results to plot
points(para[1:12], Sc[1:12], pch=16, col=4)

#Unconditional Glacial Varve
sarima(diff(log(varve)), p=0, d=0, q=1, no.constant=TRUE)

