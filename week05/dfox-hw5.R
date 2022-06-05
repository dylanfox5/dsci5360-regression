### dsci 5360 week 5 HW

library(xts)
library(astsa)
library(dynlm)
library(zoo)

## problem 1
temp  = tempr-mean(tempr)  # center temperature    
temp2 = temp^2             # square it
temp3 = temp^3
trend = time(cmort)        # time

fit = lm(cmort ~ trend + temp + temp2 + part, na.action=NULL)
fit1 = lm(cmort ~ trend + temp + temp2 + temp3 + part, na.action=NULL)

summary(fit)       # regression results
summary(aov(fit))  # ANOVA table   (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 

num = length(cmort)                                     # sample size
AIC(fit)/num - log(2*pi)                                # AIC 
BIC(fit)/num - log(2*pi)                                # BIC   

num = length(cmort)                                     # sample size
AIC(fit1)/num - log(2*pi)                                # AIC 
BIC(fit1)/num - log(2*pi)                                # BIC

# based on the AIC, the model including the extra component does slightly worse. the AIC
# value is 4.711363, while the AIC with the original model is 4.721732

## problem 2
par(mfrow=c(1,1))
tsplot(diff(gtemp_land), col=4, main="differenced global tmeperature")

# Moving Average
w = c(.5, rep(1,11), .5)/12
soif = filter(gtemp_land, sides=2, filter=w)
tsplot(gtemp_land, col=astsa.col(4,.7), ylim=c(-1, 1.15))
lines(soif, lwd=2, col=4)

# using the moving average loses a considerable amount of the frequency and variance of high/low temps

# Kernel
tsplot(gtemp_land, col=astsa.col(4,.7), ylim=c(-1, 1.15))
lines(ksmooth(time(gtemp_land), gtemp_land, "normal", bandwidth=12), lwd=2, col=4)

# using the kernel smoothing continues to lose a considerable amount of the frequency and variance. probably
# even more than the moving average it seems

# Lowess
tsplot(gtemp_land, col=astsa.col(4,.6))
lines(lowess(gtemp_land, f=.05), lwd=2, col=4)

# using the lowess smoothing introduces less variance loss compared to moving average/kernel. if i were to pick
# a smoothing trend to move forward with, i would consider using either lowess or moving average

## problem 3
AR1 = c(1, -.8, .15) # original AR coefs on the left
polyroot(AR1)
MA1 = c(1, -.30)       # original MA coefs on the right
polyroot(MA1)

AR2 = c(1, -1, .5)
polyroot(AR2)
MA2 = c(1, -1)
polyroot(MA2)

# a
# only the first model is redundant. it can be reduced to: Xt = .5Xt + wt

# b
# i
round(ARMAtoMA(ar=-.5, ma=0, 10), 2) # first 10 psi-weights
round(ARMAtoAR(ar=-.5, ma=0, 10), 2) # first 10 pi-weights
# invertible

# ii
round(ARMAtoMA(ar=c(-1, .5), ma=-1, 10), 2) # first 10 psi-weights
round(ARMAtoAR(ar=c(-1, .5), ma=1, 10), 2) # first 10 pi-weights
# causal

## problem 4
jj <- JohnsonJohnson
Trend = time(jj) - 1970
Q = factor(cycle(jj))
reg = lm(log(jj) ~ 0 + Trend + Q, na.action=NULL)
model.matrix(reg)
summary(reg)

# a
# the estimated average annual increase in the logged earnings per share will be the sum of
# each quarter's coefficient: 1.052793 + 1.080916 + 1.151024 + 0.882266 = 4.166999

# b
# the average annual earnings decrease from Q3 to Q4: 1.152014 - 0.882266 = 0.267857

# c
plot(log(jj), col=3)
lines(fitted(reg))

# d
plot(log(jj) - fitted(reg))

# based on the graph of the residuals, there doesn't seem to be a pattern. therefore, this could
# be white noise. the fit looks good