# 06 02 Forecasting
library(xts)
library(astsa)


### Forecasting Recruitment
sarima(rec, p=2, d=0, q=0)  # fit the model
sarima.for(rec, n.ahead=24, p=2, d=0, q=0)
abline(h=61.8585, col=4)    # display estimated mean