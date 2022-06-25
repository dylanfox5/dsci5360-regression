### dsci 5360 final project @dfox

library(xts)
library(astsa)
library(forecast)

## eda
df <- read.csv("/Users/dylanfox/OneDrive - Graceland University/Graduate Work/dsci5360-regression_time_series/final-project/ParisFlu.csv")
df
df$date <- as.Date(df$date)
ts <- xts(df$flu.rate, df$date)
ts.plot(ts)

summary(fit <- lm(df$flu.rate ~ df$date))
ts.plot(ts)
abline(fit)

# detrending 
fit <- lm(ts ~ time(ts), na.action=NULL)
ts.plot(resid(fit))
acf1(resid(fit), 48, main="detrended flu rate")

# differencing
ts.plot(diff(ts))
acf1(diff(ts), 48, main="differenced flu rate")

# compare
par(mfrow=c(2,1))
ts.plot(resid(fit))
ts.plot(diff(ts))
dev.new()
par(mfrow=c(2,1))
acf1(resid(fit), 48, main="detrended flu rate")
acf1(diff(ts), 48, main="differenced flu rate")
par(mfrow=c(1,1))

## arima
auto.arima(ts, seasonal=FALSE)
diff <- na.omit(diff(ts))
sarima(diff, p=0, d=0, q=1)
sarima.for(diff, n.ahead=52, p=0, d=0, q=1, plot.all=TRUE)

## sarima
auto.arima(diff, seasonal=TRUE)
sarima(ts, p=0, d=0, q=0, P=2, D=0, Q=3, 12)
sarima.for(ts, n.ahead=52, p=0, d=0, q=0, P=2, D=0, Q=3, 12)

## mixed sarima
sarima(ts, p=2, d=0, q=0, P=0, D=0, Q=3, 12)
sarima.for(ts, n.ahead=52, p=2, d=0, q=0, P=0, D=0, Q=3, 12)

## best model
# seasonal arima

## predict one year
ts_sub <- ts[1:468]
sarima.for(ts_sub, n.ahead=52, p=2, d=0, q=3, plot.all=TRUE)
dev.new()
ts.plot(ts)

ts_sub2 <- ts[469:length(ts)]
abline(ts_sub2)

## predict one year into future
sarima.for(ts, n.ahead=52, p=2, d=0, q=3, plot.all=TRUE)
