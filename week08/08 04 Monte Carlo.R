# Monte Carlo
#install.packages("MonteCarlo")
library(MonteCarlo)
library(dplyr)
library(ggplot2)

ttest<-function(n,loc,scale){
  
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  decision<-abs(stat)>1.96
  return(list("stat"=decision))
}

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
summary(MC_result)

# Output of a single repetition
ttest<-function(n,loc,scale){
  
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  return(list("stat"=stat))
}
MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
df<-MakeFrame(MC_result)
head(df)


tbl <- as_tibble(df)
ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))

#############################################################################
# Application to Time-Series
library(forecast)

phi <- 0.7
time_steps <- 24
N <- 1000
sigma_error <- 1

sd_series <- sigma_error^2/(1-phi^2)
starts <- rnorm(N, sd=sqrt(sd_series))
estimates1 <- numeric(N)
res <- numeric(time_steps)

for (i in 1:N) {
  errs <- rnorm(time_steps, sd=sigma_error)
  res[1] <- starts[i] + errs[1]
  
  for (t in 2:time_steps) {
     res[t] <- phi * tail(res, 1) + errs[t]
  }
  estimates1[i]<- arima(res, c(1, 0, 0))$coef[1]
}

hist(estimates1, main="Estimated Phi for AR(1) when ts is AR(1)", breaks=50)

summary(estimates1)

##

phi_1 <- 0.7
phi_2 <- -0.2
estimates2 <- numeric(N)
for (i in 1:N) {
  res <- arima.sim(list(order=c(2,0,0), ar=c(phi_1, phi_2)), n=time_steps)
  estimates2[i] <- arima(res, c(1,0,0))$coef[1]
}
hist(estimates2, main="Estimated Phi for AR(1) when ts is AR(2)", breaks=50)
summary(estimates2)
