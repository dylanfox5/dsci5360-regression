##### Time Series Elements
install.packages("xts")
install.packages("astsa")
library(astsa)

#J and J
par(mfrow=2:1)
tsplot(jj, ylab="QEPS", type="o", col=4, main="Johnson & Johnson Quarterly Earnings")
tsplot(log(jj), ylab="log(QEPS)", type="o", col=4)

# Global Warming Data
par(mfrow = c(1,1))
tsplot(cbind(gtemp_land,gtemp_ocean), spaghetti=TRUE, col = astsa.col(c(2,4), .5), 
       lwd=2, type="o", pch=20, ylab="Temperature Deviations", main="Global Warming")
legend("topleft", col=c(2,4), lty=1, lwd=2, pch=20,  bg="white",
       legend=c("Land Surface", "Sea Surface"))

# Dow Jones Industrial Average
library(xts)     # install.packages("xts") if you don't have it already 
djia_return = diff(log(djia$Close))[-1]
par(mfrow=2:1)
plot(djia$Close, col=4)
plot(djia_return, col=4)

par(mfrow = c(1,1))
tsplot(diff(log(gdp)), type="o", col=4)       # using diff log
points(diff(gdp)/lag(gdp,-1), pch="+", col=2) # actual return

# El Nino

par(mfrow = c(2,1))
tsplot(soi, ylab="", xlab="", main="Southern Oscillation Index", col=4)
text(1970, .91, "COOL", col=5)
text(1970,-.91, "WARM", col=6)
tsplot(rec, ylab="", main="Recruitment", col=4) 
par(mfrow = c(1,1))

# Predator - Prey
tsplot(cbind(Hare,Lynx), col = astsa.col(c(2,4), .6), lwd=2, type="o", pch=c(0,2),
       spaghetti=TRUE, ylab=expression(Number~~~(""%*% 1000)))
legend("topright", col=c(2,4), lty=1, lwd=2, pch=c(0,2), legend=c("Hare", "Lynx"), bty="n")

#fMRI
par(mfrow=c(3,1))
x = ts(fmri1[,4:9], start=0, freq=32)        # data
names = c("Cortex","Thalamus","Cerebellum")
u = ts(rep(c(rep(.6,16), rep(-.6,16)), 4), start=0, freq=32) # stimulus signal

for (i in 1:3){ 
  j = c(1,3,5)[i]
  tsplot(x[,j:(j+1)], ylab="BOLD", xlab="", main=names[i], col=5:6, ylim=c(-.6,.6), 
         lwd=2, xaxt="n", spaghetti=TRUE)
  axis(seq(0,256,64), side=1, at=0:4)
  lines(u, type="s", col=gray(.3)) 
}
mtext("seconds", side=1, line=1.75, cex=.9)
par(mfrow = c(1,1))

