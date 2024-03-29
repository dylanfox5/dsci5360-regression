######## 02 01 Nonparametric Regression
library(faraway)

#Data
data(exa)
plot(y ~ x, exa,main="Example A")
lines(m ~ x, exa, lwd=2)
curve((sin(2*pi*x^3)^3), from=0.0, to=1.0)

data(exb)
plot(y ~ x, exb,main="Example B")
lines(m ~ x, exb, lwd=2)

plot(waiting ~ eruptions, faithful,main="Old Faithful")

### Example comparison of bandwidth (smoother)
for(bw in c(0.1,0.5,2)){
with(faithful,{
     plot(waiting ~ eruptions, col=gray(0.75))
     lines(ksmooth(eruptions,waiting,"normal",bw))
     })}



#install.packages("sm")
library(sm)
with(faithful,sm.regression(eruptions, waiting, h=h.select(eruptions,waiting)))
with(exa, sm.regression(x, y, h=h.select(x,y)))
with(exb, sm.regression(x, y, h=h.select(x,y)))

# Smoothing Splines
with(faithful,{
    plot(waiting ~ eruptions, col=gray(0.75))
    lines(smooth.spline(eruptions,waiting),lty=2)
})
with(exa,{
    plot(y ~ x, col=gray(0.75))
    lines(x,m)
    lines(smooth.spline(x,y),lty=2)
})
with(exb,{
    plot(y ~ x, col=gray(0.75))
    lines(x,m)
    lines(smooth.spline(x,y),lty=2)
})


#Regression Splines
rhs <- function(x,c) ifelse(x>c,x-c,0)
curve(rhs(x,0.5),0,1)

(knots <- 0:9/10)

dm  <- outer(exa$x,knots,rhs)
matplot(exa$x,dm,type="l",col=1, xlab="x", ylab="")

lmod <- lm(exa$y ~ dm)
plot(y ~ x, exa, col=gray(0.75))
lines(exa$x,predict(lmod))


newknots <- c(0,0.5,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)
dmn  <- outer(exa$x,newknots,rhs)
lmod <- lm(exa$y ~ dmn)
plot(y ~x, exa, col=gray(0.75))
lines(exa$x,predict(lmod))



library(splines)
matplot(bs(seq(0,1,length=1000),df=12),type="l",ylab="",col=1)

lmod <- lm(y ~ bs(x,12),exa)
plot(y ~ x, exa, col=gray(0.75))
lines(m ~ x, exa)
lines(predict(lmod) ~ x, exa, lty=2)

# Local Polynomials
with(faithful,{
    plot(waiting ~ eruptions, col=gray(0.75))
    f <- loess(waiting ~ eruptions)
    i <- order(eruptions)
    lines(f$x[i],f$fitted[i])
})

with(exa,{
    plot(y ~ x, col=gray(0.75))
    lines(m ~ x)
    f <- loess(y ~ x)
    lines(f$x,f$fitted,lty=2)
    f <- loess(y ~ x,span=0.22)
    lines(f$x,f$fitted,lty=5)
})

with(exb,{
    plot(y ~ x, col=gray(0.75))
    lines(m ~ x)
    f <- loess(y ~ x)
    lines(f$x,f$fitted,lty=2)
    f <- loess(y ~ x,span=1)
    lines(f$x,f$fitted,lty=5)
})

# COnfidence
library(ggplot2)
ggplot(exa, aes(x=x,y=y)) + geom_point(alpha=0.25) + geom_smooth(method="loess", span=0.22) + geom_line(aes(x=x,y=m),linetype=2)

library(mgcv)
ggplot(exa, aes(x=x,y=y)) + geom_point(alpha=0.25) + geom_smooth(method="gam", formula=y ~ s(x, k=20)) + geom_line(aes(x=x,y=m),linetype=2)

# Multivariable

data(savings)
y <- savings$sr
x <- cbind(savings$pop15,savings$ddpi)
sm.regression(x,y,h=c(1,1),xlab="pop15",ylab="growth",zlab="savings rate")
sm.regression(x,y,h=c(5,5),xlab="pop15",ylab="growth",zlab="savings rate")

library(mgcv)
amod <- gam(sr ~ s(pop15,ddpi), data=savings)
vis.gam(amod, col="gray", ticktype="detailed",theta=-35)

lomod <- loess(sr ~ pop15 + ddpi, data=savings)
xg <- seq(21,48,len=20)
yg <- seq(0,17,len=20)
zg <- expand.grid(pop15=xg,ddpi=yg)
persp(xg, yg, predict(lomod, zg), theta=-35, ticktype="detailed", xlab="pop15", ylab="growth", zlab="savings rate", col="gray")
