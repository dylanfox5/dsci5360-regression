library(faraway)
data(bliss)
modl <- glm(cbind(dead,alive) ~ conc, family=binomial, bliss)
summary(modl)$coef

#Binomial
y <- bliss$dead/30;  mu <- y
eta <- logit(mu)
z <- eta + (y-mu)/(mu*(1-mu))
w <- 30*mu*(1-mu)
lmod <- lm(z  ~ conc, weights=w, bliss)
coef(lmod)

for(i in 1:5){
 eta <- lmod$fit
 mu <- ilogit(eta)
 z <- eta + (y-mu)/(mu*(1-mu))
 w <- 30*mu*(1-mu)
 lmod <- lm(z  ~ bliss$conc, weights=w)
 cat(i,coef(lmod),"\n")
}

sumary(lmod)

### computing correct standard error
xm <- model.matrix(lmod)
wm <- diag(w)
sqrt(diag(solve(t(xm) %*% wm %*% xm)))


summary(lmod)$coef[,2]/summary(lmod)$sigma

###Hypothesis Testing

sumary(modl)
1-pchisq(deviance(modl),df.residual(modl))
anova(modl,test="Chi")
modl2 <- glm(cbind(dead,alive) ~ conc+I(conc^2),family=binomial,bliss)
anova(modl,modl2,test="Chi")
anova(modl2,test="Chi")

### Diagnostics
residuals(modl)
residuals(modl,"pearson")
residuals(modl,"response")
bliss$dead/30 - fitted(modl)
residuals(modl,"working")
modl$residuals
residuals(lmod)

# Outliers
influence(modl)$hat

influence(modl)$coef
cooks.distance(modl)

#Model Diagnostics
data(gala)
gala <- gala[,-2]
modp <- glm(Species ~ .,family=poisson,gala)

plot(residuals(modp) ~ predict(modp,type="response"),
  xlab=expression(hat(mu)),ylab="Deviance residuals")

plot(residuals(modp) ~ predict(modp,type="link"),
  xlab=expression(hat(eta)),ylab="Deviance residuals")

plot(residuals(modp,type="response") ~ predict(modp,type="link"),
  xlab=expression(hat(eta)),ylab="Response residuals")

## Diagnostics and response
plot(Species ~ Area, gala)
plot(Species ~ log(Area), gala)

mu <- predict(modp,type="response")
z <- predict(modp)+(gala$Species-mu)/mu
plot(z ~ log(Area), gala,ylab="Linearized Response")


modpl <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) +
  log(Scruz+0.1) + log(Adjacent), family=poisson, gala)
c(deviance(modp),deviance(modpl))



mu <- predict(modpl,type="response")
u <- (gala$Species-mu)/mu + coef(modpl)[2]*log(gala$Area)
plot(u ~ log(Area), gala,ylab="Partial Residual")
abline(0,coef(modpl)[2])

z <- predict(modpl)+(gala$Species-mu)/mu
plot(z ~ predict(modpl), xlab="Linear predictor",
  ylab="Linearized Response")

### Unusual Points
halfnorm(rstudent(modpl))

gali <- influence(modpl)
halfnorm(gali$hat)

halfnorm(cooks.distance(modpl))

plot(gali$coef[,5],ylab="Change in Scruz coef",xlab="Case no.")

modplr <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest)
  + log(Scruz+0.1) + log(Adjacent), family=poisson, gala, subset=-25)
cbind(coef(modpl),coef(modplr))

modpla <- glm(Species ~ log(Area)+log(Adjacent), family=poisson, gala)
dp <- sum(residuals(modpla,type="pearson")^2)/modpla$df.res
sumary(modpla,dispersion=dp)

### Sandwich
#install.packages("sandwich")
data(gala)
library(sandwich)
modpla <- glm(Species ~ log(Area)+log(Adjacent), family=poisson, gala)
(sebeta <- sqrt(diag(vcovHC(modpla))))

### Robust
#install.packages("robustbase")
data(gala)
library(robustbase)
rmodpla <- glmrob(Species ~ log(Area)+log(Adjacent), family=poisson, data=gala)
summary(rmodpla)


wts <- rmodpla$w.r
names(wts) <- row.names(gala)
head(sort(wts))
