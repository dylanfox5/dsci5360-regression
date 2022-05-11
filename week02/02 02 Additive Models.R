### 02 02 Additive Models

library(faraway)
library(ggplot2)

#EDA
data(ozone)
ggplot(ozone, aes(x=temp, y=O3)) + geom_point(size=1) + geom_smooth()
ggplot(ozone, aes(x=ibh, y=O3)) + geom_point(size=1) + geom_smooth() + theme(axis.text.x = element_text(angle = 90))
ggplot(ozone, aes(x=ibt, y=O3)) + geom_point(size=1) + geom_smooth()

#Ref Model
olm <- lm(O3 ~ temp + ibh + ibt, ozone)
sumary(olm)

#install.packages("effects")
library(effects)
plot(Effect("temp", olm, partial.residuals=TRUE))
plot(Effect("ibh", olm, partial.residuals=TRUE))
plot(Effect("ibt", olm, partial.residuals=TRUE))

#Additive Models using mgcv
library(mgcv)
ammgcv <- gam(O3 ~ s(temp)+s(ibh)+s(ibt),data=ozone)
summary(ammgcv)

plot(ammgcv, residuals=TRUE, select=1)
plot(ammgcv, residuals=TRUE, select=2)
plot(ammgcv, residuals=TRUE, select=3)

#Is temp significant? F-Test
am1 <- gam(O3 ~ s(temp)+s(ibh),data=ozone)
am2 <- gam(O3 ~ temp+s(ibh),data=ozone)
anova(am2,am1,test="F")

# Adding interaction variables
amint <- gam(O3 ~ te(temp,ibh)+s(ibt),data=ozone)
summary(amint)

anova(ammgcv,amint,test="F")

#tools for finding simple transformations
rhs <- function(x,c) ifelse(x > c, x-c, 0)
lhs <- function(x,c) ifelse(x < c, c-x, 0)

olm2 <- lm(O3 ~ rhs(temp,60)+lhs(temp,60)+rhs(ibh,1000)+lhs(ibh,1000),  ozone)
sumary(olm2)


predict(ammgcv,data.frame(temp=60,ibh=2000,ibt=100),se=T)
predict(ammgcv,data.frame(temp=120,ibh=2000,ibt=100),se=T)

# Diagnostics
plot(residuals(ammgcv)~predict(ammgcv),xlab="Predicted",ylab="Residuals")
abline(h=0)
qqnorm(residuals(ammgcv),main="")
qqline(residuals(ammgcv))


#Model with all data using gam
amred <- gam(O3 ~ s(vh)+s(wind)+s(humidity)+s(temp)+s(dpg)+ s(vis)+s(doy),data=ozone)
summary(amred)

alm <- lm(O3 ~ vis+doy+ibt+humidity+temp,data=ozone)
sumary(alm)

#Generalized Additive Models
gammgcv <- gam(O3 ~ s(temp)+s(ibh)+s(ibt),family=poisson,  scale=-1,data=ozone)
summary(gammgcv)

plot(gammgcv, residuals=TRUE, select=1)
plot(gammgcv, residuals=TRUE, select=2)
plot(gammgcv, residuals=TRUE, select=3)


data(epilepsy)
egamm <- gamm(seizures ~ offset(timeadj) + treat*expind+s(age), family=poisson,  random=list(id=~1), data=epilepsy, subset=(id!=49))
summary(egamm$gam)
