### dsci 5360 week 2 HW

library(ISLR)
library(tidyverse)
library(ggplot2)
library(naniar)
library(corrplot)
library(coefplot)
library(faraway)
library(sm)
library(splines)
library(mgcv)

## problem 1
data(teengamb)

# a 
plot(teengamb$income, teengamb$gamble)

# b
for(bw in c(0.1,0.5,2)) {
  with(teengamb, {
    plot(gamble ~ income, col=gray(0.75))
    lines(ksmooth(income, gamble, "normal", bw))
  })}
# the fit does not look linear

# c
lmod <- lm(gamble ~ bs(income), teengamb)
plot(gamble ~ income, teengamb, col=gray(0.75))
lines(predict(lmod) ~ income, teengamb, lty=2)

# d
with(teengamb,{
  plot(gamble ~ income, col=gray(0.75))
  f <- loess(gamble ~ income)
  i <- order(income)
  lines(f$x[i],f$fitted[i])
})

# e
ggplot(teengamb, aes(x=income,y=gamble)) + geom_point(alpha=0.25) + geom_smooth(method="loess", span=0.22) + geom_line(aes(x=income,y=gamble),linetype=2)

# a linear fit is not justified 

## problem 2
data(prostate)

# a 
plot(prostate$age, prostate$lweight)

# the relationship doesn't appear to be linear...almost quadratic or maybe cubic

# b
with(prostate, sm.regression(age, lweight, h=h.select(age, lweight)))

# c
with(prostate, {
  plot(lweight ~ age, col=gray(0.75))
  lines(smooth.spline(age, lweight), lty=2)
})

# d
ggplot(prostate, aes(x=age,y=lweight)) + geom_point(alpha=0.25) + geom_smooth(method="loess", span=0.22) + geom_line(aes(x=age,y=lweight),linetype=2)

# i don't believe a linear fit is plausible for this data

# e
with(prostate, {
  plot(lweight ~ age, col=gray(0.75))
  sm.regression(age, lweight, h=h.select(age, lweight))
  lines(smooth.spline(age, lweight), lty=2)
  f <- loess(lweight ~ age)
  lines(f$x, f$fitted,lty=2)
})

# f
amod <- gam(lweight ~ s(age, lpsa), data=prostate)
vis.gam(amod, col="gray", ticktype="detailed",theta=-35)

## problem 3
data(aatemp)

# a
plot(aatemp$year, aatemp$temp)

# the underlying trend suggests that temps have increased in more recent years

# b
lm <- lm(temp ~ year, data=aatemp)
plot(temp ~ year, aatemp, col=gray(0.75))
lines(predict(lm) ~ year, aatemp, lty=2)

# the main drawback of this approach is that it does not fit the data very well. this is because
# it deceptive to outliers that skew the fit

# c
ggplot(aatemp, aes(x=year,y=temp)) + geom_point(alpha=0.25) + geom_smooth(method="lowess", span=.75)

# this tells us that the underlying trend is deceptive to outliers because the confidence band 
# is rather large at some areas. there aren't many spots that the confidence is small, meaning that
# we are not confident in this fit

# d
rhs <- function(x,c) ifelse(x>c,x-c,0)
curve(rhs(x,0.5),0,1)
(knots <- 0:11/12)
dmn <- outer(aatemp$year, knots, rhs)
lmod <- lm(aatemp$temp ~ dmn)
plot(temp ~ year, aatemp, col=gray(0.75))
lines(aatemp$year,predict(lmod))

# e

# Looking at the summary of the linear fit and regression spline, we have equal p-values of 
# 0.001533. This tells us we should look into increasing the dimensionality of our spline fit
# to adjust the curvature in hopes of better fitting our data.

## problem 4
data(fat)
fat

# a
plot(fat$age, fat$siri)
plot(fat$weight, fat$siri)
plot(fat$height, fat$siri)
plot(fat$adipos, fat$siri)
plot(fat$neck, fat$siri)
plot(fat$chest, fat$siri)
plot(fat$abdom, fat$siri)
plot(fat$hip, fat$siri)
plot(fat$thigh, fat$siri)

# generally speaking, each of the plots above have a positive relationship. This means, 
# as body fat increases, so does the x-value. this makes sense because those with a higher
# body fat percentage will likely have larger measurements

# b
lm <- lm(siri ~ age+weight+height+adipos+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data=fat)
summary(lm)
par(mfrow = c(1, 1))
plot(lm)
cooksD <- cooks.distance(lm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

influential <- names(influential) # 39, 42, 86
outliers <- fat[influential,]
fat_no_outliers <- fat %>% anti_join(outliers)
lm2 <- lm(siri ~ age+weight+height+adipos+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data = fat_no_outliers)
summary(lm2) 

# our adjusted r^2 increased slightly with the removal of the three outliers.
# it went from 0.7343 => 0.7382. based on our newest model, the abdom is the most significant
# predictor variable

# c
ammgcv <- gam(siri ~ s(age)+s(weight)+s(height)+s(adipos)+s(neck)+s(chest)+s(abdom)+s(hip)+s(thigh)+s(knee)+s(ankle)+s(biceps)+s(forearm)+s(wrist), data=fat)
summary(ammgcv)
cooksD <- cooks.distance(ammgcv)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# d
plot(ammgcv, residuals=TRUE, select=1)
plot(ammgcv, residuals=TRUE, select=2)
plot(ammgcv, residuals=TRUE, select=7) # abdom has the strongest relationship

# e

# the r^2 value for the additive model is 0.787 which is slightly better than our linear fit.
# this means that we improve our accuracy with the additive model but does not mean it is 
# necessarily better. the variable that makes the most nonlinear contribution to the fit is 
# height. replacing this or removing may improve our model slightly.

ammgcv <- gam(siri ~ s(age)+s(weight)+s(adipos)+s(neck)+s(chest)+s(abdom)+s(hip)+s(thigh)+s(knee)+s(ankle)+s(biceps)+s(forearm)+s(wrist), data=fat)
summary(ammgcv)

# without height, our new r^2 value is 0.788

# f

## problem 5
data(dvisits)

# a
gammgcv <- gam(doctorco ~ sex+age+agesq+s(income)+levyplus+freepoor+illness+s(actdays)+s(hscore)+chcond1+chcond2, scale=-1, data=dvisits)
summary(gammgcv)

# sex, freepoor, illness are statistically significant

# b
gammgcv <- gam(doctorco ~ sex+freepoor+illness, scale=-1, data=dvisits)
summary(gammgcv)

# the larger model looks to preferred

# c
gammgcv <- glm(doctorco ~ sex+freepoor+illness, family=poisson, data=dvisits)
summary(gammgcv)

# d

# someone is more likely to visit the doctor if they are covered by the government
# or have been recently ill.

# e
predict(gammgcv, tail(dvisits, n=1), type="response")
