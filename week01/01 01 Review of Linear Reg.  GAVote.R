### Review of Linear Regression

# install.packages("faraway")
library(faraway)

data("gavote")

## Examining the Data
str(gavote)
head(gavote)
summary(gavote)

#### Calculate the undercount as a fraction of total votes

gavote$undercount <- (gavote$ballots-gavote$votes)/gavote$ballots
summary(gavote)

hist(gavote$undercount, main="Undercount", xlab="Percent Undercount")
plot(density(gavote$undercount), main="Undercount")
rug(gavote$undercount)
barplot(sort(table(gavote$equip), decreasing=TRUE), las=2)

#### Gore and proportion of African Americans

gavote$pergore <- gavote$gore/gavote$votes
plot(pergore ~ perAA, gavote, xlab = "Proportion of African American", ylab = 'Proportion for Gore')

plot(undercount ~ equip, gavote, xlab="", las=3)


xtabs(~atlanta + rural, gavote)

names(gavote)[4] <- "usage"
names(gavote)

#### Selecting Variables of Interest
nix <- c(3, 10, 11, 12)
cor(gavote[,nix])

#fitting the linear model

lmod <- lm(undercount ~ pergore + perAA, gavote)
coef(lmod)
predict(lmod)
residuals(lmod)
deviance(lmod)
df.residual(lmod)

# standard deviation
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum <- summary(lmod)
lmodsum$sigma
lmodsum$r.squared
cor(predict(lmod), gavote$undercount)^2
lmodsum$adj.r.squared
summary(lmod)
sumary(lmod)

# Interpretation

gavote$cpergore <- gavote$pergore-mean(gavote$pergore)
gavote$cperAA <- gavote$perAA - mean(gavote$perAA)

lmodi <- lm(undercount ~ cperAA + cpergore*usage + equip, gavote)                                     
sumary(lmodi)                                   

#Hypothesis Testing

anova(lmod, lmodi)
drop1(lmodi, test="F")                                     

confint(lmodi)                                     

### Diagnostics
plot(lmodi)                                     
plot(lmod)

gavote[cooks.distance(lmodi) > 0.1,]

halfnorm(hatvalues(lmodi))
gavote[hatvalues(lmodi) > 0.3,]
termplot(lmodi, partial=TRUE, terms=1)                                     

#Robust Regression
library(MASS)
rlmodi <- rlm(undercount ~ cperAA + cpergore*usage+equip, gavote)
summary(rlmodi)


# Weighted Least Squares

wlmodi <- lm(undercount ~ cperAA+cpergore*usage+equip, gavote, weights = ballots)
summary(wlmodi)
sqrt(0.035*(1-0.035)/881)


#Transformation

plmodi <- lm(undercount ~ poly(cperAA,4)+cpergore*usage+equip, gavote)
summary(plmodi)
termplot(plmodi,partial=TRUE,terms=1)

library(splines)
blmodi <- lm(undercount ~ cperAA+bs(cpergore,4)+usage+equip, gavote)
termplot(blmodi,partial=TRUE,terms=2)

# Variable Selection
biglm <- lm(undercount ~ (equip+econ+usage+atlanta)^2+(equip+econ+usage+atlanta)*(perAA+pergore), gavote)
smallm <- step(biglm,trace=FALSE)
sumary(smallm)

drop1(smallm,test="F")

### Final Model
finalm <- lm(undercount~equip + econ  + perAA + equip:econ + equip:perAA, gavote)
sumary(finalm)



# Drawing Conclusions
pdf <- data.frame(econ=rep(levels(gavote$econ), 5),  equip=rep(levels(gavote$equip), rep(3,5)), perAA=0.233)

pp <- predict(finalm,new=pdf)
xtabs(round(pp,3) ~ econ + equip, pdf)

pdf <- data.frame(econ=rep("middle",15), equip=rep(levels(gavote$equip),  rep(3,5)), perAA=rep(c(.11,0.23,0.35),5))
pp <- predict(finalm,new=pdf)

propAA <- gl(3,1,15,labels=c("low","medium","high"))
xtabs(round(pp,3) ~ propAA + equip,pdf)
