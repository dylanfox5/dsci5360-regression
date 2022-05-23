### dsci 5360 week 3 HW

library(faraway)
library(ggplot2)
library(dplyr)
library(lme4)
library(RLRsim)
library(pbkrtest)

## problem 1
data(denim)

# a
ggplot(denim, aes(supplier, waste)) +
  geom_point() +
  labs(x = "Supplier") +
  labs(y = "Waste")

# it appears suppliers 4 and 3 are pretty consistent with the waste they produce. supplier 1 looks to be relatively consistent
# but contains a few outliers. suppliers 5 and 2 are generally inconsistent when compared with the others. supplier 2 also
# contains what looks to be the largest outlier out of the data.

# b
lmod <- aov(waste ~ supplier, denim)
summary(lmod) # .334
coef(lmod)

# not significant

# c
qqnorm(residuals(lmod),main="")
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)

# d
mmod <- lmer(waste ~ 1+(1|supplier), denim)
summary(mmod)

# the standard deviation of the effects is 0.8192

# e
smod <- lmer(waste ~ 1+(1|supplier), denim, REML=FALSE)
sumary(smod)

# the standard deviation drops to 0 which indicates the bias of the ML method

# f
confint(mmod, level=0.95)

# g
outs <- boxplot(denim)$out
denim <- denim[-which(denim$waste %in% outs),]

mmod <- lmer(waste ~ 1+(1|supplier), denim)
summary(mmod)

# without the outliers, the standard deviation for the effects increased and decreased for the residuals.

# h
ranef(mmod)$supplier
(cc <- model.tables(lmod))
cc[[1]]$supplier/ranef(mmod)$supplier

library(lattice)
dotplot(ranef(mmod, condVar=TRUE))

# the best supplier is 4

## problem 2
data(jsp)

jspr <- jsp[jsp$year==2,]
ggplot(jspr, aes(x=raven, y=english))+xlab("Raven Score")+ylab("English Score")+geom_point(position = position_jitter(),alpha=0.3)
ggplot(jspr, aes(x=social, y=english))+xlab("Social Class")+ylab("English Score")+geom_boxplot()

glin <- lm(english ~ raven*gender*social,jspr)
anova(glin)

glin <- lm(english ~ raven+gender+social,jspr)
summary(glin)

mmod <- lmer(english ~ raven*social*gender+(1|school)+(1|school:class), data=jspr)
mmodr <- lmer(english ~ raven+social+(1|school)+(1|school:class), data=jspr)
KRmodcomp(mmod, mmodr)

all3 <- lmer(english ~ raven*social*gender+(1|school)+(1|school:class), data=jspr, REML=FALSE)
all2 <- update(all3, . ~ . -raven:social:gender)
notrs <- update(all2, . ~ . -raven:social)
notrg <- update(all2, . ~ . -raven:gender)
notsg <- update(all2, . ~ . -social:gender)
onlyrs <- update(all2, . ~ . -social:gender - raven:gender)
all1 <-  update(all2, . ~ . -social:gender - raven:gender - social:raven)
nogen <- update(all1, . ~ . -gender)

anova(all3, all2, notrs, notrg, notsg, onlyrs, all1, nogen)[,1:4]

jspr$craven <- jspr$raven-mean(jspr$raven)
mmod <- lmer(english ~ craven+gender+social+(1|school)+(1|school:class),jspr)
sumary(mmod)

# a large difference between this analysis and the one explored in the notes is that the addition
# of raven, gender, and social are all significant in predicting english test scores. in predicting 
# math test scores, only multiplication of raven and social were significant. But here, we can 
# use all three, plus random effects, to accurately predict the english test scores. predicting math 
# test scores ended up omitting a fair amount of random effects due to them not being statistically
# significant.

## problem 3
data(ratdrink)

# a
ggplot(ratdrink, aes(x=weeks, y=wt)) +
  geom_point(aes(color=treat)) +
  labs(x = "weeks") +
  labs(y = "weight")

ggplot(ratdrink, aes(x=weeks, y=wt))+geom_line()+facet_wrap(~ treat)

# we can see that the thiouracil group looks to gain the least amount of weight in the weeks. the 
# control and thyroxine group are pretty similar, but the thyroxine groups edges the control group
# out slightly.

# b
ml <- lmList(wt ~ I(weeks) | treat, ratdrink)

intercepts <- sapply(ml,coef)[1,]
slopes <- sapply(ml,coef)[2,]
plot(intercepts,slopes,xlab="Intercept",ylab="Slope")

summary(ml)

# i 
# the fixed effect intercept term for the control and thyroxine groups are very similar when compared
# to the thiouracil group. this makes sense when we go back to our initial analysis of the data. the results
# are similar so the model will produce similar results for each group. the thiouracil group has a larger t value
# than the other groups which indicate that there is a larger difference there. 

# ii
# the interaction between the thiouracil group and weeks indicate that there is less difference in data
# between thiouracil and weeks than the other groups and weeks. this is because rats gained less weight
# in this group as opposed to the others. this means our data is closer together.

# iii
mmod <- lmer(wt ~ weeks+(weeks|treat), ratdrink, REML=FALSE)
# the intercept standard deviatoin is 2.396

# c
# there is a significant treatment effect for thiouracil but not for thyroxine

# d
qqnorm(residuals(mmod),main="")
plot(fitted(mmod),residuals(mmod),xlab="Fitted",ylab="Residuals")
abline(h=0)

# as the weight increases our residuals become more spread apart. this means that it is harder
# to predict when a rat has a higher weight than one with a more predictable weight

# e
confint(mmod, level=0.95)

# the thyroxine group is not significantly different than the control group