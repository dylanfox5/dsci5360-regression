########### Mixed Effects Models ##############
library(faraway)
library(ggplot2)
data(pulp)
ggplot(pulp, aes(x=operator, y=bright))+geom_point(position = position_jitter(width=0.1, height=0.0))

#One - way ANOVA Model
op <- options(contrasts=c("contr.sum", "contr.poly"))
lmod <- aov(bright ~ operator, pulp)
summary(lmod)
coef(lmod)
options(op)
(0.447-0.106)/5

# MLE Demonstration

library(lme4)
mmod <- lmer(bright ~ 1+(1|operator), pulp)
summary(mmod)
sumary(mmod)

smod <- lmer(bright ~ 1+(1|operator), pulp, REML=FALSE)
sumary(smod)

# Inference
nullmod <- lm(bright ~ 1, pulp)

lrtstat <- as.numeric(2*(logLik(smod)-logLik(nullmod)))
pvalue <- pchisq(lrtstat,1,lower=FALSE)
data.frame(lrtstat, pvalue)

y <- simulate(nullmod)
lrstat <- numeric(1000)
set.seed(123)
##### The following loop will produce warning messages which can be ignored
for(i in 1:1000){
   y <- unlist(simulate(nullmod))
   bnull <- lm(y ~ 1)
   balt <- lmer(y ~ 1 + (1|operator), pulp, REML=FALSE)
   lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull)))
}

mean(lrstat < 0.00001)
mean(lrstat > 2.5684)
sqrt(0.019*0.981/1000)

#install.packages('RLRsim')
library(RLRsim)
exactLRT(smod, nullmod)
exactRLRT(mmod)
VarCorr(mmod)

as.data.frame(VarCorr(mmod)) # A more convient form for extracting parameters

bsd <- numeric(1000)
#### The following loop will produce warning messages which can be ignored
for(i in 1:1000){
  y <- unlist(simulate(mmod))
  bmod <- refit(mmod, y)
  bsd[i] <- as.data.frame(VarCorr(bmod))$sdcor[1]
}

quantile(bsd, c(0.025, 0.975))
confint(mmod, method="boot")

# Estimating Random Effects
ranef(mmod)$operator
(cc <- model.tables(lmod))
cc[[1]]$operator/ranef(mmod)$operator

library(lattice)
dotplot(ranef(mmod, condVar=TRUE))

# Prediction
fixef(mmod)+ranef(mmod)$operator
predict(mmod, re.form=~0)[1]
predict(mmod, newdata=data.frame(operator="a"))

group.sd <- as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd <- as.data.frame(VarCorr(mmod))$sdcor[2]
pv <- numeric(1000)
#### The following loop will produce warning messages which can be ignored
for(i in 1:1000){
 y <- unlist(simulate(mmod))
 bmod <- refit(mmod, y)
 pv[i] <- predict(bmod, re.form=~0)[1] + rnorm(n=1,sd=group.sd) + rnorm(n=1,sd=resid.sd)
}
quantile(pv, c(0.025, 0.975))

#### The following loop will produce warning messages which can be ignored
for(i in 1:1000){
 y <- unlist(simulate(mmod, use.u=TRUE))
 bmod <- refit(mmod, y)
 pv[i] <- predict(bmod, newdata=data.frame(operator="a")) + rnorm(n=1,sd=resid.sd)
}
quantile(pv, c(0.025, 0.975))

# Diagnostics

qqnorm(residuals(mmod),main="")
plot(fitted(mmod),residuals(mmod),xlab="Fitted",ylab="Residuals")
abline(h=0)

# Nested Effects
data(eggs)
summary(eggs)

ggplot(eggs, aes(y=Fat, x=Lab, color=Technician, shape=Sample)) + geom_point(position = position_jitter(width=0.1, height=0.0))+scale_color_grey()

cmod <- lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician) +  (1|Lab:Technician:Sample), data=eggs)
sumary(cmod)

cmodr <- lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician), data=eggs)
lrstat <- numeric(1000)
#### The following loop will produce warning messages which can be ignored
for(i in 1:1000){
   rFat <- unlist(simulate(cmodr))
   nmod <- lmer(rFat ~ 1 + (1|Lab) + (1|Lab:Technician), data=eggs)
   amod <- lmer(rFat ~ 1 + (1|Lab) + (1|Lab:Technician) +
   (1|Lab:Technician:Sample), data=eggs)
   lrstat[i] <- 2*(logLik(amod)-logLik(nmod))
}
mean(lrstat > 2*(logLik(cmod)-logLik(cmodr)))

cmods <- lmer(Fat ~ 1 + (1|Lab:Technician:Sample), data=eggs)
exactRLRT(cmods, cmod, cmodr)

VarCorr(cmodr)
confint(cmod, method="boot")

# Crossed Effects
data(abrasion)
matrix(abrasion$material,4,4)

ggplot(abrasion,aes(x=material, y=wear, shape=run, color=position))+geom_point(position = position_jitter(width=0.1, height=0.0))+scale_color_grey()
lmod <- aov(wear ~ material + run + position, abrasion)
summary(lmod)

mmod <- lmer(wear ~ material + (1|run) + (1|position), abrasion)
sumary(mmod)

mmodp <- lmer(wear ~ material + (1|position), abrasion)
mmodr <- lmer(wear ~ material + (1|run), abrasion)
exactRLRT(mmodp, mmod, mmodr)
exactRLRT(mmodr, mmod, mmodp)

mmod <- lmer(wear ~ material + (1|run) + (1|position), abrasion,REML=FALSE)
nmod <- lmer(wear ~ 1+ (1|run) + (1|position), abrasion,REML=FALSE)
KRmodcomp(mmod, nmod)

# Multilevel Models
data(jsp)
jspr <- jsp[jsp$year==2,]
ggplot(jspr, aes(x=raven, y=math))+xlab("Raven Score")+ylab("Math Score")+geom_point(position = position_jitter(),alpha=0.3)
ggplot(jspr, aes(x=social, y=math))+xlab("Social Class")+ylab("Math Score")+geom_boxplot()

glin <- lm(math ~ raven*gender*social,jspr)
anova(glin)

glin <- lm(math ~ raven*social,jspr)
anova(glin)

glin <- lm(math ~ raven+social,jspr)
summary(glin)

table(jspr$school)

mmod <- lmer(math ~ raven*social*gender+(1|school)+(1|school:class),  data=jspr)
mmodr <- lmer(math ~ raven*social+(1|school)+(1|school:class),  data=jspr)
KRmodcomp(mmod, mmodr)

all3 <- lmer(math ~ raven*social*gender+(1|school)+(1|school:class), data=jspr, REML=FALSE)
all2 <- update(all3, . ~ . - raven:social:gender)
notrs <- update(all2, . ~ . -raven:social)
notrg <- update(all2, . ~ . -raven:gender)
notsg <- update(all2, . ~ . -social:gender)
onlyrs <- update(all2, . ~ . -social:gender - raven:gender)
all1 <-  update(all2, . ~ . -social:gender - raven:gender - social:raven)
nogen <- update(all1, . ~ . -gender)

anova(all3, all2, notrs, notrg, notsg, onlyrs, all1, nogen)[,1:4]

jspr$craven <- jspr$raven-mean(jspr$raven)
mmod <- lmer(math ~ craven*social+(1|school)+(1|school:class),jspr)
sumary(mmod)

diagd <- fortify.merMod(mmod)
ggplot(diagd,aes(sample=.resid))+stat_qq()
ggplot(diagd,aes(x=.fitted,y=.resid)) +geom_point(alpha=0.3) +geom_hline(yintercept=0) +xlab("Fitted") +ylab("Residuals")

qqnorm(ranef(mmod)$school[[1]],main="School effects")
qqnorm(ranef(mmod)$"school:class"[[1]],main="Class effects")

adjscores <- ranef(mmod)$school[[1]]

rawscores <- coef(lm(math ~ school-1,jspr))
rawscores <- rawscores-mean(rawscores)

plot(rawscores,adjscores)
sint <- c(9,14,29)
text(rawscores[sint],adjscores[sint]+0.2,c("9","15","30"))

mmodc <- lmer(math ~ craven*social+(1|school:class),jspr)
mmods <- lmer(math ~ craven*social+(1|school),jspr)
exactRLRT(mmodc, mmod, mmods)
exactRLRT(mmods, mmod, mmodc)

schraven <- lm(raven ~ school, jspr)$fit
mmodc <- lmer(math ~ craven*social+schraven*social+(1|school)+  (1|school:class),jspr)
KRmodcomp(mmod, mmodc)
