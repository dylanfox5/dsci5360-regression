### Repeated Measures and Longitudinal Data

library(faraway)
library(ggplot2)


# Longitudinal Data
data(psid)
head(psid)

library(dplyr)
psid20 <- filter(psid, person <= 20)
ggplot(psid20, aes(x=year, y=income))+geom_line()+facet_wrap(~ person)

ggplot(psid20, aes(x=year, y=income+100, group=person)) +geom_line() + facet_wrap(~ sex) + scale_y_log10()

lmod <- lm(log(income) ~ I(year-78), subset=(person==1), psid)
coef(lmod)

library(lme4)
ml <- lmList(log(income) ~ I(year-78) | person, psid)

intercepts <- sapply(ml,coef)[1,]
slopes <- sapply(ml,coef)[2,]
plot(intercepts,slopes,xlab="Intercept",ylab="Slope")
psex <- psid$sex[match(1:85,psid$person)]
boxplot(split(slopes,psex))

t.test(slopes[psex=="M"],slopes[psex=="F"])
t.test(intercepts[psex=="M"],intercepts[psex=="F"])

psid$cyear <- psid$year-78
mmod <- lmer(log(income) ~ cyear*sex +age+educ+(cyear|person),psid)
sumary(mmod, digits=3)

library(pbkrtest)
mmod <- lmer(log(income) ~ cyear*sex +age+educ+(cyear|person),psid, REML=FALSE)
mmodr <- lmer(log(income) ~ cyear + sex +age+educ+(cyear|person),psid, REML=FALSE)
KRmodcomp(mmod,mmodr)

confint(mmod, method="boot")

diagd <- fortify.merMod(mmod)
ggplot(diagd,aes(sample=.resid))+stat_qq()+facet_grid(~sex)


diagd$edulevel <- cut(psid$educ,c(0,8.5,12.5,20), labels=c("lessHS","HS","moreHS"))
ggplot(diagd, aes(x=.fitted,y=.resid)) + geom_point(alpha=0.3) + geom_hline(yintercept=0) + facet_grid(~ edulevel) + xlab("Fitted") + ylab("Residuals")

# Repeated Measures
data(vision)
vision$npower <- rep(1:4,14)
ggplot(vision, aes(y=acuity, x=npower, linetype=eye)) + geom_line() + facet_wrap(~ subject, ncol=4) + scale_x_continuous("Power",breaks=1:4,labels=c("6/6","6/18","6/36","6/60"))

mmod <- lmer(acuity~power + (1|subject) + (1|subject:eye),vision)
sumary(mmod)

4.64^2/(4.64^2+3.21^2+4.07^2)
(4.64^2+3.21^2)/(4.64^2+3.21^2+4.07^2)


mmod <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision,REML=FALSE)
nmod <- lmer(acuity~1+(1|subject)+(1|subject:eye),vision,REML=FALSE)
KRmodcomp(mmod, nmod)

mmodr <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision,REML=FALSE, subset=-43)
nmodr <- lmer(acuity~1+(1|subject)+(1|subject:eye),vision,REML=FALSE, subset=-43)
KRmodcomp(mmodr, nmodr)

op <- options(contrasts=c("contr.helmert", "contr.poly"))
mmodr <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision,subset=-43)
sumary(mmodr)

options(op)
contr.helmert(4)

plot(resid(mmodr) ~ fitted(mmodr),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(ranef(mmodr)$"subject:eye"[[1]],main="")

# Multiple Response Multilevel Models
data(jsp)
jspr <- jsp[jsp$year==2,]
mjspr <- data.frame(rbind(jspr[,1:6],jspr[,1:6]),  subject=factor(rep(c("english","math"),c(953,953))),  score=c(jspr$english/100,jspr$math/40))
ggplot(mjspr, aes(x=raven, y=score))+geom_jitter(alpha=0.25)+facet_grid(gender ~ subject)

mjspr$craven <- mjspr$raven-mean(mjspr$raven)
mmod <- lmer(score ~ subject*gender + craven*subject + social + (1|school) + (1|school:class) + (1|school:class:id),mjspr)
sumary(mmod)


mmod <- lmer(score ~ subject*gender+craven*subject+social+  (1|school)+(1|school:class)+(1|school:class:id),mjspr, REML=FALSE)
mmodr <- lmer(score ~ subject*gender+craven+subject+social+(1|school)+(1|school:class)+(1|school:class:id),mjspr, REML=FALSE)
KRmodcomp(mmod, mmodr)
0.101^2/(0.101^2+0.117^2)

diagd <- fortify(mmod)
ggplot(diagd, aes(x=.fitted,y=.resid)) + geom_point(alpha=0.3) + geom_hline(yintercept=0) + facet_grid(~ subject) + xlab("Fitted") + ylab("Residuals")
