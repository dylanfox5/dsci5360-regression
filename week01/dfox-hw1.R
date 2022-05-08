### dsci 5360 week 1 HW

library(ISLR)
library(tidyverse)
library(ggplot2)
library(naniar)
library(corrplot)
library(coefplot)
library(faraway)

## problem 1
df <- Auto

str(df)
head(df)
summary(df)
gg_miss_var(df)

df1 <- df %>%
  select("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")

plot(df1)
correlations = cor(df1)
corrplot(correlations)

ggplot(df1, aes(x = mpg, y = weight)) + geom_point(aes(color = cylinders))

lm1 = lm(mpg ~ cylinders + horsepower + weight + displacement + year + acceleration, data=df1)
summary(lm1)
coefplot(lm1)

lm2 <- lm(mpg ~ weight + year + cylinders, data=df1)
summary(lm2)

lm2 <- lm(mpg ~ weight + year + acceleration, data=df1)
summary(lm2)

lm2 <- lm(mpg ~ weight + year + horsepower, data=df1)
summary(lm2)

lm2 <- lm(mpg ~ weight + year + displacement, data=df1)
summary(lm2)

lm3 <- lm(mpg ~ weight + year, data=df1)
summary(lm3)
coefplot(lm3)

plot(lm3)
halfnorm(hatvalues(lm3))
termplot(lm3, partial=TRUE, terms=1)

ex <- data.frame(weight=rep(df1$weight), year=rep(df1$year))
pred <- predict(lm3, new=ex)
xtabs(round(pred,3) ~ weight + year, ex)

## problem 2
data("gala")

poi <- glm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, family=poisson, gala)
poi$coefficients
poi$deviance

## problem 3
poi.log <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + Scruz + log(Adjacent), family=poisson, gala)
poi.log$coefficients
poi.log$deviance

## log model is better due to lower deviance