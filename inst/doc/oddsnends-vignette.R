## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----out.width="150px"--------------------------------------------------------
knitr::include_graphics("https://github.com/jenineharris/stuff/blob/master/stickerdraft.png?raw=true")

## -----------------------------------------------------------------------------
# enter demo data
sick <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
          0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0)
age <- c(23, 25, 26, 34, 54, 46, 48, 95, 81, 42, 62, 25, 31, 49, 57, 52, 54, 63, 61, 50,
         43, 35, 26, 74, 34, 46, 43, 65, 81, 42, 62, 25, 21, 47, 51, 22, 34, 59, 26, 55)
smoke <- c('Former', 'Former', 'Former', 'Never', 'Current', 'Current', 'Current', 'Current', 'Never', 'Former', 'Never', 'Former', 'Current', 'Former', 'Never', 'Current', 'Current', 'Current', 'Former', 'Never','Former', 'Former', 'Former', 'Never', 'Current', 'Current', 'Current', 'Current', 'Never', 'Former', 'Never', 'Former', 'Current', 'Former', 'Never', 'Current', 'Current', 'Current', 'Former', 'Never')

# create data frame
smokeData <- data.frame(sick, age, smoke)

## -----------------------------------------------------------------------------
# estimate the logistic regression model object
logisticModel <- glm(formula = sick ~ age + smoke, data = smokeData, na.action = na.exclude, family = binomial(logit))

# print model summary for the logistic model object
summary(object = logisticModel)

## -----------------------------------------------------------------------------
# open odds.n.ends package
library(package = "odds.n.ends")

# get the  basics
odds.n.ends(mod = logisticModel)

