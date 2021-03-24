lalonde <- read.csv("lalonde.csv", header = TRUE)
attach(lalonde)

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

# 1.

prob1 <- lm(re78 ~ treat)
summary(prob1)

# 2.

coeftest(prob1, vcov = sandwich)

#3


