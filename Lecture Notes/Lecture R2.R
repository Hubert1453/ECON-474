# R2 for ECON 474

# Set up a working directory
# setwd(C:\Users\hubst\ECON-474\ECON-474)

# read data
draft_data <- read.csv("draft.csv", header = TRUE)
attach(draft_data)

# run a regression
# lnwage = beta1 + beta2*veteran + error

lm(lnwage ~ veteran)
reg1 <- lm(lnwage ~ veteran, data = draft_data)

# s.e. is homoskedastic
out1 <- summary(reg1)

names(reg1)

# lnwage = beta1 + beta2*veteran + beta3*birthyr + error

reg2 <- lm(lnwage ~ veteran + birthyr)

# lnwage = beta1 + beta2*veteran + beta3*(birthyr*draft) + error

reg3 <- lm(lnwage ~ veteran + birthyr*draft)

# lnwage = beta1 + beta2*veteran + beta3*(birthyr)^2 + error

reg4 <- lm(lnwage ~ veteran + I(birthyr^2))

draft_data$birthyr2 = draft_data$birthyr^2

reg5 = lm(lnwage ~ veteran + birthyr2, data = draft_data)

# homoskedasticity s.e. 
# sqrt((1/(n - 2)*sum(residual^2))/(var(x)*(n - 1)))

sqrt(out1$sigma^2/(var(draft_data$veteran)*(dim(draft_data)[1]-1)))

# heteroskedasticity s.e.
# sqrt(sum(v^2)/(var(x)*(n - 1))^2), v = (x - x_bar)*residual

v = (draft_data$veteran - mean(draft_data$veteran))*reg1$residuals
sqrt(sum(v^2)/(var(draft_data$veteran)*(dim(draft_data)[1]-1))^2)

# How to obtain robust heteroskedastic s.e.
# install 2 packages
install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

coeftest(reg1, vcov = sandwich)


