# R3 for ECON 474

# Set up a working directory
# setwd("C:/Users/hubst/ECON-474/ECON-474/Lecture Notes")

# read data
draft_data <- read.csv("lalonde.csv", header = TRUE)
attach(lalonde)

# create a binary variable (> 0, == 1)
lalonde$u78 <- as.numeric(re78 > 0)
# or
lalonde$u78 <- ifelse(lalonde$re78 > 0, 1, 0)

# statistics
summary(lalonde)
mean(lalonde$treat)
sd(lalonde$treat)

# calculate mean and sd of all the variables
mean <- apply(lalonde, 2, mean) #2 indicates column

sd <- apply(lalonde, 2, sd)

# combine the results
cbind(mean,sd)

# calculate mean and sd by group (use this for problem set)
aggregate(lalonde, by = list(treat == 1, treat == 0), mean)
mean_gp <- aggregate(lalonde, by = list(treat), mean)
sd_gp <- aggregate(lalonde, by = list(treat), sd)

# create stat table
stat_tab <- matrix(rep(0, 66), 11, 6)
stat_tab[,1] <- t(mean_gp[2, -c(1, 2, 11)]) # treatment mean
stat_tab[,3] <- t(mean_gp[1, -c(1, 2, 11)]) #control mean
stat_tab[,2] <- t(sd_gp[2, -c(1, 2, 11)]) # treatment sd
stat_tab[,4] <- t(sd_gp[1, -c(1, 2, 11)]) #control sd
stat_tab[,5] <- stat_tab[,1] - stat_tab[,3]
stat_tab[,6] <- stat_tab[,5]/sqrt((stat_tab[,2]^2 + stat_tab[,4]^2)/2)

# regression with robust se
# install.packages: lmtest, sandwich

reg1 <- lm(re78~treat, data~lalonde)
reg2 <- lm(re78~treat + re74 + re75, data = lalonde)
reg3 <- lm(re78~treat + re74 + re75 + education  + married, data = lalonde)

coeftest(reg1, vcov = sandwich)
coeftest(reg2, vcov = sandwich)
coeftest(reg3, vcov = sandwich)