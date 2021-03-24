# R1 for ECON 474

# Set up a working directory
# setwd("C:/Users/hubst/ECON-474/ECON-474/Lecture Notes")

# read a file (draft.csv)
draft_data <- read.csv("draft.csv", header = TRUE)
attach(draft_data)

# install and load packages
install.packages("STAT")
library(STAT) # require(STAT)

mean(draft_data$lnwage)

a <- 0 # a = 0
b <- c(1, 2, 3, 4, 5) ; c = 1:5
d <- matrix(c(1:20), 5, 4)

# Extract data
b[2]
d[1,2]
d[2,]
d[,4]
b[b > 2] # condition

mean(lnwage[veteran == 1]) - mean(lnwage[veteran == 0])

d[d > 4 & d < 14]
d[d < 4 | d > 14]

# operation
f <- b + c; g = b - c
a^2
sqrt(a)

length(b)
dim(draft_data)
max(b); min(b); mean(b); median(b); sum(b); range(b); var(b); sd(b)

h = as.numeric(b <= 3)
r = ifelse(b <= 3, 1, 0)

# T-test
# H_0: E(lnwage_T) - E(lnwage_C) = 0

# ATE = E(lnwage(1) - lnwage(0)) = E(lnwage_T) - E(lnwage_C) under RCT
# hat_ATE = sample mean(lnwage_T) - sample mean(lnwage_C)
mean(lnwage[veteran == 1]) - mean(lnwage[veteran == 0]) # estimates ATE

t.test(lnwage[veteran == 1], lnwage[veteran == 0])

# H_0: E(X) - E(Y) = 0

# mean(X) - mean(Y) = 0.2
# SE(X_bar - Y_bar) = sqrt(Var(X)/N_x + Var(Y)/N_y)
# t.statistic = 0.2/SE

t.test(lnwage[veteran == 1 & birthyr > 51], lnwage[veteran == 0 & birthyr > 51])
t.test(lnwage ~ veteran) # flips treatment and control group
t.test(lnwage[veteran == 0, lnwage[veteran == 1]]) # flips treatment and control group
