# R4 for ECON 474

# Set up a working directory
# setwd("C:/Users/hubst/ECON-474/ECON-474/Lecture Notes")

# read data
draft_data <- read.csv("lalonde.csv", header = TRUE)
attach(lalonde)

# estimate propensity score, P(D = 1|X) - E(D|X)
logit_out <- glm(treat ~ age+education+black+hispanic+married+nodegree+re74+re75+u74+u75,
    data = lalonde, family = binomial(logit))
lalonde$propensity <- logit_out$fitted.values

prop_quan <- as.numeric(quantile(lalonde$propensity[lalonde$treat == 1], probs = c(0.2, 0.4, 0.6, 0.8)))

# allocate each individual into 5 subclasses
for (i in (1:dim(lalonde)[1])){
  if (lalonde$propensity[i] < prop_quan[1]) lalonde$class[i] = 1
  if (lalonde$propensity[i] >= prop_quan[1] & lalonde$propensity[i] < prop_quan[2]) lalonde$class[i] = 2
  if (lalonde$propensity[i] >= prop_quan[2] & lalonde$propensity[i] < prop_quan[3]) lalonde$class[i] = 3
  if (lalonde$propensity[i] >= prop_quan[3] & lalonde$propensity[i] < prop_quan[4]) lalonde$class[i] = 4
  if (lalonde$propensity[i] >= prop_quan[4]) lalonde$class[i] = 5
}

# crosstab
num_tab <- table(lalonde$class, lalonde$treat)

# conditional ATT and SE
mean_gp <- aggregate(lalonde$re78, by = list(lalonde$class, lalonde$treat), mean)
sd_gp <- aggregate(lalonde$re78, by = list(lalonde$class, lalonde$treat), sd)

catt <- mean_gp[6:10, 3] - mean_gp[1:5, 3]
se_class <- sqrt(sd_gp[6:10, 3]^2/num_tab[,2] + sd_gp[1:5, 3]^2/num_tab[,1])

# overall ATT and SE
att <- sum(catt*num_tab[,2]/sum(num_tab[,2]))
se <- sqrt(sum(se_class^2*(num_tab[,2]/sum(num_tab[,2]))^2))

# built-in function

install.packages("MatchIt")
library(MatchIt)

m_out <- matchit(treat ~ age+education+black+hispanic+married+nodegree+re74+re75+u74+u75,
        data = lalonde, method = "subclass", subclass = 5)

lalonde_pscore <- match.data(m_out)

# crosstab
num_tab <- table(lalonde_pscore$subclass, lalonde_pscore$treat)

# conditional ATT and SE
mean_gp <- aggregate(lalonde_pscore$re78, by = list(lalonde_pscore$subclass, lalonde_pscore$treat), mean)
sd_gp <- aggregate(lalonde_pscore$re78, by = list(lalonde_pscore$subclass, lalonde_pscore$treat), sd)

catt <- mean_gp[6:10, 3] - mean_gp[1:5, 3]
se_class <- sqrt(sd_gp[6:10, 3]^2/num_tab[,2] + sd_gp[1:5, 3]^2/num_tab[,1])

# overall ATT and SE
att <- sum(catt*num_tab[,2]/sum(num_tab[,2]))
se <- sqrt(sum(se_class^2*(num_tab[,2]/sum(num_tab[,2]))^2))

# matching

m_out2 <- matchit(treat ~ age+education+black+hispanic+married+nodegree+re74+re75+u74+u75,
                  data = lalonde, method = "nearest", ratio = 1)

lalonde_matching <- match.data(m_out2)

summary(lm(re78 ~ treat, data = lalonde_matching))