
library(MASS)

# Q1)
immer
t.test(immer$Y1, immer$Y2, paired = TRUE, level = 0.95)

# Q2)

vect_bool <- (immer$Y1 - immer$Y2) < 0
vect_bool
x <- sum(vect_bool)
n <- length(vect_bool)
prop.test(x, n, p=0.5)

# Q3)
var.test(shoes$A, shoes$B)

# Q4)
t.test(shoes$A, shoes$B, var.equal = TRUE)

# Q5)
galaxies
shapiro.test(galaxies)

# Q6)
setwd("/home/user/Bureau/UTC_GI01/SY02/TP/TP7/")
X<-read.csv("delai-data.data")$delai
X

lambda = 1/mean(X)
lambda
ks.test(X, "pexp", rate = lambda)


# Q7)
separation <- quantile(X, seq(0, 1, 0.1))

# Q8)
boites <- cut(X, breaks = separation, include.lowest = TRUE)
tab <- table(boites)
tab

length(X)
sum(tab)

# Q9)
pexp(separation[2], lambda)
pexp(separation[3], lambda) - pexp(separation[2], lambda)
pexp(separation[4], lambda) - pexp(separation[3], lambda)

d <- diff(c(0, pexp(separation, lambda)[2:(length(separation)-1)], 1))
sum(d)


# Q10)
chisq.test(tab, p = d)

# Q11)
stat <- chisq.test(tab, p = d)$statistic
1 - pchisq(stat, df=length(tab)-1-1)

# Q12)
glace <- data.frame(chocolat = c(100, 350), vanille = c(120, 200), fraise = c(60, 90), row.names = c("homme", "femme"))

# Q13)
chisq.test(glace)

# Q14)
ct <- chisq.test(glace)

# Q15)
ct$observed
ct$expected

# Q16)
sum((ct$expected - glace)^2/ct$expected)

# Q17)
X<-read.csv("cold.data", row.names = 1)
X
chisq.test(X)
