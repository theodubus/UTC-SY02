setwd("/home/user/Bureau/UTC_GI01/SY02/TP/TP6/")

# Q1)


DATA <- read.csv("bottles.data")


t.test(DATA, mu=500, alternative = "less", conf.level = 0.9)
t.test(DATA, mu=500, alternative = "less")


# Q2)
DATA <- read.csv("MM.data")
DATA

1713/6

for (couleur in DATA) {
  print(couleur)
  print(prop.test(x=couleur, sum(DATA), 1/6))
}

prop.test(x=DATA$Red, sum(DATA), 1/6)


# Q3)
# install.packages("isdals")
library(isdals)
data(bodyfat)

reg1 <- lm(Fat~Triceps, data = bodyfat)
reg2 <- lm(Fat~Thigh, data = bodyfat)
reg3 <- lm(Fat~Midarm, data = bodyfat)

par(mfrow=c(2, 2))

plot(bodyfat$Triceps, bodyfat$Fat)
abline(reg1, col="red", lwd=2)
qqnorm(rstandard(reg1))
qqline(rstandard(reg1), col="red", lwd=2)
plot(reg1$fitted.values, rstandard(reg1))
abline(0, 0, col="blue", lwd=2)
confint(reg1)
summary(reg1)

dev.off()
par(mfrow=c(2, 2))

plot(bodyfat$Thigh, bodyfat$Fat)
abline(reg2, col="red", lwd=2)
qqnorm(rstandard(reg2))
qqline(rstandard(reg2), col="red", lwd=2)
plot(reg1$fitted.values, rstandard(reg3))
abline(0, 0, col="blue", lwd=2)
confint(reg2)
summary(reg2)

dev.off()
par(mfrow=c(2, 2))

plot(bodyfat$Midarm, bodyfat$Fat)
abline(reg3, col="red", lwd=2)
qqnorm(rstandard(reg3))
qqline(rstandard(reg3), col="red", lwd=2)
plot(reg1$fitted.values, rstandard(reg3))
abline(0, 0, col="blue", lwd=2)
confint(reg3)
summary(reg3)

dev.off()


# H0 = a = 0, H1 = a != 0 --> inutile
# H0 = b = 0, H1 = b != 0

# Q4)

duree <- c(16.7, 1.1, 0.2, 4.5, 6.2, 19, 1, 0.2, 0.1)
mean(duree)


# W = {xbar <= c} c = 1/lamb0 (1 + Ualpha* / sqrt(n))


# Q5.1)
DATA <- read.csv("delai-data.data")
DATA

n <- length(DATA$delai)
Xbar<- mean(DATA$delai)

lamb0 <- 1/151
c <- 1/lamb0 * (1 + qnorm(0.05)/sqrt(n))

Xbar <= c

# Q5.2)

# p value = Ph0(Xbar <= xbar) = pnorm((xbar - mu1) / sigma)
mu <- 1 / lamb0
sigma <- 1 / sqrt(n * lamb0**2)
pnorm((Xbar - mu) / sigma)

# Q6)

puiss_emp <- function(theta0, theta, n){
  x <- rexp(n, rate = theta)
  c <- 1/theta0 * (1 + qnorm(0.05)/sqrt(n))
  return (mean(x) <= c)
}

# Q7)
theta0 <- 1 / 151
mean(replicate(100, puiss_emp(theta0, theta0, 100)))

# Q8)
thetas <- seq(theta0, theta0 * 1.70, length.out = 100)
thetas

replicate_puiss <- function(theta0, theta, n, k){
  sim <- function(){
    x <- rexp(n, rate = theta)
    c <- 1/theta0 * (1 + qnorm(0.05)/sqrt(n))
    return (mean(x) <= c)
  }
  mean(replicate(k, sim()))
}


n <- length(DATA$delai)
vect_puiss_thetas <- sapply(thetas, function(theta) replicate_puiss(theta0, theta, n, 100))
plot(thetas, vect_puiss_thetas, type = "o", ylab = "", xlab="theta sous H1", xlim = c(theta0, thetas[length(thetas)]), ylim = c(0, 1))

1/151

# Cas d'études
sleep

# H0 = mu = mu0 = 0 (le médicament n'a pas d'effet significatif)
# H1 = mu = mu1 > mu0 (le médicament est efficace)

groupe1 <- sleep$extra[sleep$group == 1]
groupe2 <- sleep$extra[sleep$group == 2]
groupe1
groupe2

shapiro.test(groupe1)
shapiro.test(groupe2)


t.test(groupe1, mu=0, alternative = "greater") # conservation de H0
t.test(groupe2, mu=0, alternative = "greater") # acceptation de H1
