# TP3

runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}

# sample(a, b) génère un vecteur contenant b valeurs de l'intervalle a
# <<- est un opérateur d'affectation servant à définir des variables globales dans une fonction


# Q1
estim <- function(echantillon) {
  a <- 2 * mean(echantillon)
  return(a)
}


# Q2
estim(runifa(100))
a <- replicate(1000, estim(runifa(100)))

# Q3
mean(a)
boxplot(a)
param

# Q4
# a == sqrtk(E[X^k](k + 1))
estimk <- function(echantillon, k) {
  n <- length(echantillon)
  moment_empirique <- (1/n) * sum(echantillon**k) # == E[X^k]
  a <- (moment_empirique * (k + 1))**(1/k)
  return(a)
}
# test d'une valeur de k
estimk(runifa(100), 250)
a <- replicate(1000, estimk(runifa(100), 250))
boxplot(a)


# test de toutes les valeurs de k
a_values <- c()
for (k in 1:260) {
  a_values <- c(a_values, estimk(runifa(10000), k))
}
plot(a_values)





runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

# Q5
echant <- runknown(1000)
mean(echant)
var(echant)
sd(echant)

# Q6
hist(echant, breaks = 50)

# Q7
plot(ecdf(echant))

# la fonction de répartition monte au niveau de chaque bosse, et stagne ailleurs :
hist(echant, breaks = 50, freq = FALSE)
plot(ecdf(echant), add=TRUE)


# Q8
n <- 1000
echant <- runknown(n)
m <- 7.2
s <- sqrt(32.36)
t <- (mean(echant) - m) / (s / sqrt(n))
t


# Q9
t_values <- c()
for (i in 1:1000){
  
  echant <- runknown(n)
  t <- (mean(echant) - m) / (s / sqrt(n))
  
  t_values <- c(t_values, t)
}

hist(t_values)


# Q10
random.T <- function(n){
  echant <- runknown(n)
  m <- 7.2
  s <- sqrt(32.36)
  t <- (mean(echant) - m) / (s / sqrt(n))
  return(t)
}

t_values <- replicate(1000, random.T(1000))
hist(t_values, freq=FALSE)
curve(dnorm, add=TRUE)
mean(t_values)
var(t_values)

# Q11
plot(ecdf(t_values))

# Q12
curve(pnorm, add=TRUE)

# Q13
t_values <- replicate(5000, random.T(3))
plot(ecdf(t_values))
curve(pnorm, add=TRUE)

t_values <- replicate(5000, random.T(5000))
plot(ecdf(t_values))
curve(pnorm, add=TRUE)

# Q14
f <- function(lambda, x){
  y <- lambda * exp(-lambda * x)
  y[x < 0] <- 0
  return(y)
}

f <- function(lambda, x){
  return(dexp(x, lambda))
}


# Q15
L <- function(lambda, x){
  return(prod(f(lambda, x)))
}


# Q16
logL <- function(lambda, x){
  return(sum(log(f(lambda, x))))
}

# Q17
x <- rexp(100, 3)
L(3.1, x)
L(2.8, x)
logL(3.1, x)
logL(2.8, x)


# Q18
lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
plot(lambdas, logL.lambdas, type = "l")



g <- function(x) {
  -(x - pi)^2
}
(opt <- optimize(g, lower = -10, upper = 10, maximum = TRUE))
opt$maximum

# Q19
(opt <- optimize(function(lambda) logL(lambda, x), lower = 0, upper = 10, maximum = TRUE))
opt$maximum

# Q20
sim.EMV <- function(){
  x <- rexp(100, 3)
  (opt <- optimize(function(lambda) logL(lambda, x), lower = 0, upper = 10, maximum = TRUE))
  return(opt$maximum)
}

# Q21
a <- replicate(10000, sim.EMV())
boxplot(a)
a
mean(a)
var(a)

biais_empirique <- mean(a) - 3

n <- 100
biais_theorique <- ((3*n) / (n-1)) - 3


biais_empirique
biais_theorique

# Q22

# installer une bibliothèque
# install.packages("ma-bibliothèque") 

# changer de bibliothèque
# library(ma-bibliothèque)

# Q22
# install.packages("pracma")
library(pracma)

# Q23
# grad(f, x) calcule la dérivée d'une fonction en un point

sim.Fisher <- function(){
  x <- rexp(100, 3)
  grad(function(lambda) logL(lambda, x), 3)**2 
}

sim.Fisher <- function() {
  x <- rexp(n, 3)
  # Log-vraisemblance par rapport à x
  logLx <- function(lambda) logL(lambda, x)
  # Information de Fisher
  (grad(logLx, 3))^2
}

sim.Fisher()

inf.Fisher <- mean(replicate(1000, sim.Fisher()))
inf.Fisher

n / 3^2

