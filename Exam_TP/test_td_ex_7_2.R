vitesse <- c(30, 40, 50, 60, 70, 80, 90, 100, 110)

consommation <- c(0.24, 0.20, 0.38, 4.58, 7.12, 2.49, 6.17, 1.80, 16.15)
consommation <- log(consommation)

# on race la consommation en fonction de la vitesse :
plot(vitesse, consommation)


n <- length(vitesse)
b <- (1/n * sum(vitesse * consommation) - mean(vitesse) * mean(consommation)) / (1/n * sum(vitesse^2) - mean(vitesse)^2)
a <- mean(consommation) - b * mean(vitesse)


sigma2 <- 1/n * sum((consommation - a - b * vitesse)^2)
Sreg <- b^2 * (1/n * sum(vitesse^2) - mean(vitesse)^2)


var_tot <- 1/n * sum((consommation - mean(consommation))^2)
var_tot
Sreg + sigma2

R2 <- Sreg / (Sreg + sigma2)
R2

# on trace la droite de régression :
abline(a, b)

a
b

h<- function (i){
  return(1/n + (vitesse[i] - mean(vitesse))^2 / sum((vitesse - mean(vitesse))^2))
}


for (i in 1:n){
  print(h(i))
}

2/n * 3


residu_standard <- function(i){
  residu <- consommation[i] - a - b * vitesse[i]
  sigma2 <- 1/(n-2) * sum((consommation - a - b * vitesse)^2)
  return(residu / sqrt(sigma2 * (1 - h(i))))
}

residu_student <- function(i){
  residu <- consommation[i] - a - b * vitesse[i]
  sigma2 <- 1/(n-3) * sum((consommation[-i] - a - b * vitesse[-i])^2)
  return(residu / sqrt(sigma2 * (1 - h(i))))
}

for (i in 1:n){
  print(residu_standard(i))
}

for (i in 1:n){
  print(residu_student(i))
}

alpha <- 0.05
qt(1-alpha/(2*n), df=n-3)


residus_standard <- c()
residus_student <- c()
for (i in 1:n){
  residus_standard <- c(residus_standard, residu_standard(i))
  residus_student <- c(residus_student, residu_student(i))
}

plot(vitesse, residus_standard)
abline(0, 0)


# qq plot
qqnorm(residus_standard)

empirical_quantiles <- sort(residus_standard)
theorical_quantiles <- qnorm((1:n)/(n+1))

plot(theorical_quantiles, empirical_quantiles)


log_conso_125 <- a + b * 125
conso_125 <- exp(log_conso_125)
conso_125

IC <- function(x){
  estimateur <- a + b * x
  sigma2 <- 1/(n-2) * sum((consommation - a - b * vitesse)^2)
  t <- qt(1-alpha/2, df=n-2)
  nSx2 <- sum((vitesse - mean(vitesse))^2)
  return( estimateur + c(-1, 1) * t * sqrt(sigma2 * (1/n + (x - mean(vitesse))^2 / nSx2)))
}

IP <- function(x){
  estimateur <- a + b * x
  sigma2 <- 1/(n-2) * sum((consommation - a - b * vitesse)^2)
  t <- qt(1-alpha/2, df=n-2)
  nSx2 <- sum((vitesse - mean(vitesse))^2)
  return( estimateur + c(-1, 1) * t * sqrt(sigma2 * (1 + 1/n + (x - mean(vitesse))^2 / nSx2)))
}

IC(125)
exp(IC(125))

IP(125)
exp(IP(125))