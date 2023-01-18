library(MASS)

head(painters)
View(painters)

# Q1
par(mfrow = c(2, 2))
par(mfrow = c(1, 1))
dev.off()
hist(painters$Composition)
hist(painters$Drawing)
hist(painters$Colour)
hist(painters$Expression)

# Q2
moy <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression) / 4
View(moy)
tab <- painters
tab$moyenne <- moy
tab <- subset(tab, select = -c(Composition:School))
tab

# Q3
n <- nrow(tab)
s <- sum(tab$moyenne)
moyenne_empirique <- s / n
moyenne_empirique



diff <- sum((tab$moyenne - moyenne_empirique)**2)
variance_ncorr <- diff/n
variance_corr <- diff/(n-1)
ecart_type_ncorr <- sqrt(variance_ncorr)
ecart_type_corr <- sqrt(variance_corr)
variance_ncorr
variance_corr
ecart_type_ncorr
ecart_type_corr


# Q4
mean(tab$moyenne)
var(tab$moyenne)
var_n <- var(tab$moyenne) * (n-1)/n
var_n
sd(tab$moyenne)
eq_n <- sqrt(var_n)
eq_n


# Q5
hist(tab$moyenne)
# Suit une loi normale

# Q6
# unif -> uniforme
# pois -> poisson
# exp -> exponentielle
# binom -> binomiale
# norm -> normale
# t -> t de Student
# chisq -> chi carré
# f -> loi de Fisher

# rajouter le préfixe "d" pour la densité
# "p" pour la fonction de répartition
# "q" pour la fonction quantile
# "r" pour la fonction génératrice de nombres aléatoires

punif(4, min = 2, max = 5)
qunif(0.25, min = 2, max = 5)
runif(1, min = 2, max = 5)
runif(10, min = 2, max = 5)

# Q6.1
1 - pnorm(3, mean=0, sd=1)

# Q6.2
pnorm(42, mean=35, sd=6)

# Q6.3
pnorm(50, mean=35, sd=6) - pnorm(40, mean=35, sd=6)

# Q6.4
for (n in c(5, 10, 30)) {
  print(dbinom(n-1, n, 0.5))
}

# Q6.5
1 - pbinom(14, 20, 0.5)
sum(dbinom(15:20, 20, 0.5))

# Q6.6
pbinom(15, 20, 0.5) - pbinom(9, 20, 0.5)
sum(dbinom(10:15, 20, 0.5))

# Q7
for (a in c(0.05, 0.1, 0.9)) {
  cat("Quartile d'ordre ", a, "pour la loi normale : ", qnorm(a, mean=0, sd=1), "\n")
  cat("Quartile d'ordre ", a, "pour la loi chi² : ", qchisq(a, 10), "\n")
  cat("Quartile d'ordre ", a, "pour la loi student : ", qt(a, 2.5), "\n")
  cat("Quartile d'ordre ", a, "pour la loi student : ", qf(a, 2, 5), "\n")
  cat("\n")
}


# Q8
#créer une fonction
carre <- function(x){
  y <- x**2
  x<- 0
  return(y)
}
carre(5)
carre(c(2, 4, 6))
carre(2:4)


dloi <- function(x, b){
  if (b <= 0){
    stop("B ne peut pas être négatif")
  }
  a <- 2/b**2
  f <- a * x 
  f[x < 0 | x > b] <- 0
  return (f)
}

a <- dloi(c(-1, 0, 1, 2, 3, 4, 5), 3)
a

# Q9
curve(dloi(x, 3), from = -5, to = 5)


# Q10
ploi <- function(x, b){
  if (b <= 0){
    stop("B ne peut pas être négatif")
  }
  f <- (x**2) / (b**2) 
  f[x < 0] <- 0
  f[x > 1] <- 1
  return (f)
}

# Q11
curve(ploi(x, 3), from = -5, to = 5)

# Q12
qloi <- function(alpha, b){
  if (b <= 0) {
    stop("B ne peut pas être inférieur à 0")
  }
  
  if (alpha < 0 || alpha > 1){
    stop("Alpha doit être compris entre 0 et 1")
  }
  
  f <-  sqrt(alpha) * b
  return(f)
}

qloi(1, 3)
curve(qloi(x, 3), from =0, to = 1)

# Q13
rloi <- function(n, b){
  if (b <= 0) {
    stop("B ne peut pas être inférieur à 0")
  }
  
  if (n <= 0)
  {
    stop("La taille de l'échantillon doit être positive")
  }
  
  u <- runif(n, min = 0, max = 1)
  gen_loi <- qloi(u, b)
  
  return(gen_loi)
}

rloi(5, 3)
curve(rloi(x, 3), from =5, to = 8) # zigouigouis

# Q14
res1 <- rloi(10**6,  3)
histo <- hist(res1, breaks = 100, freq = FALSE)
curve(dloi(x, 3), from = -1, to = 5, add = TRUE)
