# Q1)

chisq1 <- function(mu, sigma, n){
  x <- rnorm(n, mean = mu, sd = sigma)
  real <- (n-1) * sd(x)**2 /  sigma**2
  return(real)
}


chisq1000 <- replicate(1000, chisq1(5, 2, 1000))
hist(chisq1000, freq = FALSE)
curve(dchisq(x, df=n-1), add = TRUE)


# Q2)

t1 <- function(mu, sigma, n){
  x <- rnorm(n, mean = mu, sd = sigma)
  real <- (mean(x) - mu) / (sd(x) / sqrt(n))
  return(real)
}
t1000 <- replicate(1000, t1(5, 2, 100))
hist(t1000, freq = FALSE)
curve(dt(x, df=n-1), add = TRUE)


# Q3)

alpha <- 0.05
mu <- 5
sd <- 2
n <- 100

x <- rnorm(n, mu, sd = sd)
IC_min <- mean(x) - qnorm(1-alpha/2) * sqrt(sd**2 / n)
IC_max <- mean(x) + qnorm(1-alpha/2) * sqrt(sd**2 / n)
IC_min
IC_max


# Q4)

x <- rnorm(n, mu, sd = sd)
IC_min <- mean(x) - qt(1-alpha/2, df=n-1) * sqrt(sd(x)**2 / n)
IC_max <- mean(x) + qt(1-alpha/2, df=n-1) * sqrt(sd(x)**2 / n)
IC_min
IC_max
t.test(x, conf.level = 1 - alpha)$conf.int

# Q5)

gen_IC <- function(x, alpha){
  n <- length(x)
  IC_min <- mean(x) - qt(1-alpha/2, df=n-1) * sqrt(sd(x)**2 / n)
  IC_max <- mean(x) + qt(1-alpha/2, df=n-1) * sqrt(sd(x)**2 / n)
  
  return(c(IC_min, IC_max))
}

x <- rnorm(100, 5, sd = 2)
gen_IC(x, 0.05)

# Q6)

alpha <- 0.05
gen_IC_1000 <- replicate(1000, gen_IC(rnorm(100, 5, sd=2), alpha))
gen_IC_1000

# Q7)
setwd("/home/user/SY02/TP/TP4")
source("utils.R")

a <- plot_ICs(gen_IC_1000, 5)
100 * a$hit / 1000


# Q8)

# augm n replicate
gen_IC_10 <- replicate(50, gen_IC(rnorm(100, 5, sd=2), alpha))
gen_IC_100 <- replicate(100, gen_IC(rnorm(100, 5, sd=2), alpha))
gen_IC_1000 <- replicate(1000, gen_IC(rnorm(100, 5, sd=2), alpha))

# augm n echantillon
gen_IC_10_2 <- replicate(100, gen_IC(rnorm(50, 5, sd=2), alpha))
gen_IC_100_2 <- replicate(100, gen_IC(rnorm(500, 5, sd=2), alpha))
gen_IC_1000_2 <- replicate(100, gen_IC(rnorm(5000, 5, sd=2), alpha))

par(mfrow=c(2, 3))
plot_ICs(gen_IC_10, 5, xlim = c(4, 6))
plot_ICs(gen_IC_100, 5, xlim = c(4, 6))
plot_ICs(gen_IC_1000, 5, xlim = c(4, 6))
plot_ICs(gen_IC_10_2, 5, xlim = c(4, 6))
plot_ICs(gen_IC_100_2, 5, xlim = c(4, 6))
plot_ICs(gen_IC_1000_2, 5, xlim = c(4, 6))


# Q9)
gen_IC_2 <- replicate(100, gen_IC(rnorm(100, 5, sd=2), alpha))
gen_IC_5 <- replicate(100, gen_IC(rnorm(100, 5, sd=5), alpha))
gen_IC_10 <- replicate(100, gen_IC(rnorm(100, 5, sd=10), alpha))

par(mfrow=c(1, 3))
plot_ICs(gen_IC_2, 5, xlim = c(2, 8))
plot_ICs(gen_IC_5, 5, xlim = c(2, 8))
plot_ICs(gen_IC_10, 5, xlim = c(2, 8))

# Q10)

hit <- function(alpha){
  x<-rnorm(100, mu, sd=2)
  IC <- gen_IC(x, alpha)
  return (mu >= IC[1] & mu <= IC[2])
}

mu <- 5
hit(0.05)

taux <- replicate(10000, hit(0.05))
mean(taux)


# Q11)

prop <- function(p, n, k, alpha){
  sim <- function(){
    x <- rbinom(n, 1, p) 
    m <- mean(x)
    IC <- m + c(-1, 1) * qnorm(1- alpha / 2) * sqrt(m * (1 - m) / n)
    return(p >= IC[1] & p <= IC[2])
  }
  return(mean(replicate(k, sim())))
}

prop2 <- function(p, n, k, alpha){
  sim <- function(){
    x <- rbinom(1, n, p) 
    m <- x/n
    IC <- m + c(-1, 1) * qnorm(1- alpha / 2) * sqrt(m * (1 - m) / n)
    return(p >= IC[1] & p <= IC[2])
  }
  return(mean(replicate(k, sim())))
}


prop(0.5, 100, 1000, 0.05)
prop2(0.5, 100, 1000, 0.05)

# Q12)
p <- 0.02
k <- 10000
alpha <- 0.05

ns <- floor(10^seq(1, 4, length.out = 30))

slpt <- sapply(ns, function(n) prop(p, n, k, alpha))
slpt2 <- sapply(ns, function(n) prop2(p, n, k, alpha))

dev.off()
plot(log10(ns), slpt, type = "l", col = "red")
plot(log10(ns), slpt2, type = "l", col = "red")


# Q13)

prop3 <- function(p, n, k, alpha){
  sim <- function(){
    x <- rbinom(1, n, p) 
    m <- x/n
    u <- qnorm(1- alpha / 2)
    
    IC <- ((2*n*m + u**2) + c(-1, 1) * u * sqrt(u**2 + 4*n*m*(1 - m)))/ (2*n + 2 * u**2)
    return(p >= IC[1] & p <= IC[2])
  }
  return(mean(replicate(k, sim())))
}

slpt3 <- sapply(ns, function(n) prop3(p, n, k, alpha))

plot(log10(ns), slpt, type = "l", col = "red")
lines(log10(ns), slpt3, col = "green")