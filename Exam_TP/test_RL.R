setwd("/home/user/Bureau/UTC_GI01/SY02/Exam_TP/")

load("cctp_A2022.RData")

ls()


RL02

x <- RL02$X
y <- RL02$Y

alpha <- 0.05
reg <- lm(Y~X , data = RL02)
n <- length(x)


###################
# estim de b et a #
###################
a <- reg$coefficients[1]
b <- reg$coefficients[2]

#################
# IC sur b et a #
#################
confint(reg, level= 1 - alpha)
# minA maxA
# minB maxB


alpha <- 0.05
n <- length(x)

###################
# estim de sigma #
###################
estim_sigma2 <- 1/n * sum((y - a - b*x)**2)


################
# IC sur sigma #
################
(n-2) * estim_sigma2 / qchisq(1-alpha/2 , df=n-2)
(n-2) * estim_sigma2 / qchisq(alpha/2 , df=n-2)


#############
# Variances #
#############

# variance totale
vt <- (1/n)  * sum((y - mean(y))**2)
SY2 <- mean((y - mean(y))^2)
vt
SY2

# variance expliquée par la régression
vreg <- (1/n)  * sum((reg$fitted.values - mean(y))**2)
Sreg <- mean((reg$fitted.values - mean(y))^2)
vreg
Sreg

# variance résiduelle
vres <- (1/n)  * sum((y - reg$fitted.values)**2)
Sres <- mean(reg$residuals^2)
vres
Sres

# somme
vreg + vres
Sreg + Sres

SY2
vt

############################
# R2 coef de determination #
############################
R2 <- vreg / vt
R2
pred_y = x * b + a
R2 <- cor(y, pred_y, method = c("pearson"))**2

R2 <- summary(reg)$r.squared

#################################
# predictions nouvelles valeurs #
#################################
new <- data.frame(X = c(0.2))
predict(reg, new, interval = "confidence", level=0.90)
predict(reg, new, interval = "prediction", level=0.90)
# prediction IC_min IC_max


############################
# levier et levier extreme #
############################
h<- function (i, x){
  n <- length(x)
  return(1/n + (x[i] - mean(x))^2 / sum((x - mean(x))^2))
}

for (i in 1:n){
  print(h(i, x))
}
seuil <- 2/n * 3


#######################################
# residus simple, standard et student #
#######################################

residu <- function(i, x, y){
  residu <- y[i] - a - b * x[i]
  return(residu)
}

i <- 10
residu(i, x, y)
summary(reg)$residuals[i]
reg$residuals[i]


residu_standard <- function(i, x, y){
  residu <- y[i] - a - b * x[i]
  sigma2 <- 1/(n-2) * sum((y - a - b * x)^2)
  return(residu / sqrt(sigma2 * (1 - h(i, x))))
}

residu_student <- function(i, x, y){
  residu <- y[i] - a - b * x[i]
  sigma2 <- 1/(n-3) * sum((y[-i] - a - b * x[-i])^2)
  return(residu / sqrt(sigma2 * (1 - h(i, x))))
}
for (i in 1:n){
  print(residu_standard(i, x, y))
}
for (i in 1:n){
  print(residu_student(i, x, y))
}

##############################
# Seuil pour points anormaux #
##############################
#|res_student| > tn-3, 1-alpha/2n
seuil <- qt(1-alpha/(2*n), df=n-3)
seuil

