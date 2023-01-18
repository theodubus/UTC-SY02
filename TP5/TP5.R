donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6),
                      vary = c(1.01, 1.44, 1.55, 2.1))

reg <- lm(vary~varx, data = donnees)
reg
plot(reg)
# !!!! y AVANT x

x <- c(0, 0.2, 0.3, 0.6)
y <- c(1.01, 1.44, 1.55, 2.1)

a <- reg$coefficients[1]
b <- reg$coefficients[2]

# Q1)
#a == intercept
#b == varx

# Q2)
plot(x, y)
abline(a, b,col="red",lwd=2)
abline(reg, col="blue", lwd=2)

# abline(a, b) a->ordonnée à l'origine et b-> coef directeur

# Q3)
summary(reg)

# somme des résidus
sum(reg$residuals)

# image de moy(x) par la droite
mean(x) * b + a

# moy(y)
mean(y)

# Q4)

# variance totale
vt <- (1/4)  * sum((y - mean(y))**2)
vt

# variance expliquée par la régression
vreg <- (1/4)  * sum((reg$fitted.values - mean(y))**2)
vreg

# variance résiduelle
vres <- (1/4)  * sum((y - reg$fitted.values)**2)
vres

# somme
vreg + vres
vt


vreg / vt
pred_y = x * b + a

cor(y, pred_y, method = c("pearson"))**2


# Q5)
anscombe
attach(anscombe) # attention cette écriture rend impossible la prédiction de nouveaux points
x1
reg1 <- lm(y1 ~ x1)
reg2 <- lm(y2 ~ x2)
reg3 <- lm(y3 ~ x3)
reg4 <- lm(y4 ~ x4)

a1 <- reg1$coefficients[1]
b1 <- reg1$coefficients[2]
a2 <- reg2$coefficients[1]
b2 <- reg2$coefficients[2]
a3 <- reg3$coefficients[1]
b3 <- reg3$coefficients[2]
a4 <- reg4$coefficients[1]
b4 <- reg4$coefficients[2]
a1
a2
a3
a4
b1
b2
b3
b4
# mêmes coefficients alors que distibution différente

plot(x1, y1)
abline(a1, b1,col="red",lwd=2)
abline(reg1, col="blue", lwd=2)

plot(x2, y2)
abline(a2, b2,col="red",lwd=2)
abline(reg2, col="blue", lwd=2)

plot(x3, y3)
abline(a3, b3,col="red",lwd=2)
abline(reg3, col="blue", lwd=2)

plot(x4, y4)
abline(a4, b4,col="red",lwd=2)
abline(reg4, col="blue", lwd=2)

# le modèle linéaire n'est pas adapté pour toutes les données

# Q6)
# qqnorm(rstandard(reg1))
# hnorm
qqnorm(reg1$residuals)
qqline(reg1$residuals, col="red", lwd=2)
hist(rstandard(reg1), freq=FALSE)
curve(dnorm(x, mean=mean(rstandard(reg1)), sd=sd(rstandard(reg1))),add=TRUE, col="red", lwd=2)


qqnorm(reg2$residuals)
qqline(reg2$residuals, col="red", lwd=2)
hist(rstandard(reg2), freq=FALSE)
curve(dnorm(x, mean=mean(rstandard(reg1)), sd=sd(rstandard(reg1))),add=TRUE, col="red", lwd=2)

qqnorm(reg3$residuals)
qqline(reg3$residuals, col="red", lwd=2)
hist(rstandard(reg3), freq=FALSE)
curve(dnorm(x, mean=mean(rstandard(reg1)), sd=sd(rstandard(reg1))),add=TRUE, col="red", lwd=2)

qqnorm(reg4$residuals)
qqline(reg4$residuals, col="red", lwd=2)
hist(rstandard(reg4), freq=FALSE)
curve(dnorm(x, mean=mean(rstandard(reg1)), sd=sd(rstandard(reg1))),add=TRUE, col="red", lwd=2)

# hhom, hind, hlin
plot(reg1$fitted.values, rstandard(reg1))
plot(x1, rstandard(reg1))
abline(0, 0, col="blue", lwd=2)

plot(reg2$fitted.values, rstandard(reg2))
plot(x2, rstandard(reg2))
abline(0, 0, col="blue", lwd=2)

plot(reg3$fitted.values, rstandard(reg3))
plot(x3, rstandard(reg3))
abline(0, 0, col="blue", lwd=2)

plot(reg4$fitted.values, rstandard(reg4))
plot(x4, rstandard(reg4))
abline(0, 0, col="blue", lwd=2)



# Q7)
setwd("/home/user/Bureau/UTC_GI01/SY02/TP/TP5/")
DATA <- read.csv("hooker-data.data")
DATA

x <- DATA$Temp
y <- DATA$Pression
reg <- lm(Pression~Temp, data = DATA)
a <- reg$coefficients[1]
b <- reg$coefficients[2]
a
b

plot(x, y)
abline(reg, col="blue", lwd=2)

# Q8)
confint(reg, level=0.99) # intervalle de confiance pour a et b

# Q9)
?predict.lm

new <- data.frame(Temp = c(97, 99, 100))
predict(reg, new, interval = "confidence") # intervalle de confiance


# Q10)

# Nt = a exp(bt) <=>  ln(Nt) = ln(a exp(bt)) = ln(a) + bt
# ln(Nt) = ln(a) + bt

DATA <- read.csv("moore-data.data")
DATA

plot(DATA$Date.of.introduction, DATA$Transistor.count) # modèle linéaire non adapté
plot(DATA$Date.of.introduction, log(DATA$Transistor.count)) # modèle linéaire semble adapté

reg <- lm(log(Transistor.count)~Date.of.introduction, data = DATA)
reg

a <- reg$coefficients[1]
b <- reg$coefficients[2]

abline(a, b, col="blue", lwd=2)

IC_b <- confint(reg,"Date.of.introduction")
IC_b

new <- data.frame(Date.of.introduction = c(2018))

exp(predict(reg, new, interval = "confidence"))
exp(predict(reg, new, interval = "prediction"))


# exp(b(t+T))/exp(bt) == 2
# exp(bt + bT) / exp(bt) == 2
# exp(bt) * exp(bT) / exp(bt) == 2
# exp(bT) == 2
# bT = log(2)
# T = log(2) / b

log(2) / IC_b

# Q11)
DATA <- read.csv("cedar-data.data")
DATA

x <- DATA$diameter
y <- DATA$height
reg <- lm(height~diameter, data = DATA)
a <- reg$coefficients[1]
b <- reg$coefficients[2]

plot(x, y)
abline(a, b, col="red", lwd=2)
abline(reg, col="blue", lwd=2)

plot(x, reg$residuals)
plot(reg$fitted.values, reg$residuals)
plot(reg$fitted.values, rstandard(reg))
plot(reg, which=1)

# les résidus semblent être corrélés en fonction des valeurs prédites.

# Q12)
boxcox <- function(x, lambda){
  if (lambda == 0)
    return(log(x))
  else
    return((x**lambda - 1)/lambda)
}

# Q13)
par(mfrow = c(2, 3))
for (lambda in c(-1, -1/2, 0, 1/3, 1/2, 1)){
  plot(x, y, main = paste("lambda = ", lambda))
  DATA$modif_diameter <- boxcox(x, lambda)
  reg <- lm(height ~ modif_diameter, data = DATA)
  print(summary(reg)$r.squared)
  curve(reg$coefficients[1] + reg$coefficients[2] * boxcox(x, lambda), add = TRUE)
}

dev.off()

# Q14)

DATA

x <- log(DATA$diameter)
y <- DATA$height
reg <- lm(height~log(diameter), data = DATA)
a <- reg$coefficients[1]
b <- reg$coefficients[2]

plot(x, y)
abline(a, b, col="red", lwd=2)
abline(reg, col="blue", lwd=2)

plot(reg$fitted.values, rstandard(reg))
plot(reg$fitted.values, reg$residuals)
plot(reg, which = 1)
