# Q1
a <- log(640320**3 + 744)/sqrt(163)
print(a==pi)

# Q2
notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10.)

# Q3
notes_update <- c(notes, 4)

v <- c(1, 2, 3, 4, 5)
u <- c(5, 4, 3, 2, 1)
v + 1
2*v
v/3
u * v
u == 2
v > pi

# Q4
notes10 <- notes_update/2
notes10
length(notes10[notes10 > 6])

# Q5
mean(c(notes10[1], notes10[length(notes10)], notes10[3]))

# Q6
choix <- notes_update > 10.
res <- notes_update[choix]
length(res)

# Q7
non_fract <- notes_update == round(notes_update)
res <- notes_update[non_fract]
min(res)

# Q8
notes2 <- notes_update - 2
notes2

# Q9
choix <- notes2 < 0.
res <- notes2[choix]
length(res)

notes2[choix] <- 0
notes2

# Q10
ADN <- factor(c("A", "C", "A", "A", "G", "A", "T", "G", "C", "C", "A", "T", "T", "G", "T", "C"))
levels(ADN)
nlevels(ADN)


# Q11
A_nb <- length(ADN[c(ADN =="A")])
C_nb <- length(ADN[c(ADN =="C")])
G_nb <- length(ADN[c(ADN =="G")])
T_nb <- length(ADN[c(ADN =="T")])


# Q12
X <- read.csv("sy02.data")
length(X)
ncol(X)
nrow(X)
names(X)

# Q13
head(X)
summary(X)

# Q14
X[,c(2, ncol(X))]

# Q15
mean(X[X$correcteur.median == "EG", 'median'])
mean(X[X$correcteur.median == "EG", 2])

# Q16
notes_median <- X$median
notes_final <- X$final
progression <- notes_final - notes_median
length(progression[progression > 0]) / length(progression) * 100


# Q17
mean(X$median)
sd(X$median)
var(X$median)
median(X$median)
max(X$median)
min(X$median)

# Q18 
summary(X$median)
quantile(X$median)

# Q19
IQR(X$median)

# Q20
SY02_sorted <- sort(X$median)
taille <- length(SY02_sorted) - 10
SY02_sorted <- SY02_sorted[11:taille]
mean(SY02_sorted)


# Q21
t <- table(X$correcteur.median)
barplot(t)

# Q22
boxplot(X$final)
summary(X$final)

# Q23
val <- quantile(X$final, 0.25) - 1.5 * IQR(X$final)
length(X$final[X$final < val])

# Q24
stem(X$moyenne)

# Q25
hist(X$final)

# Q26
h <- hist(X$final, breaks=c(0, 15, 20))

# Q27
h$density

# Q28
sum(diff(h$breaks) * h$density)

# Q29
plot(X$median, X$final)
plot(X$final ~ X$median, data=X)

# Q30
boxplot(X$final ~ X$correcteur.final, data=X)

# Q31
final <- X$final[X$correcteur.final == "DH"]
boxplot(final)

# Q32
stripchart(X$final ~ X$correcteur.final, data=X)

# Q33
stripchart(X$final ~ X$correcteur.final, data=X, method="jitter")

