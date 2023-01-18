# Révisions TP

# outils de math déjà présents : pi, log(), sin() etc...

# affectation
a <- expr
a = expr


# vecteur
# données de même type, deux écritures possibles :
c(1, 2, 3, 4)
1:4

# concaténer les vecteurs :
c(1, 2, 3, 4, 5)
c(1, c(2, 3), c(4, 5))

# opération sur chaque elt du vecteur
notes + 1
notes == 4



# extraire un sous vecteur
notes[7:9] # l'indexage commence à 1
notes[c(1, 5, 6)] # vecteur composé des cases 1, 5 et 6
notes[1] + notes[3]
notes10[notes10>6] # masque


floor() # partie entière inf
ceil() # partie entière sup



# facteur : vecteur pour des variables qualitatives
# création :
(collection <- c("R", "V", "B", "V"))
(f <- factor(collection))

# si on veut mettre un ordre :

(f <- ordered(collection))
f <- factor(collection, ordered = TRUE) # ordre alphabétique par défaut
f <- factor(collection, ordered = TRUE, levels=c("R", "V", "B")) # changer l'ordre par défaut
f > "B" # on peut faire des tests de comparaison


adn <- c("A","C","A","A","G","A","T","G","C","C","A","T","T","G","T","C")
f_adn <-(factor(adn))
f_adn
nlevels(f_adn)
levels(f_adn)

for (nucl in levels(f_adn)) {
  print(length(f_adn[f_adn == nucl]))
}

# tableaux individus-variables
# il faut spécifier les colonnes en argument (vecteur quand données quantitatives et facteur quand qualitatives)

v <- 5:10
f <- factor(c("R", "V", "B", "R", "V", "B"))
X <- data.frame(v, f, v > 7) # data.frame à 3 colonnes, int, char et bool
X

# data.frame peut être stocké au format csv
# avec read.csv, on peut charger un fichier csv, soit avec le chemin absolu, soit en changeant le répertoire courant :
setwd("/home/user/Bureau/UTC_GI01/SY02/TP/TP1")

X <- read.csv("sy02.data")
X

length(X) # nb colonnes
ncol(X) # nb colonnes
nrow(X) # nb lignes
names(X) # nom colonnes


head(X) # premières lignes
summary(X)

X["median"]
X$median
X[1, 1]
X[, 3]
X[4 ,]
X[1:10, ]
X[c(1, 3), c(1, 4)]

X[, c(2,ncol(X))]


# masque
X[X$median > 10, ]


mean(X[X$correcteur.median == "EG", ]$median)
mean(X[X$correcteur.median == "EG", "median"])


mean(X$final)
sd(X$final)
var(X$final)
median(X$final)
max(X$final)
min(X$final)
summary(X$final)

quantile(X$final)
quantile(X$final, 0.40)
quantile(X$final, 0.5)

quantile(X$final, 0.75) - quantile(X$final, 0.25)
IQR(X$final)

notes <- sort(X$median)
mean(notes[11 : length(notes) - 10]) # moyenne empirique tronquée d'ordre 10

adn <- c("A","C","A","A","G","A","T","G","C","C","A","T","T","G","T","C")
f_adn <-(factor(adn))
(table <- table(adn))
barplot(table)

(table <- table(X$correcteur.median))
barplot(table)

notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)
boxplot(notes)
boxplot(X$final)


final <- X$final
q <- quantile(final, 0.25)
moustache_inf <- sort(final[final > q])[1]
abberant <- moustache_inf - (1.5 * IQR(final))
length(final[final < abberant])


final <- X$final
abberant <- quantile(final, 0.25) - (1.5 * IQR(final))
length(final[final <= abberant])


stem(X$moyenne)

hist(X$final)

h <- hist(X$final, breaks = c(0, 15, 20))

h$density

sum(diff(h$breaks) * h$density)

plot(X$median, X$final)
plot(median ~ final, data = X)

c(final, data=X$final[X$correcteur.final == "DH"])
X$final[X$correcteur.final == "DH"]


boxplot(X$final[X$correcteur.final == "DH"])

stripchart(final ~ correcteur.final, data=X)

stripchart(final ~ correcteur.final, data=X, method="jitter")

# Révisions TP2

# charger une librairie et visualier un dataframe
library(MASS)
head(painters)
View(painters) # permet de voir en détail le contenu d'un variable

# Q1
par(mfrow = c(2, 2)) # sépare l'affichage en une matrice 2*2
dev.off() # remet l'affichage normal, équivalent à par(mfrow = c(1, 1))
hist(painters$Composition)
hist(painters$Expression)
#...

# Q2
moy <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression) / 4
tab <- painters
tab$moyenne <- moy
tab <- subset(tab, select = -c(Composition:School)) # enlever des colonnes

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


# Q4
mean(tab$moyenne)
var(tab$moyenne)
var_n <- var(tab$moyenne) * (n-1)/n
sd(tab$moyenne)
eq_n <- sqrt(var_n)


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


#créer une fonction
carre <- function(x){
  y <- x**2
  x<- 0
  return(y)
}
carre(5)
carre(c(2, 4, 6))
carre(2:4)



# dessiner la courbe d'une fonction
curve(dloi(x, 3), from = -5, to = 5)

# comparer courbe fonction densité et histo génération empirique
res1 <- rloi(10**6,  3)
histo <- hist(res1, breaks = 100, freq = FALSE)
curve(dloi(x, 3), from = -1, to = 5, add = TRUE)
