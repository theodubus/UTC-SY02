pi
log(640320**3 + 774)/sqrt(163)

# affectation
a <- (1 + sqrt(5)) / 2

a
a**2
a+1

# vecteur
# données de même type, deux écritures possibles :
c(1, 2, 3, 4)
1:4

notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)

# on peut concaténer les vecteurs, deux notations équivalentes :
c(1, 2, 3, 4, 5)
c(1, c(2, 3), c(4, 5))

notes <- c(notes, 4)
notes

notes + 1 # on peut appliquer une opération sur tous les éléments d'un vecteur
notes == 4

notes10 <- notes / 2
notes10

# notes supérieures à 6
sup6 <- notes10 > 6
length(notes10[sup6])


# extraire un sous vecteur
notes[7:9] # l'indexage commence à 0
notes[c(1, 5, 6)] # vecteur composé des cases 1, 5 et 6
notes[1] + notes[3]

mean(notes10)

sup10 <- notes > 10
length(notes[sup10])


notes_entieres <- notes == floor(notes)
min(notes[notes_entieres])


notes10[2] <- 0
notes10[2:4] <- 10
notes10[notes10 == 10] <- 2
notes10


notes2 <- notes -2
length(notes2[notes2 < 0])
notes2[notes2 < 0] <- 0
notes2


# facteur : vecteur pour des variables qualitatives
# création :
(collection <- c("R", "V", "B", "V"))
(f <- factor(collection))

# si on veut mettre un ordre :

(f <- ordered(collection))
f > "B"
f <- factor(collection, ordered = TRUE)
f < "R"
f <- factor(collection, ordered = TRUE, levels=c("R", "V", "B")) # changer l'ordre par défaut
f < "R"


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
X <- data.frame(v, f, v > 7)
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

X[1, 1]
X[, 3]
X[4 ,]
X[1:10, ]
X[c(1, 3), c(1, 4)]

X[, c(2,ncol(X))]

X$median

# masque
X[X$median > 10, ]


mean(X[X$correcteur.median == "EG", ]$median)
mean(X[X$correcteur.median == "EG", "median"])

prog <- nrow(X[X$median < X$final,])
tot <- nrow(X)
(prog / tot) * 100 

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

quantile(X$final, 0.25)
quantile(X$final, 0.5)
quantile(X$final, 0.75)

quantile(X$final, 0.75) - quantile(X$final, 0.25)
IQR(X$final)


notes <- sort(X$median)
mean(notes[11 : length(notes) - 10])

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

final <- X$final
length(final)
length(final[final <= 15])
length(final[final > 15])

h$density

sum(diff(h$breaks) * h$density)

plot(X$median, X$final)
plot(median ~ final, data = X)

c(final, data=X$final[X$correcteur.final == "DH"])
X$final[X$correcteur.final == "DH"]


boxplot(X$final[X$correcteur.final == "DH"])

stripchart(final ~ correcteur.final, data=X)

stripchart(final ~ correcteur.final, data=X, method="jitter")
