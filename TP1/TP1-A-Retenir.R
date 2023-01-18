#   Affectation : <-
a <- (1+sqrt(5))/2





#       Structures



# 1. Vecteur : variable quantitative




# regroupe une collection de données de même type

c(1, 2, 3, 4, 5) # c() pour concatenate
notes <- c(notes, 4) # Ajout élément

#     pour les suites de pas 1 on peut abréger c(1:5)
#     peut concatener des vecteurs
#     peut faire des opérations (+ * - /)
#     peut faire des tests (== < > <= >=)

# Extraction d'un vecteur directement
v[1]
v[2:4] # On extrait le vecteur formé par les cases 2, 3 et 4
v[c(1, 3, 5)] # On extrait le vecteur formé par les cases 1, 3 et 5

# Extraction d'un vecteur par masque
choix <- c(FALSE, FALSE, TRUE, FALSE, TRUE) # ou via un test choix <- v > pi
v[choix]

floor() # arrondi la valeur en paramètre
min() # retourne la valeur minimum d'un vecteur en paramètre





# 2. Facteur : variable qualitative




# spécifie les modalités (levels) de la variable qualitative

(f <- factor(c("R", "V", "B", "V"))) # factor créer un facteur, () force l'affichage

# Meme extraction, pas de calcul

f <- factor(collection, ordered = TRUE) # trier par ordre alphabétique
f <- factor(collection, ordered = TRUE, levels = c("R", "V", "B")) # trier par levels
# nlevels retourne le nombre de level





# 3. Tableaux individus–variables




# spécification des colonnes en paramètre

v <- 5:10
f <- factor(c("R", "V", "B", "R", "V", "B"))
X <- data.frame(v, f, v > 7)

read.csv("nom") # peuvent venir de fichier csv

head(X) # retourne le début d'un tableau
summary(X) # retourne min quantile median mean max

# Extraction

X[limite_dim_1 , limite_dim_2] # 'median' ou 2 pour limite ou X$median > 10




#       Statistiques descriptives



mean() # moyenne
sd() #
var() # variance
median() # médiane
max()
min()
IQR() # inter quartile
quantile() # les différents quartiles
sort() # tri le paramètre



#       Analyse univariée



# 1. Variables qualitatives


t <- table(X$correcteur.median) # table de contingence
barplot(t) # trace le diagramme en bandes



# 2. Variables quantitatives

notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)
boxplot(notes) # Boîte à moustaches


stem(X$moyenne) # Diagramme en tige et feuilles


hist(X$final) # Histogramme
h <- hist(X$final, breaks=c(0, 15, 20)) # Histogramme avec découpage

h$density 




#       Analyse bivariée



# 1. Quantitative

plot(X$median, X$final)
plot(X$final ~ X$median, data=X) # graphique de dispersion final(sortie) en fonction du médian(entrée) 

# 2. Quantitative vs qualitative


boxplot(X$final ~ X$correcteur.final, data=X)


stripchart(X$final ~ X$correcteur.final, data=X, method="jitter") 
# graphique de dispersion unidimensionnel et jitter permet de mieux voir si valeur similaire
