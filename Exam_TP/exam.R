setwd("/home/user/Bureau/UTC_GI01/SY02/Exam_TP/")

load("cctp_A2022.RData")

ls()


#########################
## test sur la moyenne ##
#########################

sample <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
t.test(sample, mu=5)


sample1 <- c(1, 3, 5, 7, 9)
sample2 <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
t.test(x=sample1, y=sample2)

# var.equal = TRUE // variances égales
# paired = TRUE // 2 échantillons appariés
# alternative = "two.sided","less","greater" // préciser le sens
# conf.level = 0.9 // niveau de confiance
help(t.test)

res <- t.test(...)
res$p.value # p value
res$parameter # ddl
res$statistic # stat t

t.test(TEST113, mu=9, alternative = "greater")$p.value
t.test(TEST547$A, TEST547$B, var.equal = TRUE)$p.value
t.test(TEST547$A, TEST547$B)$p.value


t.test(TEST327$A, TEST327$B, paired = TRUE)$p.value

#############################
## test sur une proportion ##
#############################

prop.test(nb_succ, nb_tot, p=proba_test)
# alternative = "two.sided","less","greater" // préciser le sens
# conf.level = 0.9 // niveau de confiance
# correct = False // correction de continuité, TRUE par défaut


# nb_succ et nb_tot peuvent etre de plusieurs dimensions si plusieurs groupes
x <- c(50, 90) # nombre de réussites dans chaque groupe
n <- c(100, 200) # nombre de observations dans chaque groupe

help(prop.test)

prop.test(87, 128, p=0.56)$p.value

##########################
## test sur la variance ##
##########################

var.test(TEST427$A, TEST427$B)$p.value

help(var.test)

#######################
## test de normalité ##
#######################

shapiro.test(X)
help(shapiro.test)

#######################
## test d'adéquation ##
#######################

ks.test(X, "pexp", rate = lambda)
help(ks.test)

##########################################
## test d'adéquation ou d'indépendance  ##
##########################################

help(chisq.test)

tab <- data.frame(...)
chisq.test(tab)

x <- c(10, 20, 30) # effectifs observés dans le premier groupe
y <- c(15, 25, 35) # effectifs observés dans le deuxième groupe
resultat <- chisq.test(x, y, correct = TRUE)


x <- c(10, 20, 30, 40) # effectifs observés dans chaque catégorie
p <- c(0.25, 0.25, 0.25, 0.25) # probabilités attendues sous l'hypothèse de la distribution spécifiée

resultat <- chisq.test(x, p = p, correct = TRUE, rescale.p = TRUE)