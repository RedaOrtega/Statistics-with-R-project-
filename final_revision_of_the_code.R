## Installation des bibliothèques:
library(dplyr)
library(readxl)
library(ggplot2)
library(ggdist)
library(psych)
library(ltm)
library(plotrix)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(moments)
library(BioStatR)
library(ggplot2)
#Etape 1: Importation des données.

library(readxl)
d <- read_excel("Nouveau dossier/CI1/devoir1_statistique/Data/Questionnaire vierge (réponses).xlsx")
View(d)

#Etap 2: Pre-traitement.

#2.1 conversion des données:
# Remplacer les noms des colonnes par des noms plus courtes.
Likert_QST <- c("Q1N1","Q1N2","Q1N3","Q1N4","Q1N5","Q1N6","Q2N1","Q2N2","Q2N3","Q2N4","Q2N5","Q3N1","Q3N2","Q3N3","Q3N4","Q4N1","Q4N2","Q5N1","Q5N2","Q5N3","Q6N1","Q6N2")

for (i in 6:length(d)) {
  colnames(d)[i] <- Likert_QST[i-5]
}
#fonction qui permet de convertir les répenses dans chaque colonne
convertir_reponses <- function(colonne) {
  colonne[colonne == "d'accord"] <- 2
  colonne[colonne == "tout à fait d'accord"] <- 1
  colonne[colonne == "neutre"] <- 3
  colonne[colonne == "pas d'accord"] <- 4
  colonne[colonne == "pas du tout d'accord"] <- 5
  return(colonne)
}

# Appliquer la fonction à chaque colonne :

d$Q1N1 <- convertir_reponses(d$Q1N1)
d$Q1N2 <- convertir_reponses(d$Q1N2)
d$Q1N3 <- convertir_reponses(d$Q1N3)
d$Q1N4 <- convertir_reponses(d$Q1N4)
d$Q1N5 <- convertir_reponses(d$Q1N5)
d$Q1N6 <- convertir_reponses(d$Q1N6)

d$Q2N1 <- convertir_reponses(d$Q2N1)
d$Q2N2 <- convertir_reponses(d$Q2N2)
d$Q2N3 <- convertir_reponses(d$Q2N3)
d$Q2N4 <- convertir_reponses(d$Q2N4)
d$Q2N5 <- convertir_reponses(d$Q2N5)

d$Q3N1 <- convertir_reponses(d$Q3N1)
d$Q3N2 <- convertir_reponses(d$Q3N2)
d$Q3N3 <- convertir_reponses(d$Q3N3)
d$Q3N4 <- convertir_reponses(d$Q3N4)

d$Q4N1 <- convertir_reponses(d$Q4N1)
d$Q4N2 <- convertir_reponses(d$Q4N2)

d$Q5N1 <- convertir_reponses(d$Q5N1)
d$Q5N2 <- convertir_reponses(d$Q5N2)
d$Q5N3 <- convertir_reponses(d$Q5N3)

d$Q6N1 <- convertir_reponses(d$Q6N1)
d$Q6N2 <- convertir_reponses(d$Q6N2)

#La convertion :

if (!is.numeric(d$`Age`))
  d$Age=as.numeric(d$`Age`)

if (is.character(d$`Genre`))
  d$Genre=as.factor(d$Genre)

if (is.character(d$`Votre filière`))
  d$`Votre filière` = as.factor(d$`Votre filière`)

if (is.character(d$`Année d'étude`))
  d$`Votre filière`=as.factor(d$`Année d'étude`)

for (i in 6:length(d)) {
  if (is.character(d[[i]])) {
    d[[i]] <- as.factor(d[[i]])
  }
}

#3.2 Nettoyage des données  :
#nettoyage des valeurs aberrantes:
table(d$Age)
table(d$Genre)
table(d$Q1N1)
table(d$`Votre filière`)
table(d$`Année d'étude`)
boxplot(d$Age)
#there is no outlier values.
"boxplot.stats(d$Age)$out


for(i in 1:30)
{
  if (d$Age[i]%in%boxplot.stats(d$Age)$out)
  {
    d$Age[i]=NA
  }
}
#3.2.2 Traitement des valeurs manquantes
for(i in 1:30)
{
  if (is.na(d$Age[i]))
  {
    d$Age[i]=mean(d$Age,na.rm=TRUE)
  }
}
d$Scolarité=as.integer(d$Age)"
#Etape 4 : Data Processing.
#4.1 Statistique descriptive univariee (Visualisation des données):
summary(d)
plot(d$Genre)
chisq.test(table(d$Genre))
hist(d$Age)
plot(d$Q1N1)
plot(d$Q1N2)
plot(d$Q1N3)
plot(d$Q1N4)
plot(d$Q1N5)
plot(d$Q1N6)
plot(d$Q2N1)
plot(d$Q2N2)
plot(d$Q2N3)
plot(d$Q2N4)
plot(d$Q2N5)
plot(d$Q3N1)
plot(d$Q3N2)
plot(d$Q3N3)
plot(d$Q3N4)
plot(d$Q4N1)
plot(d$Q4N2)
plot(d$Q5N1)
plot(d$Q5N2)
plot(d$Q5N3)
plot(d$Q6N1)
plot(d$Q6N2)

#4.2 analyse univarie (test de normalite):

shapiro.test(d$Age)
#p-value < 5%
#H1 est acceptée
#test de quasi-normalite :

library(moments)
skewness(d$Age)
kurtosis(d$Age)

#4.3 Analyse bivariée (test d'hypotheses):

#analyse de fiabilite(coherence)
entite0 = data.frame(d$Q1N1,d$Q1N2,d$Q1N3,d$Q1N4,d$Q1N5,d$Q1N6,d$Q2N1,d$Q2N2,d$Q2N3,d$Q2N4,d$Q2N5,d$Q3N1,d$Q3N2,d$Q3N3,d$Q3N4,d$Q4N1,d$Q4N2,d$Q5N1,d$Q5N2,d$Q6N1,d$Q6N2)
cronbach.alpha(entite0)

#l'hypothese 1:
#H0 : il n'y a pas d'association significatif entre la qualité de la formation à l'ENSAK et les exigences de maché du travail.
#H1 : il y a une association significatif entre la qualité de la formation à l'ENSAK et les exigences de maché du travail.

chisq.test(d$Q1N3,d$Q2N1)

#p-value = 0.4306 > 5%.
#conclusion: H0 qui est accepté.

#l'hypothese 2:
#H0 : Il n'y a pas d'association significative entre les compétences acquises grâce à l'autoformation et les attentes du travail.
#H1 : Il y a une association significative entre les compétences acquises grâce à l'autoformation et les attentes du travail.

chisq.test(d$Q1N5,d$Q3N2)

#p-value = 0.7267 > 5%.
#conclusion: H0 qui est accepté.

#l'hypothese 3:
#H0 : Il n'y a pas d'association significative entre les expériences pratiques acquises durant les stages et les co-op et les attentes du marché du travail.
#H1 : Il y a une association significative entre  les expériences pratiques  acquises durant les stages et les co-op et les attentes du marché du travail.

chisq.test(d$Q1N4,d$Q4N1)
#p-value = 0.4427 > 5%.
#conclusion: H0 qui est accepté.

#l'hypothese 4:
#H0 : Il n'y a pas d'association significative entre  le profile professionnelle et le renforcement de l'employabilité.
#H1 : Il y a une association significative entre  le profile professionnelle et le renforcement de l'employabilité.

chisq.test(d$Q1N1,d$Q6N2)
#p-value = 3.136e-08
#conclusion: H1 qui est accepté.

#H0 : Il n'y a pas d'association significative entre les compétences techniques et les projets personnels et l'employabilité.
#H0 : Il y a une association significative entre les compétences techniques et les projets personnels et l'employabilité.

chisq.test(d$Q1N2,d$Q5N3)
#p-value = 0.2428
#conclusion : H0 qui est accepté.

#symetry
mosaicplot(d)
d$Q1N1=as.factor(d$Q1N1)
d$Q1N2=as.factor(d$Q1N2)
d$Q1N3=as.factor(d$Q1N3)
d$Q1N4=as.factor(d$Q1N4)
d$Q1N5=as.factor(d$Q1N5)
d$Q1N6=as.factor(d$Q1N6)
d$Q2N1=as.factor(d$Q2N1)
class1=data.frame(d$Q1N1,d$Q1N2,d$Q1N3,d$Q1N4,d$Q1N5,d$Q1N6,d$Q2N1)
pairs(class1)

d$Q1N1=as.integer(d$Q1N1)
d$Q1N2=as.integer(d$Q1N2)
d$Q1N3=as.integer(d$Q1N3)
d$Q1N4=as.integer(d$Q1N4)
d$Q1N5=as.integer(d$Q1N5)
d$Q1N6=as.integer(d$Q1N6)
d$Q2N1=as.integer(d$Q2N1)
class1<-data.frame(d$Q1N1,d$Q1N2,d$Q1N3,d$Q1N4,d$Q1N5,d$Q1N6,d$Q2N1)
class1s<-scale(class1,center = T,scale=T)
class1d<-dist(class1s)
cah.ward<-hclust(class1d,method="ward.D2")
plot(cah.ward)
d$Q2N2=as.factor(d$Q2N2)
d$Q2N3=as.factor(d$Q2N3)
d$Q2N4=as.factor(d$Q2N4)
d$Q2N5=as.factor(d$Q2N5)
d$Q3N1=as.factor(d$Q3N1)
d$Q3N2=as.factor(d$Q3N2)
d$Q3N3=as.factor(d$Q3N3)
class1=data.frame(d$Q2N2,d$Q2N3,d$Q2N4,d$Q2N5,d$Q3N1,d$Q3N2,d$Q3N3)
pairs(class1)

d$Q2N2=as.integer(d$Q2N2)
d$Q2N3=as.integer(d$Q2N3)
d$Q2N4=as.integer(d$Q2N4)
d$Q2N5=as.integer(d$Q2N5)
d$Q3N1=as.integer(d$Q3N1)
d$Q3N2=as.integer(d$Q3N2)
d$Q3N3=as.integer(d$Q3N3)
class1<-data.frame(d$Q2N2,d$Q2N3,d$Q2N4,d$Q2N5,d$Q3N1,d$Q3N2,d$Q3N3)
class1s<-scale(class1,center = T,scale=T)
class1d<-dist(class1s)
cah.ward<-hclust(class1d,method = "ward.D2")
plot(cah.ward)

d$Q3N4=as.factor(d$Q3N4)
d$Q4N1=as.factor(d$Q4N1)
d$Q4N2=as.factor(d$Q4N2)
d$Q5N1=as.factor(d$Q5N1)
d$Q5N2=as.factor(d$Q5N2)
d$Q5N3=as.factor(d$Q5N3)
d$Q6N1=as.factor(d$Q6N1)
d$Q6N2=as.factor(d$Q6N2)
class1=data.frame(d$Q3N4,d$Q4N1,d$Q4N2,d$Q5N1,d$Q5N2,d$Q5N3,d$Q6N1,d$Q6N2)
pairs(class1)

d$Q3N4=as.integer(d$Q3N4)
d$Q4N1=as.integer(d$Q4N1)
d$Q4N2=as.integer(d$Q4N2)
d$Q5N1=as.integer(d$Q5N1)
d$Q5N2=as.integer(d$Q5N2)
d$Q5N3=as.integer(d$Q5N3)
d$Q6N1=as.integer(d$Q6N1)
d$Q6N2=as.integer(d$Q6N2)
class1<-data.frame(d$Q3N4,d$Q4N1,d$Q4N2,d$Q5N1,d$Q5N2,d$Q5N3,d$Q6N1,d$Q6N2)
class1s<-scale(class1,center = T,scale=T)
class1d<-dist(class1s)
cah.ward<-hclust(class1d,method = "ward.D2")
plot(cah.ward)
#Le modéle statistique : Regression linéaire simple.
#Puisque nos variables sont de type qualitative on ne peut pas leurs appliquer la regression linéaire.