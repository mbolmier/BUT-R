df = read.csv("fao.csv", sep=";", dec=",", header = TRUE)
print(nrow(df))
print(summary(df))

# Exercice 2

# Quelle est la disponibilité alimentaire moyenne mondiale en Kcal/personne/jour ?
print(mean(df$Dispo_alim, na.rm=TRUE))
# Quel est le nombre d’habitant dans le monde ?
print(sum(df$Population, na.rm=TRUE))
# Quel est l’écart-type du volume des exportations de viande ? Et des importations de viande ?
print(sd(df$Export_viande, na.rm=TRUE))
print(sd(df$Import_viande, na.rm=TRUE))
# Quelle est la médiane du volume de production de viande ?
print(median(df$Prod_viande, na.rm=TRUE))
# Calculez les quartiles du nombre de Kcal de disponibilité alimentaire.
print(quantile(df$Dispo_alim))
# Calculez les centiles du volume d’importation de viande.
print(quantile(df$Import_viande, seq(0,1,0.01)))

# Exercice 3

# Construire une requête pour extraire les lignes du dataset avec les 5 pays les moins peuplés.
rang = order(df$Population)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# Construire une requête pour extraire les lignes du dataset avec les 5 pays les plus peuplés.
rang = order(df$Population, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# Construire une requête pour extraire les lignes du dataset avec les 5 pays qui produisent le plus de viande.
rang = order(df$Prod_viande, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# Construire une requête pour extraire les lignes du dataset avec les 5 pays qui importent le plus de viande.
rang = order(df$Import_viande, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# En moyenne, le besoin énergétique moyen d’une adulte est de 2300 kcal par jour. Construire une requête pour extraire
# les lignes du dataset avec les pays qui ont une disponibilité alimentaire supérieure ou égale à 2300 kcal. Combien de pays sont concernés ?
resultat = subset(df, Dispo_alim>=2300)
View(resultat)

# Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire strictement supérieure à 3500 kcal
# et qui importe un volume de viande supérieure ou égale à 1 000 000 tonnes par an. Combien de pays sont concernés ?
resultat = subset(df, Dispo_alim > 3500  & Import_viande > 1000)
View(resultat)

# Construire une requête pour extraire les lignes du dataset avec la France et la Belgique.
resultat = subset(df, Nom %in% c("France","Belgique"))
View(resultat)

# Exercice 4

# Ajouter une colonne nommée part_export qui correspond à la part des exportations de viande par rapport à la production de viande.
df$Part_export<-df$Export_viande/df$Prod_viande

# La colonne Dispo_alim présente la disponibilité alimentaire par personne. Ajouter une colonne nommée dispo_alim_pays qui correspond à la disponibilité total du pays en Kcal/jour.
df$Dispo_alim_pays<-df$Dispo_alim*df$Population

# Exporter le nouveau dataframe dans un fichier csv nommé ExportTp2.csv avec la fonction write.table().
write.table(x = df, file = "ExportTp2.csv")

# Calculer la somme de la disponibilité alimentaire mondiale.
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)
print(dispo_alim_mondiale)

# Sachant qu’en moyenne, le besoin énergétique moyen d’une adulte est de 2300 kcal par jour. Combien d’adulte pourrait-on nourrir avec la disponibilité alimentaire mondiale ?
print(dispo_alim_mondiale/2300)

# Exercice 5

# Représenter graphiquement dans un nuage de points le lien entre Prod_viande et Export_viande. Commenter le lien entre ces deux variables ?
plot(x = df$Prod_viande,
     y = df$Export_viande,
     main = "Pays : Prod_viande / Export_viande")

# Calculer le coefficient de corrélation de cette relation avec la fonction cor().
cor(x = df$Prod_viande,
    y = df$Export_viande)

# Construire la matrice des corrélations des variables quantitatives avec la fonction cor(). Afficher cette matrice dans une vue et arrondisser
# les valeurs avec deux décimales uniquements. Commenter la relation la plus forte, la plus faible.
matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

#Pour mieux visualiser ces corrélations, nous allons utiliser un package qui ne fait pas parti des packages par défaut. Installer le package corrplot avec la fonction install.packages() sauf s'il est déjà installé.
install.packages("corrplot")

# Construire une Corrélogramme avec la fonction corrplot()
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")
