print(iris)
print(class(iris))
# View permet d'affichier le jeu de données
View(iris)
# nrow pour afficher le nombre de lignes
print(nrow(iris))
# ncol pour afficher le nombre de colonnes
print(ncol(iris))
# colnames = noms des colonnes
print(colnames(iris))
# summary = résumé du dataframe
print(summary(iris))
# print les colonnes Sep.L et Spec
print(iris[ , c("Sepal.Length","Species")])
# print les lignes
print(iris[ c(100,103,105) , ])
# print les lignes 50 à 100
print(iris[50:100, ])
# mean = moyenne
print(mean(iris$Sepal.Length))
# median = médiane
print(median(iris$Sepal.Length))
# sd = écart-type
print(sd(iris$Petal.Length))
# quantile = quantiles, deciles... (from, to, by)
print(quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1)))

####################################################
#     EXERCIE 2 - Import/Exporter un dataframe     #
####################################################

dfManga = read.csv("/Users/mbolmier/Documents/BUT/Semestre 2/prog/R/manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime = read.csv("/Users/mbolmier/Documents/BUT/Semestre 2/prog/R/anime.csv", header = TRUE, sep = ",", dec = ".")
print(class(dfManga))
print(class(dfAnime))

View(dfManga)
View(dfAnime)
# dim print le nb de lignes et de colonnes
print(dim(dfManga))
print(dim(dfAnime))

# moyenne des scores des mangas et des animes (Anime > Manga)
print(mean(dfManga$Score))
print(mean(dfAnime$Score))

# somme des votes pour chacun (Anime > Manga)
print(sum(dfManga$Vote))
print(sum(dfAnime$Vote))

# ecart-type des notes de score (Manga plus homogène)
print(sd(dfManga$Score))
print(sd(dfAnime$Score))
# déciles des dataframes (Anime à le décile 1 le plus petit)
print(quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1)))
print(quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1)))

# Score des Mangas qui ont un score de plus de 9/10 (10)
MSsup9 = subset(dfManga, Score > 9)
print(nrow(MSsup9))

# Mangas qui ont plus de 200k votes (12)
MVsup200k = subset(dfManga, Vote > 200000)
print(nrow(MVsup200k))

# Mangas qui ont plus de 200k votes et plus de 8/10 (11)
MVS = subset(dfManga, Score > 8 & Vote > 200000)
print(nrow(MVS))

# Mangas avec une note entre 7/10 et 8/10 (8038)
MN78 = subset(dfManga, Score >= 7 & Score <= 8)
print(nrow(MN78))


effectifRating = table(dfAnime$Rating)
print(effectifRating)
print(length(effectifRating))
print(prop.table(effectifRating))

# Animer avec rating = R - 17+ (violence & profanity) (1538)
Avp = subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
print(nrow(Avp))

# Animer avec rating = R - 17+ (violence & profanity) et 8/10 (328)
Avp8 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score >= 8)
print(nrow(Avp8))

# Animer différent de rating = R - 17+ (violence & profanity) (8462)
Adiffvp = subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
print(nrow(Adiffvp))

# Animer avec dans rating : "PG - Children" et "G - All Ages" (1510)
Arpg = subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
print(nrow(Arpg))

# Animer différent dans rating de : "PG - Children" et "G - All Ages" (8490)
Ardiffpg = subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
print(nrow(Ardiffpg))

# Animer avec un score >= 9 et nombre de vote > 400k (496)
As9v400 = subset(dfAnime, Score >= 9 | Vote > 400000)
print(nrow(As9v400))

# Modif des dataframes en gardant que ces vars
dfAnime = dfAnime[ , c("Title","Score","Vote","Ranked")]
dfManga = dfManga[ , c("Title","Score","Vote","Ranked")]

# Ajout du colonne "Type" avec comme valeur Anime ou Manga
dfAnime$Type = "Anime"
dfManga$Type = "Manga"

# Concaténations des 2 dataframes dans dfConcat puis une visualisation
dfConcat = rbind(dfManga,dfAnime)
View(dfConcat)

# Export du dataframes dans un csv
write.table(x = dfConcat, file = "/Users/mbolmier/Documents/BUT/Semestre 2/prog/R/ExportTp1.csv", sep = ";",row.names = FALSE)
