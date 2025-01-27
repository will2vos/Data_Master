# positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

# importation mesures de biomasse
coll_biomass <-
  read.csv(
    "coll_biomass.csv",
    header = TRUE,
    stringsAsFactors = TRUE)

# importation données des sites
data60 <-
  read.csv("Data_60.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

colnames(coll_biomass)[1]<-"sites"

# joindre donnée de biomasse et information des sites
coll_biomass <-
  merge(data60, coll_biomass, by = "sites", all.x = TRUE)

coll_biomass<- coll_biomass[order(coll_biomass$sites),]

coll_biomass<- coll_biomass[,-1]
write.csv(coll_biomass,"coll_biomass.csv", row.names = F)

# visualisation de la biomasse des collemboles en fonction du traitements de coupe
coll_biomass$Coupe <- relevel(coll_biomass$Coupe, ref = "temoin")
boxplot(coll_biomass$weight ~ coll_biomass$Coupe)

# comparaison des biomasse en fonction de traitements de coupes
aov_coll <- aov(weight ~ Coupe + Bloc, data = coll_biomass)
summary(aov_coll)

par(mfcol = c(2, 2))
plot(aov_coll)

