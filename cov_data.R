#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data.Git")

#importation jeu de données CWD
CWD<-read.csv("cov_cwd_mesures.csv",header = TRUE, sep=",",dec= ".", stringsAsFactors = TRUE)
CWD$Classe<-as.factor(CWD$Classe)
CWD$Analogue<-as.factor(CWD$Analogue)
str(CWD)

CWD$Coupe <- factor(CWD$Coupe, levels = c("temoin", "partielle", "totale"))

# créer colonne avec volume CWD en cm3 et m3
CWD$Vol_CM3 <- (CWD$Longueur/12)*(5*CWD$Diam_bas+5*CWD$Diam_api+2*sqrt(CWD$Diam_bas*CWD$Diam_api))
CWD$Vol_M3 <- CWD$Vol_CM3 /1000000

#ajout de colonne pour m3/ha

CWD$Vol_m3_ha <- NA
CWD$Vol_m3_ha[CWD$Coupe == "temoin"] <- CWD$Vol_M3[CWD$Coupe == "temoin"] / 0.48
CWD$Vol_m3_ha[CWD$Coupe == "partielle"] <- CWD$Vol_M3[CWD$Coupe == "partielle"] / 0.96
CWD$Vol_m3_ha[CWD$Coupe == "totale"] <- CWD$Vol_M3[CWD$Coupe == "totale"] / 0.96

agg_CWD_m3_ha <- with(tapply(Vol_m3_ha, INDEX = Coupe, FUN = sum),
                    data = CWD) 
agg_CWD_m3_ha

sd_CWD_m3_ha <- with(tapply(Vol_m3_ha, INDEX = Coupe, FUN = sd),
                      data = CWD) 
sd_CWD_m3_ha

# graphique

par(mfrow = c(1, 1), mar = c(5, 4.5, 4, 2) + 0.1)

boxplot(CWD$Vol_m3_ha ~ CWD$Coupe,
        xlab = NA,
        ylab = expression("CWD (m"^{3} * "/ha)"),
        xaxt = "n",
        cex.axis = 1.2,
        cex.lab = 1.2,
        cex.main = 1.5
        )

overstory = c("Control", "Partial-cut", "Clear-cut")

axis(
  1,
  at = 1:3,
  labels = overstory,
  lwd = 1,
  padj = 0.5
)

mtext("A", side=3, line=1, cex=2, adj=0.1, col="black") 

# CWD selon les coupes
agg_CWD_cm3 <- with(tapply(Vol_CM3, INDEX = Coupe, FUN = sum),
                 data = CWD) 
agg_CWD_cm3

agg_CWD_m3 <- agg_CWD_cm3 /1000000
agg_CWD_m3

# 12 placettes pour les témoins, 24 placettes pour coupe partielle, 24 placettes pour les coupes totale. 
# chaque placette mesure 400 m2 soit 0,4 ha
# Coupe partielle et coupe totale = 0,96 ha
# témoins = 0,48 ha

CWD_m3_ha = agg_CWD_m3 / 0.96
CWD_m3_ha[2] = agg_CWD_m3[2] / 0.48
CWD_m3_ha

# mesure des SD en m3/ha

# temoin
CWD_control <- CWD$Vol_CM3[CWD$Coupe == "temoin"]
CWD_control_ha <- CWD_control / 0.48
sd(CWD_control_ha)
CWD_control_ha_m3 <-CWD_control_ha/1000000
sd(CWD_control_ha_m3)

# partielle
CWD_partielle <- CWD$Vol_CM3[CWD$Coupe == "partielle"]
CWD_partielle_ha <- CWD_partielle / 0.96
sd(CWD_partielle_ha)
CWD_partielle_ha_m3 <-CWD_partielle_ha/1000000
sd(CWD_partielle_ha_m3)

# totale
CWD_totale <- CWD$Vol_CM3[CWD$Coupe == "totale"]
CWD_totale_ha <- CWD_totale / 0.96
sd(CWD_totale_ha)
CWD_totale_ha_m3 <-CWD_totale_ha/1000000
sd(CWD_totale_ha_m3)

# ajout 
# Sum CWD en cm3 dans Jeu de donnée 60 sites
Data60<-read.csv("Data_60.csv",header = TRUE, sep=",", stringsAsFactors = TRUE)

Data60$CWD_tot <- with(tapply(Vol_CM3, INDEX = ID_site, FUN = sum),
                  data = CWD)

Data60$CWD_tot_m3 <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                       data = CWD)

#création de dataframe séparant les classe de débris ligneux

classe1<-CWD[CWD$Classe=="1",]
Data60$CWD_1 <- with(tapply(Vol_CM3, INDEX = ID_site, FUN = sum),
                       data = classe1)

classe2<-CWD[CWD$Classe=="2",]
Data60$CWD_2 <- with(tapply(Vol_CM3, INDEX = ID_site, FUN = sum),
                     data = classe2)

classe3<-CWD[CWD$Classe=="3",]
Data60$CWD_3 <- with(tapply(Vol_CM3, INDEX = ID_site, FUN = sum),
                     data = classe3)

classe4<-CWD[CWD$Classe=="4",]
Data60$CWD_4 <- with(tapply(Vol_CM3, INDEX = ID_site, FUN = sum),
                     data = classe4)

# remplacer le NA par un valeur de 0

Data60[is.na(Data60)] <- 0

# vérification de la corrélation entre les débris ligneux

plot(~ CWD_1 + CWD_2 + CWD_3 + CWD_4, data=Data60)

# regroupement des quatres classes de débris ligneux en deux catégories (frais/avancé)

Data60$Frais <- Data60$CWD_1 + Data60$CWD_2
Data60$Avance <- Data60$CWD_3 + Data60$CWD_4

# vérification de la corrélation entre les classes frais et avancé de CWD

plot(~ Frais + Avance, data=Data60)

# après vérification, on voit que les classes de décompositions de débris ligneux sont corrélées
# entre elles. Même après avoir regroupés les quatres classes en deux catégorie ( frais/avancé)
# elle sont toujours fortement corrélées.
# les carabes et les salamandre pouvant repondre mieux à une catégoriue de décomposition
# il faudrait voir si on pourrait inclure une catégorie dans les modèles a la place du volume
# total de débris ligneux


#importation data litter
litt<-read.csv("cov_litter_mesures.csv",header = TRUE, sep=",",stringsAsFactors = TRUE, dec = ".")
str(litt)

litt_mean <- with(tapply(Moyenne, INDEX = Coupe, FUN = mean),
                  data = litt)
litt_mean

Data60$litter.depth<-litt$Moyenne



#importation data canop1.3
Canop_130<-read.csv("cov_canop130_mesures.csv",header = TRUE, sep=",",stringsAsFactors = TRUE, dec = ",")
Data60$Canop_130<-Canop_130$Ouverture

canop_mean <- with(tapply(Ouverture, INDEX = Coupe, FUN = mean),
                  data = Canop_130)
canop_mean

#importation data canop0.5
Canop_050<-read.csv("cov_canop_050.csv",header = TRUE, sep=";",stringsAsFactors = TRUE, dec = ",")
Data60$Can_050<-Canop_050$Ouverture

# vérification de la similitude des données entre can_1.3 et can_0.5

label=c("Canop_130","Canop_050")
boxplot( Data60$Canop_130, Data60$Canop_050,ylim = c(0, 100), main= "Comparaison des données d'ouverture de la canopée
         mesurées à hauteur de poitrine et au sol.", xlab = "hauteur de mesure (cm)",
         ylab= " pourcentage d'ouverture", names = label )



plot(Data60$Canop_050, Data60$Canop_130)
plot(~ Data60$Can_0.5 + Data60$Can_1.3)
cor(Data60$Can_0.5, Data60$Can_1.3)

# les mesures d'ouverture de la canopé ne sont pas significativement différente quand on compare
# les boxplots des deux catégorie.
# d'après l'analyse graphique et avec la fonction cor on peut voir que les deux prises de mesures
# sont fortement corélées.

Obs_cov<- Data60[,c("sites","Bloc","Coupe","Exclos","Analogue","CWD_tot","CWD_tot_m3","litter.depth",
                    "Canop_130")]


CWD_mat<-as.matrix(Obs_cov$CWD_tot)
CWD_mean<- mean(CWD_mat, na.rm = TRUE)
CWD_sd<- sd(CWD_mat, na.rm = TRUE)
CWD_std<- as.data.frame((CWD_mat - CWD_mean)/CWD_sd)
Obs_cov$CWD_STD<- CWD_std$V1

Litter<-as.matrix(Obs_cov$litter.depth)
Lit_mean <- mean(Litter, na.rm = TRUE)
Lit_sd <- sd(Litter, na.rm = TRUE)
litter_std <- as.data.frame((Litter - Lit_mean)/Lit_sd)
Obs_cov$litter_STD<- litter_std$V1

Canop_130<-as.matrix(Obs_cov$Canop_130)
Can130_mean <- mean(Canop_130, na.rm = TRUE)
Can130_sd <- sd(Canop_130, na.rm = TRUE)
canop130_std <- as.data.frame((Canop_130 - Can130_mean)/Can130_sd)
Obs_cov$Canope_STD<- canop130_std$V1

#write.csv(Obs_cov, "ObsCov.csv", row.names = FALSE)

CWD.tot<- data.frame(ID = c(Obs_cov$sites),
                     V1 = c(Obs_cov$CWD_tot_m3),
                     V2 = c(Obs_cov$CWD_tot_m3),
                     V3 = c(Obs_cov$CWD_tot_m3),
                     V4 = c(Obs_cov$CWD_tot_m3))

#write.csv(CWD.tot, "CWD.tot.csv", row.names = FALSE)

#### calcule CWD par site en M3 ################################################

CWD_m3<-read.csv("Data_60.csv",header = TRUE, sep=";", stringsAsFactors = TRUE)

CWD_m3$CWD_tot <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                       data = CWD)

#création de dataframe séparant les classe de débris ligneux

classe1<-CWD[CWD$Classe=="1",]
CWD_m3$CWD_1 <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                     data = classe1)

classe2<-CWD[CWD$Classe=="2",]
CWD_m3$CWD_2 <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                     data = classe2)

classe3<-CWD[CWD$Classe=="3",]
CWD_m3$CWD_3 <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                     data = classe3)

classe4<-CWD[CWD$Classe=="4",]
CWD_m3$CWD_4 <- with(tapply(Vol_M3, INDEX = ID_site, FUN = sum),
                     data = classe4)

write.csv(CWD_m3,"CWD_m3.csv",row.names = FALSE)
#mat_CWD<-read.csv("CWD_m3.csv", header = TRUE, stringsAsFactors = TRUE)


plot(~ CWD_tot + litter.depth + Can_1.3 , data = Data60)

cor(Data60$CWD_tot, Data60$litter.depth, method = "pearson")
cor(Data60$CWD_tot, Data60$Can_1.3, method = "pearson")
cor(Data60$litter.depth, Data60$Can_1.3, method = "pearson")

##########
##########
##########

##### box plot des deux type de coupes pour chaque covariables dans chaque bloc

layout(matrix(1:1, nrow=1, ncol=1))

BlocA<-subset(Data60, Data60$Bloc == "A")

boxplot(BlocA$litter.depth ~ BlocA$Coupe, ylab = "Profondeur de litière (cm)",
        xlab = "Blocs", main = "Profondeur de litière en fonctin du traitement de coupe forestières,
              mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

##### boxplot des covariables de site pour les qutres blocs

layout(matrix(1:4, nrow=2, ncol=2))

boxplot(CWD_m3$CWD_tot ~ CWD_m3$Bloc, ylab = "Volume de débris ligneux (m³)",
        xlab = "Blocs", main = "Volume de débris ligneux pour chaque bloc, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

boxplot(Data60$litter.depth ~ Data60$Bloc, ylab = "Profondeur de litière (cm)",
        xlab = "Blocs", main = "Profondeur de litière (cm) pour chaque bloc, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

boxplot(Data60$Can_1.3 ~ Data60$Bloc, ylab = "Ouverture de la canopé (%)",
        xlab = "Blocs", main = "Ouverture de la canopé (%) pour chaque bloc, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)


##### box plot des 
layout(matrix(1:4, nrow=2, ncol=2))

CWD_A<-subset(CWD_m3, CWD_m3$Bloc == "A" )
boxplot(CWD_A$CWD_tot ~ CWD_A$Coupe, ylab = "Volume de débris ligneux (m³)",
        xlab = "Coupes", main = "Volume de débris ligneux du bloc A, 
        en fonction du traitement de coupe, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

CWD_B<- subset(CWD_m3, CWD_m3$Bloc =="B")
boxplot(CWD_B$CWD_tot ~ CWD_B$Coupe, ylab = "Volume de débris ligneux (m³)",
        xlab = "Coupes", main = "Volume de débris ligneux du bloc B, 
        en fonction du traitement de coupe, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

CWD_C<-subset(CWD_m3, CWD_m3$Bloc == "C" )
boxplot(CWD_C$CWD_tot ~ CWD_C$Coupe, ylab = "Volume de débris ligneux (m³)",
        xlab = "Coupes", main = "Volume de débris ligneux du bloc C, 
        en fonction du traitement de coupe, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)

CWD_D<- subset(CWD_m3, CWD_m3$Bloc =="D")
boxplot(CWD_D$CWD_tot ~ CWD_D$Coupe, ylab = "Volume de débris ligneux (m³)",
        xlab = "Coupes", main = "Volume de débris ligneux du bloc D, 
        en fonction du traitement de coupe, mai à août 2022",
        cex.axis = 1.5, cex.lab = 1.5)
