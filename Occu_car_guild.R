# modèles d'occupation pour les guildes de carabes 

#Packages
#install.packages("unmarked")
library(unmarked)

#install.packages("AICcmodavg")
library(AICcmodavg)

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

### guild aquatique

#importation dataframe de detection

det_aqua<-read.csv("detect_aquatique.csv", header = TRUE, stringsAsFactors = TRUE)

# importantion des variables influencant l'occupation

sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importation des variable de détéction standardisé (construite dans le script NMixture.R)
visites.bin<- read.csv("visites_bin.csv", header = TRUE, stringsAsFactors = TRUE)
precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)
CWD.tot<- read.csv("cwd_total_std.csv",header = TRUE,stringsAsFactors = TRUE)




# Unmarked - occu

aqua.data <- unmarkedFrameOccu(y = det_aqua[,-1], siteCovs = sites_cov[, -1],
                              obsCovs = list(visites.bin = visites.bin[,-1],
                                             precip.3D.bin = precip.3D.bin[,-1],
                                             CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

aqua.data@siteCovs$Cut<-relevel(aqua.data@siteCovs$Cut, ref = "temoin")
aqua.data@siteCovs$Analogue<-relevel(aqua.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(aqua.data)
detHist(aqua.data) #0.22 des sites avec une detection

# modèles candidats

m0.aqua <- occu(~ visites.bin ~ (1|Bloc), data = aqua.data)
m1.aqua <- occu(~ visites.bin ~ Cut + (1|Bloc), data = aqua.data)
m2.aqua <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = aqua.data)
m3.aqua <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = aqua.data)
m4.aqua <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = aqua.data)
m5.aqua <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = aqua.data)
m6.aqua <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = aqua.data)
m7.aqua <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = aqua.data)
m8.aqua <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = aqua.data)
m9.aqua <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = aqua.data)
m10.aqua <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = aqua.data)
m11.aqua <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = aqua.data)
m12.aqua <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = aqua.data)
m13.aqua <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = aqua.data)
m14.aqua <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = aqua.data)
m15.aqua <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = aqua.data)

# inspection des modèles candidats

summary(m0.aqua)
summary(m1.aqua) #SE élevé pour cut
summary(m2.aqua)
summary(m3.aqua)
summary(m4.aqua)
summary(m5.aqua)#SE élevé pour analogue
summary(m6.aqua) #SE élevé pour analogue
summary(m7.aqua)
summary(m8.aqua)
summary(m9.aqua)
summary(m10.aqua) # SE élevé pour analogue
summary(m11.aqua)# SE élevé pour analogue
summary(m12.aqua)
summary(m13.aqua)
summary(m14.aqua)
summary(m15.aqua)# SE élevé pour analogue

# ajustement

m0.2.aqua <- occu(~ visites.bin ~ 1, data = aqua.data)
gof.aqua.m0 <- mb.gof.test(m0.2.aqua, nsim = 5000, plot.hist = TRUE) 

# séléction de modèles

Can_car <- list(m2, m3, m4, m7, m8, m9, m12, m13, m14)
Names <- c("m2","m3","m4", "m7","m8","m9","m12","m13","m14")

aictab(cand.set = Can_car, modnames = Names, c.hat = 1)

# test d'ajustement

#m9.2 <- occu(~ precip.3D.bin ~ Canope130_STD, data = aqua.data)

#gof.aqua.m9 <- mb.gof.test(m9.2, nsim = 5000, plot.hist = TRUE) 
#save(gof.car.m9, file = "gof.car.m9.Rdata")
#load(file = "gof.car.m9.Rdata")



### guild forestiers

#importation dataframe de detection

det_for<-read.csv("detect_forestier.csv", header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

for.data <- unmarkedFrameOccu(y = det_for[,-1], siteCovs = sites_cov[, -1],
                               obsCovs = list(visites.bin = visites.bin[,-1],
                                              precip.3D.bin = precip.3D.bin[,-1],
                                              CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

for.data@siteCovs$Cut<-relevel(for.data@siteCovs$Cut, ref = "temoin")
for.data@siteCovs$Analogue<-relevel(for.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(for.data)
detHist(for.data)

# modèles candidats

m0.for <- occu(~ visites.bin ~ (1|Bloc), data = for.data)
m1.for <- occu(~ visites.bin ~ Cut + (1|Bloc), data = for.data)
m2.for <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = for.data)
m3.for <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = for.data)
m4.for <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = for.data)
m5.for <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = for.data)
m6.for <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = for.data)
m7.for <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = for.data)
m8.for <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = for.data)
m9.for <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = for.data)
m10.for <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = for.data)
m11.for <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = for.data)
m12.for <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = for.data)
m13.for <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = for.data)
m14.for <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = for.data)
m15.for <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = for.data)

# inspection des modèles candidats

summary(m1.for)
summary(m2.for)
summary(m3.for)
summary(m4.for)
summary(m5.for)
summary(m6.for)
summary(m7.for)
summary(m8.for)
summary(m9.for)
summary(m10.for) 
summary(m11.for)# SE élevés
summary(m12.for) 
summary(m13.for)
summary(m14.for)
summary(m15.for)

# ajustement
m0.2.for <- occu(~ visites.bin ~ 1, data = for.data)

gof.for.m0<- mb.gof.test(m0.2.for, nsim = 5000, plot.hist = TRUE) 

gof.for.m0$c.hat.est

# séléction de modèles

Can_car <- list( m1.for, m2.for, m3.for, m4.for, m5.for, m6.for, m7.for, m8.for, m9.for, m10.for, m11.for, m12.for, m13.for, m14.for, m15.for)
Names <- c("m1.for","m2.for","m3.for","m4.for","m5.for","m6.for", "m7.for","m8.for","m9.for","m10.for","m11.for","m12.for","m13.for","m14.for","m15.for")

aictab(cand.set = Can_car, modnames = Names, c.hat = 1)

# selection de modèle avec AICcmodavg

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1, parm = "Canope130_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1, parm = "precip.3D.bin",
             parm.type = "detect")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1, parm = "Cut",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1, parm = "CWD_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1, parm = "Litter_STD",
             parm.type = "psi")


# même résultats que lorsque tout les carabes sont ensembles



### guild ouvert

#importation dataframe de detection

det_open<-read.csv("detect_ouvert.csv", header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

open.data <- unmarkedFrameOccu(y = det_open[,-1], siteCovs = sites_cov[, -1],
                              obsCovs = list(visites.bin = visites.bin[,-1],
                                             precip.3D.bin = precip.3D.bin[,-1],
                                             CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

open.data@siteCovs$Cut<-relevel(open.data@siteCovs$Cut, ref = "temoin")
open.data@siteCovs$Analogue<-relevel(open.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(open.data)
detHist(open.data)

# modèles candidats

m0.open <- occu(~ visites.bin ~ (1|Bloc), data = open.data)
m1.open <- occu(~ visites.bin ~ Cut + (1|Bloc), data = open.data)
m2.open <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = open.data)
m3.open <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = open.data)
m4.open <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = open.data)
m5.open <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = open.data)
m6.open <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = open.data)
m7.open <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = open.data)
m8.open <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = open.data)
m9.open <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = open.data)
m10.open <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = open.data)
m11.open <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = open.data)
m12.open <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = open.data)
m13.open <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = open.data)
m14.open <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = open.data)
m15.open <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = open.data)

# inspection des modèles candidats

summary(m0.open)
summary(m1.open)
summary(m2.open)
summary(m3.open)
summary(m4.open)
summary(m5.open)
summary(m6.open)
summary(m7.open)
summary(m8.open)
summary(m9.open)
summary(m10.open) 
summary(m11.open)
summary(m12.open) 
summary(m13.open)
summary(m14.open)
summary(m15.open)

m0.2.open <- occu(~ visites.bin ~ 1, data = open.data)

gof.open.m0<- mb.gof.test(m0.2.open, nsim = 5000, plot.hist = TRUE) 

gof.open.m0$c.hat.est