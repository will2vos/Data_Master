#Packages
install.packages("unmarked")
library(unmarked)

install.packages("AICcmodavg")
library(AICcmodavg)

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

###################### PREPARATION DES DONNÉES #################################

# importation du data frame d'abondance de pcin

abun_pcin<- read.csv("Pcin_abundance.csv", header = TRUE, stringsAsFactors = TRUE)

#changement de nom de ligne + retenu des colonnes d'abondance uniquement

row.names(abun_pcin)<-abun_pcin[,1]
abun_pcin<-abun_pcin[,c("V1","V2","V3","V4")]

#transformation abundance en détéction

# det_pcin<-as.data.frame(ifelse(abun_pcin >= 1,1,0))
# 
# det_pcin$site<-row.names(det_pcin)
# det_pcin<-det_pcin[,c("site","V1","V2","V3","V4")]
# 
# write.csv(det_pcin,"det_pcin.csv", row.names = FALSE)

det_pcin<-read.csv("det_pcin.csv", header = TRUE, stringsAsFactors = TRUE)

# importantion des variables influencant l'occupation

sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importation des variable de détéction standardisé (construite dans le script NMixture.R)
airtemp.std<- read.csv("airtemp_std.csv", header = TRUE, stringsAsFactors = TRUE)
rh.std<- read.csv("rh_std.csv", header = TRUE, stringsAsFactors = TRUE)
jday.std<- read.csv("jday_std.csv", header = TRUE, stringsAsFactors = TRUE)
precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)



#changement du nom de ligne et retrait de la colonne X

row.names(airtemp.std)<-airtemp.std[,1]
airtemp.std<-airtemp.std[,c("X1","X2","X3","X4")]

row.names(rh.std)<-rh.std[,1]
rh.std<-rh.std[,c("X1","X2","X3","X4")]

row.names(jday.std)<-jday.std[,1]
jday.std<-jday.std[,c("X1","X2","X3","X4")]

row.names(precip.3D.bin)<-precip.3D.bin[,1]
precip.3D.bin<-precip.3D.bin[,c("X1","X2","X3","X4")]


# création d'une variable visite

visites <- as.data.frame(matrix(nrow = 60, ncol = 5))
visites$V1 <- sites_cov$ID_site
visites$V2 <- factor(1, levels = c("1", "2", "3", "4"))
visites$V3 <- factor(2, levels = c("1", "2", "3", "4"))
visites$V4 <- factor(3, levels = c("1", "2", "3", "4"))
visites$V5 <- factor(4, levels = c("1", "2", "3", "4"))

colnames(visites)[]<-c("ID","X1","X2","X3","X4")

row.names(visites)<-visites[,c("ID")]
visites<-visites[,c("X1","X2","X3","X4")]

# write.csv( x = visites,file =  "visites.csv",row.names = TRUE)

# transformation des visite en binaire a 2 niveaux

visites.bin<- as.data.frame(matrix(nrow = 60, ncol = 5))
visites.bin$V1 <- sites_cov$ID_site
visites.bin$V2 <- 0
visites.bin$V3 <- 0
visites.bin$V4 <- 1
visites.bin$V5 <- 1
colnames(visites.bin)[]<-c("ID","X1","X2","X3","X4")
row.names(visites.bin)<-visites.bin[,c("ID")]
visites.bin<-visites.bin[,c("X1","X2","X3","X4")]

#write.csv(visites.bin,"visites_bin.csv")

# création d'une matrice du volume de débris ligneux pour chaque site à chaque visite
# afin d'inclure la variable débris ligneux dans la détéction

CWD.tot <- as.data.frame(matrix(nrow = 60, ncol = 5))
CWD.tot$V1 <-sites_cov$ID_site
CWD.tot$V2 <-sites_cov$CWD_STD
CWD.tot$V3 <-sites_cov$CWD_STD
CWD.tot$V4 <-sites_cov$CWD_STD
CWD.tot$V5 <-sites_cov$CWD_STD

row.names(CWD.tot)<-CWD.tot$V1
CWD.tot <-CWD.tot[,c("V2","V3","V4","V5")]
colnames(CWD.tot)[]<-c("X1","X2","X3","X4")

# Séléction du débris ligneux avancé seulement au lieu du volume total de débris ligneux

SiteCov.brute<- read.csv("ObsCov.csv", header = TRUE, stringsAsFactors = TRUE)

CWD_old_STD <- as.data.frame(matrix(nrow = 60, ncol = 5))
CWD_old_STD$V1 <- SiteCov.brute$ID
CWD_old_mean <- mean(SiteCov.brute$Avance,na.rm = TRUE)
CWD_old_sd <- sd(SiteCov.brute$Avance, na.rm = TRUE)
CWD_old_std <- (SiteCov.brute$Avance - CWD_old_mean)/CWD_old_sd

CWD_old_STD$V2 <- CWD_old_std
CWD_old_STD$V3 <- CWD_old_std
CWD_old_STD$V4 <- CWD_old_std
CWD_old_STD$V5 <- CWD_old_std

colnames(CWD_old_STD)[]<-c("ID","X1","X2","X3","X4")
row.names(CWD_old_STD)<-CWD_old_STD$ID
CWD_old_STD<-CWD_old_STD[,c("X1","X2","X3","X4")]

sites_cov$CDW.old.std <- CWD_old_STD$X1

# création du data.frame de précipitation standardisé (si binaire ne fonctionne pas)

precip.3D<- read.csv("pcin_precip3D.csv",header = TRUE,stringsAsFactors = TRUE)

rownames(precip.3D)<-precip.3D$Jday
precip.3D<-precip.3D[,c("X1","X2","X3","X4")]

# ajout d'une colonne exclos en binaire dans les variables de site

sites_cov$Excl.bin<-ifelse(sites_cov$Exclos=="avec",1,0)


######################## ANALYSES ##############################################

# Unmarked - occu

pcin.data <- unmarkedFrameOccu(y = det_pcin, siteCovs = sites_cov[, -1],
                              obsCovs = list(visites.bin = visites.bin,
                                             precip.3D.bin = precip.3D.bin,
                                             CWD.tot = CWD.tot))



#pcin.data <- unmarkedFrameOccu(y = det_pcin, siteCovs = sites_cov[, -1],
#                              obsCovs = list(jday.std = jday.std,
#                                             precip.3D.bin = precip.3D.bin,
#                                             visites = visites,
#                                             CWD.tot = CWD.tot))

# pcin.data <- unmarkedFrameOccu(y = det_pcin, siteCovs = sites_cov[, -1],
#                                   obsCovs = list(visites = visites,
#                                                 CWD.tot = CWD.tot,
#                                                precip.3D.std = precip.3D.std))
#

# Changement de référence dans les comparaisons de traitements

pcin.data@siteCovs$Cut<-relevel(pcin.data@siteCovs$Cut, ref = "temoin")

# inspection de pcin.data

summary(pcin.data)
detHist(pcin.data)

# modèles candidats

m0 <- occu(~ visites.bin ~ (1|Bloc), data = pcin.data)
m1 <- occu(~ visites.bin ~ Cut + (1|Bloc), data = pcin.data)
m2 <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = pcin.data)
m3 <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = pcin.data)
m4 <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = pcin.data)
m5 <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = pcin.data)
m6 <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = pcin.data)
m7 <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = pcin.data)
m8 <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = pcin.data)
m9 <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = pcin.data)
m10 <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = pcin.data)
m11 <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = pcin.data)
m12 <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = pcin.data)
m13 <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = pcin.data)
m14 <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = pcin.data)
m15 <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = pcin.data)


# inspection des modèles candidats

summary(m1) # SE élevé
summary(m2)# SE élevé
summary(m3)# SE élevé
summary(m4)# SE élevé
summary(m5)# SE élevé
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10) #Nan
summary(m11)
summary(m12)
summary(m13)
summary(m14)
summary(m15)

# test d'ajustement du modèles null

#gof.pcin.m0 <- mb.gof.test(m0, nsim = 5000, plot.hist = TRUE) 
#save(gof.pcin.m0, file = "gof.pcin.m0.Rdata")

#gof.pcin.m0$chisq.table
#gof.pcin.m0$c.hat.est

# séléction de modèles

#Can_pcin <- list(m1, m2, m3, m4, m5, m6,m7, m8, m9, m10, m11,m12, m13, m14, m15)
#Names <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7","m8","m9","m10","m11","m12","m13","m14","m15")

Can_pcin <- list( m1, m2, m3, m4, m5, m6, m7, m8, m9, m11, m12, m13, m14, m15)
Names <- c("m1","m2","m3","m4","m5","m6", "m7","m8","m9","m11","m12","m13","m14","m15")

aictab(cand.set = Can_pcin, modnames = Names, c.hat = 1)

summary(m4)

# test d'ajustement de m9 sans effet aleatoire du bloc

m4.2 <- occu(~ visites.bin ~ Canope130_STD, data = pcin.data)
gof.pcin.m4.2 <- mb.gof.test(m4.2, nsim = 5000, plot.hist = TRUE) 
save(gof.pcin.m4.2, file = "gof.pcin.m4.Rdata")
#gof.pcin.m4 (nom du fichier)

gof.pcin.m4.2$chisq.table
gof.pcin.m4.2$c.hat.est

#c.hat de 2.885693


# extraction de l'estimé de paramètres
# psi
psi.beta0.pcin <- coef(m4)["psi(Int)"]
psi.beta.Canope130STD <- coef(m4)["psi(Canope130_STD)"] 
Canope130STD <- sites_cov$Canope130_STD

# p
p.beta0.pcin <- coef(m4)["p(Int)"]
p.beta.visites.bin <- coef(m4)["p(visites.bin)"]
visites.bin

# nombre de visites et de sites
nsites <- nrow(det_pcin)
nvisits <- ncol(det_pcin)

#calcule du prédicteur linéaire
# psi - vector
psi.logit <- psi.beta0.pcin + psi.beta.Canope130STD * Canope130STD 
psi.logit[1:8]

# p - matrix
p.logit <- p.beta0.pcin + p.beta.visites.bin * visites.bin
head(p.logit)

#simuler occupation à chaque site
# psi
psiSim <- rep(NA, nsites) 

for(i in 1:nsites) {
  psiSim[i] <- rbinom(n = 1, size = 1,
                      prob = plogis(psi.logit[i]))
}
psiSim

#simuler détection a chaque site
# p
pSimM4 <- matrix(NA, nrow = nsites, ncol = nvisits)

for(i in 1:nsites) { 
  for(j in 1:nvisits) {
    pSimM4[i,j] <- rbinom(n = 1, size = 1,
                          prob=plogis(p.logit[i,j])*psiSim[i])
} }
head(pSimM4)


# estimations des paramètre pour modele m9

umfSim <- unmarkedFrameOccu(y = pSimM4, siteCovs = sites_cov[, -1],
                               obsCovs = list(visites.bin = visites.bin,
                                              precip.3D.bin = precip.3D.bin,
                                              CWD.tot = CWD.tot))

m4Sim <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = umfSim)

m4Mat <- matrix(NA, nrow = 4,
                ncol = 2)
m4Mat[, 1] <- coef(m4)
m4Mat[, 2] <- coef(m4Sim)
colnames(m4Mat) <- c("orig", "sim")
rownames(m4Mat) <- names(coef(m4))

round(m4Mat, 4)

# inférence multi-modèles avec AICcmodavg

modavgShrink(cand.set = Can_pcin, modnames = Names, c.hat = 1, parm = "Canope130_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_pcin, modnames = Names, c.hat = 1, parm = "visites.bin",
             parm.type = "detect")

# inferance multimodèle - prédiction

# précipitation

#mat_precip_3D<- read.csv("pcin_precip3D.csv",header = TRUE,stringsAsFactors = TRUE)
#range(mat_precip_3D[,-1])

range(sites_cov$Canope130_STD)

Canop.130<- read.csv("Matrice_canope130.csv",header = TRUE,stringsAsFactors = TRUE)

new.dats <- data.frame(Canope130= 
                         seq(from = min(Canop.130[,c("V2")], na.rm=TRUE),
                             to = max(Canop.130[,c("V2")], na.rm= TRUE), 
                             length.out = 100))

new.dats$CWD_STD <- 0
new.dats$Litter_STD <- 0
new.dats$Bloc <- factor(x = "A", levels = c( "A","B","C","D"))
new.dats$Cut <- factor(x = "CP", levels = c( "CP","CT"))
new.dats$Analogue <- factor(x = "1", levels = c( "1","2","3","temoin"))

canop.130.mean <- mean(as.vector(as.matrix(Canop.130[,c("V2")], na.rm = TRUE)))
canop.130.sd <- sd(as.vector(as.matrix(Canop.130[,c("V2")], na.rm = TRUE)))
new.dats$Canope130_STD <- (new.dats$Canope130 - canop.130.mean)/canop.130.sd

head(new.dats)
str(new.dats)


#new.dats <- data.frame(precip.3D = 
#                         seq(from = min(mat_precip_3D[,-1], na.rm=TRUE),
 #                            to = max(mat_precip_3D[,-1], na.rm= TRUE), 
  #                           length.out = 60))

#new.dats$jday.sim <- sample(x= c(19,47,74,108), size=60, replace=TRUE)

# new.dats <- data.frame(precip.3D = 
#                         seq(from = min(mat_precip_3D[,-1], na.rm=TRUE),
#                             to = max(mat_precip_3D[,-1], na.rm= TRUE), 
#                             length.out = 90))

#new.dats$JD <- seq(from = 19,
#                   to = 108, by = 1)


# standardisation des jour julien

#jday<- read.csv("pcin_jday.csv", header = TRUE, stringsAsFactors = TRUE)

#jday.mean <- mean(as.vector(as.matrix(jday[,-1])))
#jday.sd <- sd(as.vector(as.matrix(jday[,-1])))
#new.dats$jday.std <- (new.dats$JD - jday.mean)/jday.sd

#standardisation des précipitation

#precip.mean <- mean(as.vector(as.matrix(mat_precip_3D[,-1], na.rm = TRUE)))
#precip.sd <- sd(as.vector(as.matrix(mat_precip_3D[,-1], na.rm = TRUE)))
#new.dats$precip.3D.std <- (new.dats$precip.3D - precip.mean)/precip.sd

# prédiction

preds <- modavgPred(cand.set = Can_pcin, modnames = Names, c.hat = 1, newdata = new.dats,
                    type = "response", parm.type = "psi")
head(preds)

# ajouts des valeurs prédites et de SE incondiditionnels
new.dats$mod.avg.pred <- preds$mod.avg.pred
new.dats$uncond.se <- preds$uncond.se

# calcule des intervalle de confiances avec échelle logit

preds.logit <- modavgPred(cand.set = Can_pcin, modnames = Names,
                            c.hat = 1, newdata = new.dats,
                            type = "link", parm.type = "psi")

new.dats$mod.avg.pred.logit <- preds.logit$mod.avg.pred 
new.dats$uncond.se.logit <- preds.logit$uncond.se

# calcule des limites et conversion à l'echelle original

new.dats$low95 <- plogis(new.dats$mod.avg.pred.logit -
                             qnorm(0.975) * new.dats$uncond.se.logit) 
new.dats$upp95 <- plogis(new.dats$mod.avg.pred.logit + 
                           qnorm(0.975) * new.dats$uncond.se.logit)

# affichage graphique

plot(mod.avg.pred ~ Canope130, data = new.dats,
       ylab = "Probabilité de détection",
       xlab = expression(paste("Ouverture de la canopée (%)")),
       ylim = range(c(low95, upp95)), type = "l")

# ajout de l,intervalle de confiance
lines(low95 ~ Canope130, data = new.dats,
          lty = "dotted")
lines(upp95 ~ Canope130, data = new.dats,
        lty = "dotted")




################ schéma visite ~ probabilité de détéction ###########################


dat.visites <- data.frame(visites.bin = factor(c("0", 
                                       "1"),
                                     levels = c("0","1")))

dat.visites$precip.3D.bin<- 0
dat.visites$CWD.tot<- 0

preds.visites <- modavgPred(cand.set = Can_pcin, modnames = Names,
                        c.hat = 1, newdata = dat.visites,
                        type = "response", parm.type = "detect")

dat.visites$mod.avg.pred <- preds.visites$mod.avg.pred
dat.visites$low95 <- preds.visites$lower.CL
dat.visites$upp95 <- preds.visites$upper.CL

xvals <- c(0.25, 0.75)

par(mar = c(4.5, 5, 5, 2.1))

plot(mod.avg.pred ~ xvals,
     data = dat.visites,
     main = expression(atop("Probabilité de détection de la salamandre cendrée " *italic("P.cinereus"),", selon la période estivale.")),
     ylab = "Probabilité de détection",
     xlab = "Périodes",
     ylim = range(c(low95, upp95)),
     type = "p", cex = 1.5, 
     cex.lab = 1.7, cex.axis = 1.7,cex.main = 1.5,
     xaxt = "n", xlim = c(0, 1))

##add x axis
axis(side = 1, at = xvals, 
     labels = c("mai-juin", "juillet-août"), 
     cex.lab = 1.5,cex.axis = 1.7)

##add confidence bands
segments(x0 = xvals, 
         x1 = xvals,
         y0 = dat.visites$low95, 
         y1 = dat.visites$upp95,
         lwd = 3)
#size 750 x 550

# nom du fichier graphique
#p.det.pcin



























############################ variables visite en regroupant V1 et V2 ############################




# les précipitation en variable binaire ne peuvent pas etre mises en 3 catégories,je 
# reutilise donc les valeurs standardisé des précipitation.



#vérification des fréquence de détection pour chaque analogues

Data<- read.csv("Data.csv",header = TRUE,stringsAsFactors = TRUE,sep = ";")

Analogue.pcin<- as.data.frame(aggregate(x = Data$P.cin, by = list(Data$analogue),FUN = sum))
colnames(Analogue.pcin)<- c("Analogue","Decompte")

sites.pcin<- as.data.frame(aggregate(x = Data$P.cin, by = list(Data$ID_site, Data$analogue),FUN = sum))
colnames(sites.pcin)<- c("ID","Analogue","Decompte")

# Unmarked - occu

#CWD_old_STD

CWD.tot<-CWD.tot[,c("X2","X3","X4")]

pcin.data.3lv <- unmarkedFrameOccu(y = det_pcin_3lv, siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.3lv = visites.3lv,
                                                  CWD.tot = CWD.tot,
                                                  precip.3D.std = precip.3D.std))

#pcin.data.3lv <- unmarkedFrameOccu(y = det_pcin_3lv, siteCovs = sites_cov[, -1],
 #                              obsCovs = list(precip.3D.bin = precip.3D.bin,
 #                                             visites.3lv = visites.3lv,
 #                                             CWD_old_STD = CWD_old_STD))

# Changement de référence dans les comparaisons de traitements

pcin.data.3lv@siteCovs$Cut<-relevel(pcin.data.3lv@siteCovs$Cut, ref = "temoin")

# inspection de pcin.data

summary(pcin.data.3lv)
detHist(pcin.data.3lv)

# modèles candidats

m0 <- occu(~ visites.3lv ~ (1|Bloc), data = pcin.data.3lv)
m1 <- occu(~ visites.3lv ~ Cut + (1|Bloc), data = pcin.data.3lv)
m2 <- occu(~ visites.3lv ~ CWD_STD + (1|Bloc), data = pcin.data.3lv)
m3 <- occu(~ visites.3lv ~ Litter_STD + (1|Bloc), data = pcin.data.3lv)
m4 <- occu(~ visites.3lv ~ Canope130_STD + (1|Bloc), data = pcin.data.3lv)
m5 <- occu(~ visites.3lv ~ Analogue + (1|Bloc), data = pcin.data.3lv)
m6 <- occu(~ precip.3D.std ~ Cut + (1|Bloc), data = pcin.data.3lv)
m7 <- occu(~ precip.3D.std ~ CWD_STD + (1|Bloc), data = pcin.data.3lv)
m8 <- occu(~ precip.3D.std ~ Litter_STD + (1|Bloc), data = pcin.data.3lv)
m9 <- occu(~ precip.3D.std ~ Canope130_STD + (1|Bloc), data = pcin.data.3lv)
m10 <- occu(~ precip.3D.std ~ Analogue + (1|Bloc) , data = pcin.data.3lv)
m11 <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = pcin.data.3lv)
m12 <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = pcin.data.3lv)
m13 <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = pcin.data.3lv)
m14 <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = pcin.data.3lv)
m15 <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = pcin.data.3lv)


# inspection des modèles candidats

summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m10) #NaN
summary(m11)
summary(m12)
summary(m13)
summary(m14) #NaN
summary(m15) #NaN

Can_pcin <- list( m1, m2, m3, m4,m5, m6, m7, m8, m9, m11, m12, m13, m14, m15)
Names <- c("m1","m2","m3","m4","m5","m6", "m7","m8","m9","m11","m12","m13","m14","m15")

aictab(cand.set = Can_pcin, modnames = Names, c.hat = 1)

# test d'ajustement de m4 sans effet aleatoire du bloc

m4.2 <- occu(~ visites.3lv ~ Canope130_STD, data = pcin.data.3lv)
gof.pcin.m4.2 <- mb.gof.test(m4.2, nsim = 5000, plot.hist = TRUE) 
save(gof.pcin.m4.2, file = "gof.pcin.m4.Rdata")

gof.pcin.m14.2$chisq.table
gof.pcin.m4.2$c.hat.est
