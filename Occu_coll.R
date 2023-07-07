# occupations collemboles
#Packages
install.packages("unmarked")
library(unmarked)

install.packages("AICcmodavg")
library(AICcmodavg)
c


###### diagnostiques

abun_coll$Sum<- abun_coll$V1 + abun_coll$V2+ abun_coll$V3

nrow(abun_coll[abun_coll$Sum <= 4 & abun_coll$Sum > 0,])

site_inf4<-as.data.frame(abun_coll$site[abun_coll$Sum <= 4 & abun_coll$Sum >0])
colnames(site_inf4)[]<- "site"

id.coll<- read.csv("ID_COLL.csv", header = TRUE, stringsAsFactors = TRUE)

site_inf4<- merge(x=site_inf4, y=id.coll, by= "site", all.x = TRUE)

site_inf4<-site_inf4[,c("site","ord.fam")]


#########
row.names(abun_coll)<-abun_coll[,"site"]
abun_coll<-abun_coll[,c("V1","V2","V3","V4")]

det_coll<- as.data.frame(ifelse(abun_coll == 0,0,1))

# importation du data frame d'abondance de carabes

#det_coll<- read.csv("Coll_detection.csv", header = TRUE, stringsAsFactors = TRUE)

#changement de nom de ligne + retenu des colonnes d'abondance uniquement

#row.names(det_coll)<-det_coll[,1]
#det_coll<-det_coll[,c("V1","V2","V3","V4")]

# importantion des variables influencant l'occupation

sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importation des variable de détéction standardisé (construite dans le script NMixture.R)
airtemp<- read.csv("airtemp_std.csv", header = TRUE, stringsAsFactors = TRUE)
rh<- read.csv("rh_std.csv", header = TRUE, stringsAsFactors = TRUE)
jday.std<- read.csv("jday_std.csv", header = TRUE, stringsAsFactors = TRUE)
precip_3D_std<- read.csv("precip3D_std.csv",header = TRUE,stringsAsFactors = TRUE)
precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)

#changement du nom de ligne et retrait de la colonne X

row.names(airtemp)<-airtemp[,1]
airtemp<-airtemp[,c("X1","X2","X3","X4")]

row.names(rh)<-rh[,1]
rh<-rh[,c("X1","X2","X3","X4")]

row.names(jday.std)<-jday.std[,1]
jday.std<-jday.std[,c("X1","X2","X3","X4")]

row.names(precip_3D_std)<-precip_3D_std[,1]
precip_3D_std<-precip_3D_std[,c("X1","X2","X3","X4")]

row.names(precip.3D.bin)<-precip.3D.bin[,1]
precip.3D.bin<-precip.3D.bin[,c("X1","X2","X3","X4")]

# création d'une variable visite

visites <- as.data.frame(matrix(nrow = 60, ncol = 5))
visites$V1 <- sites_cov$ID_site
visites$V2 <- factor(1, levels = c("1", "2", "3", "4"))
visites$V3 <- factor(2, levels = c("1", "2", "3", "4"))
visites$V4 <- factor(3, levels = c("1", "2", "3", "4"))
visites$V5 <- factor(4, levels = c("1", "2", "3", "4"))

colnames(visites)[]<-c("X","X1","X2","X3","X4")

row.names(visites)<-visites[,1]
visites<-visites[,c("X1","X2","X3","X4")]

#création de la variable visite binaire version binaire


visites.bin<- as.data.frame(matrix(nrow = 60, ncol = 5))
visites.bin$V1 <- sites_cov$ID_site
visites.bin$V2 <- 0
visites.bin$V3 <- 0
visites.bin$V4 <- 1
visites.bin$V5 <- 1
colnames(visites.bin)[]<-c("ID","X1","X2","X3","X4")
row.names(visites.bin)<-visites.bin[,c("ID")]
visites.bin<-visites.bin[,c("X1","X2","X3","X4")]

# ajout d'une colonne exclos en binaire dans les variables de site

sites_cov$Excl.bin<-as.factor(ifelse(sites_cov$Exclos=="avec",1,0))

# création de la matrice CWD.tot pour le volume de débris ligneux total à mettre 
# comme variable de détection

CWD.tot <- as.data.frame(matrix(nrow = 60, ncol = 5))
CWD.tot$V1 <-sites_cov$ID_site
CWD.tot$V2 <-sites_cov$CWD_STD
CWD.tot$V3 <-sites_cov$CWD_STD
CWD.tot$V4 <-sites_cov$CWD_STD
CWD.tot$V5 <-sites_cov$CWD_STD

row.names(CWD.tot)<-CWD.tot$V1
CWD.tot <-CWD.tot[,c("V2","V3","V4","V5")]
colnames(CWD.tot)[]<-c("X1","X2","X3","X4")

# Unmarked - occu

coll.data <- unmarkedFrameOccu(y = det_coll, siteCovs = sites_cov[, -1],
                              obsCovs = list(visites.bin = visites.bin,
                                             precip.3D.bin = precip.3D.bin,
                                             CWD.tot = CWD.tot))

# Changement de référence dans les comparaisons de traitements

coll.data@siteCovs$Cut<-relevel(coll.data@siteCovs$Cut, ref = "temoin")

# inspection de pcin.data

summary(coll.data)
detHist(coll.data)

# modèles candidats

m0 <- occu(~ visites.bin ~ (1|Bloc), data = coll.data)
m1 <- occu(~ visites.bin ~ Cut + (1|Bloc), data = coll.data)
m2 <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = coll.data)
m3 <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = coll.data)
m4 <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = coll.data)
m5 <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = coll.data)
m6 <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = coll.data)
m7 <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = coll.data)
m8 <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = coll.data)
m9 <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = coll.data)
m10 <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = coll.data)
m11 <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = coll.data)
m12 <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = coll.data)
m13 <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = coll.data)
m14 <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = coll.data)
m15 <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = coll.data)

# inspection des modèles candidats

summary(m1) 
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6) # SE trop élevée
summary(m7)
summary(m8) # modele ne converge pas
summary(m9)
summary(m10)# SE trop élevé
summary(m11)
summary(m12) # SE trop élevé
summary(m13)
summary(m14) # production de NaN
summary(m15) # SE trop élevé

# séléction de modèles

Can_car <- list(m1, m2, m3, m4, m5, m6,m7, m8)
Names <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8")

aictab(cand.set = Can_car, modnames = Names, c.hat = 1.07)

# test d'ajustement

#m8.TdA <- occu(~ precip.3D.std ~ Canope130_STD, data = car.data)

#gof.car.m8 <- mb.gof.test(m8.TdA, nsim = 5000, plot.hist = TRUE) 
#save(gof.car.m8, file = "gof.car.m8.Rdata")

#gof.car.m8$chisq.table
#gof.car.m8$c.hat.est
#c.hat=1.07

# extraction de l'estimé de paramètres
coef(m8)

# psi
psi.beta0.car <- coef(m8)["psi(Int)"]
psi.beta.Canope130STD <- coef(m8)["psi(Canope130_STD)"] 
Canope130STD <- sites_cov$Canope130_STD

# p
p.beta0.car <- coef(m8)["p(Int)"]
p.beta.precip.3D.std <- coef(m8)["p(precip.3D.std)"]

precip.3D<- read.csv("pcin_precip3D.csv",header = TRUE,stringsAsFactors = TRUE)


# nombre de visites et de sites
nsites <- nrow(det_car)
nvisits <- ncol(det_car)

#calcule du prédicteur linéaire
# psi - vector
psi.logit <- psi.beta0.car + psi.beta.Canope130STD * Canope130STD
psi.logit[1:8]

# p - matrix
p.logit <- p.beta0.car + p.beta.precip.3D.std * precip.3D[,-1]
head(p.logit)

# selection de modèle avec AICcmodavg

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.07, parm = "Canope130_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.07, parm = "precip.3D.std",
             parm.type = "detect")

# inferance multimodèle - prédiction

# précipitation

range(precip.3D[,-1])

new.dats <- data.frame(precip.3D = 
                         seq(from = min(precip.3D[,-1]),
                             to = max(precip.3D[,-1]), 
                             length.out = 90))

new.dats$JD <- seq(from = 19,
                   to = 108, by = 1)

# standardisation des jour julien

jday<- read.csv("pcin_jday.csv", header = TRUE, stringsAsFactors = TRUE)

jday.mean <- mean(as.vector(as.matrix(jday[,-1])))
jday.sd <- sd(as.vector(as.matrix(jday[,-1])))
new.dats$jday.std <- (new.dats$JD - jday.mean)/jday.sd

#standardisation des précipitation

precip.mean <- mean(as.vector(as.matrix(precip.3D[,-1])))
precip.sd <- sd(as.vector(as.matrix(precip.3D[,-1])))
new.dats$precip.3D.std <- (new.dats$precip.3D - precip.mean)/precip.sd

# prédiction

preds <- modavgPred(cand.set = Can_car, modnames = Names, c.hat = 1, newdata = new.dats,
                    type = "response", parm.type = "detect")
preds

# ajouts des valeurs prédites et de SE incondiditionnels
new.dats$mod.avg.pred <- preds$mod.avg.pred
new.dats$uncond.se <- preds$uncond.se

# calcule des intervalle de confiances avec échelle logit

preds.logit <- modavgPred(cand.set = Can_car, modnames = Names,
                          c.hat = 1, newdata = new.dats,
                          type = "link", parm.type = "detect")

new.dats$mod.avg.pred.logit <- preds.logit$mod.avg.pred 
new.dats$uncond.se.logit <- preds.logit$uncond.se

# calcule des limites et conversion à l'echelle original

new.dats$low95 <- plogis(new.dats$mod.avg.pred.logit -
                           qnorm(0.975) * new.dats$uncond.se.logit) 
new.dats$upp95 <- plogis(new.dats$mod.avg.pred.logit + 
                           qnorm(0.975) * new.dats$uncond.se.logit)

# affichage graphique

plot(mod.avg.pred ~ precip.3D, data = new.dats,
     ylab = "Probabilité de détection",
     xlab = expression(paste("Précipitation (mm)")),
     ylim = range(c(low95, upp95)), type = "l")

# ajout de l,intervalle de confiance
lines(low95 ~ precip.3D, data = new.dats,
      lty = "dotted")
lines(upp95 ~ precip.3D, data = new.dats,
      lty = "dotted")




###################### Occu pour ORDRES des collemboles #################################

# Poduromorpha

det.ord.podu<-read.csv("det_podu_ord.csv",header = TRUE, stringsAsFactors = TRUE)

# importantion des variables influencant l'occupation

sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importation des variable de détéction standardisé (construite dans le script NMixture.R)
visites.bin<- read.csv("visites_bin.csv", header = TRUE, stringsAsFactors = TRUE)
precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)
CWD.tot<- read.csv("cwd_total_std.csv",header = TRUE,stringsAsFactors = TRUE)




# Unmarked - occu

ord.podu.data <- unmarkedFrameOccu(y = det.ord.podu[,-1], siteCovs = sites_cov[, -1],
                               obsCovs = list(visites.bin = visites.bin[,-1],
                                              precip.3D.bin = precip.3D.bin[,-1],
                                              CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

ord.podu.data@siteCovs$Cut<-relevel(ord.podu.data@siteCovs$Cut, ref = "temoin")
ord.podu.data@siteCovs$Analogue<-relevel(ord.podu.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(ord.podu.data)
detHist(ord.podu.data) #41/60, 0.68

# modèles candidats

m0.ord.podu <- occu(~ visites.bin ~ (1|Bloc), data = ord.podu.data)
m1.ord.podu <- occu(~ visites.bin ~ Cut + (1|Bloc), data = ord.podu.data)
m2.ord.podu <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = ord.podu.data)
m3.ord.podu <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = ord.podu.data)
m4.ord.podu <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = ord.podu.data)
m5.ord.podu <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = ord.podu.data)
m6.ord.podu <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = ord.podu.data)
m7.ord.podu <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = ord.podu.data)
m8.ord.podu <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = ord.podu.data)
m9.ord.podu <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = ord.podu.data)
m10.ord.podu <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = ord.podu.data)
m11.ord.podu <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = ord.podu.data)
m12.ord.podu <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = ord.podu.data)
m13.ord.podu <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = ord.podu.data)
m14.ord.podu<- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = ord.podu.data)
m15.ord.podu <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = ord.podu.data)

# inspection des modèles candidats

summary(m1.ord.podu) # nan visite et cut
summary(m2.ord.podu)# nan visite et CWD
summary(m3.ord.podu)# SE elevée litter
summary(m4.ord.podu)# se élevée sur canope
summary(m5.ord.podu)# se elevée sur analogue
summary(m6.ord.podu)# nan precip et cut
summary(m7.ord.podu)# nan precip et CWD
summary(m8.ord.podu)# nan litter
summary(m9.ord.podu)
summary(m10.ord.podu) 
summary(m11.ord.podu)
summary(m12.ord.podu)# nan cut et precip
summary(m13.ord.podu)
summary(m14.ord.podu)# se elevée sur canop
summary(m15.ord.podu)# nan analogue




# Entomobryomorpha

det.ord.ento<-read.csv("det_ento_ord.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

ord.ento.data <- unmarkedFrameOccu(y = det.ord.ento[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

ord.ento.data@siteCovs$Cut<-relevel(ord.ento.data@siteCovs$Cut, ref = "temoin")
ord.ento.data@siteCovs$Analogue<-relevel(ord.ento.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(ord.ento.data)
detHist(ord.ento.data)

# modèles candidats

m0.ord.ento <- occu(~ visites.bin ~ (1|Bloc), data = ord.ento.data)
m1.ord.ento <- occu(~ visites.bin ~ Cut + (1|Bloc), data = ord.ento.data)
m2.ord.ento <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = ord.ento.data)
m3.ord.ento <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = ord.ento.data)
m4.ord.ento <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = ord.ento.data)
m5.ord.ento <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = ord.ento.data)
m6.ord.ento <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = ord.ento.data)
m7.ord.ento <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = ord.ento.data)
m8.ord.ento <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = ord.ento.data)
m9.ord.ento <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = ord.ento.data)
m10.ord.ento <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = ord.ento.data)
m11.ord.ento <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = ord.ento.data)
m12.ord.ento <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = ord.ento.data)
m13.ord.ento <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = ord.ento.data)
m14.ord.ento <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = ord.ento.data)
m15.ord.ento <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = ord.ento.data)

# inspection des modèles candidats

summary(m1.ord.ento)# nan cut
summary(m2.ord.ento)
summary(m3.ord.ento)
summary(m4.ord.ento)# nan canop
summary(m5.ord.ento)# nan analogue
summary(m6.ord.ento)# nan cut
summary(m7.ord.ento)# nan precip et cwd
summary(m8.ord.ento)
summary(m9.ord.ento)# nan precip et canop
summary(m10.ord.ento) # nan analogue
summary(m11.ord.ento)# nan canop
summary(m12.ord.ento)
summary(m13.ord.ento)
summary(m14.ord.ento)
summary(m15.ord.ento)# nan canop



# Symphypleona

det.ord.symp<-read.csv("det_symp_ord.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

ord.symp.data <- unmarkedFrameOccu(y = det.ord.symp[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

ord.symp.data@siteCovs$Cut<-relevel(ord.symp.data@siteCovs$Cut, ref = "temoin")
ord.symp.data@siteCovs$Analogue<-relevel(ord.symp.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(ord.symp.data)
detHist(ord.symp.data) #30/60, 0.5

# modèles candidats

m0.ord.symp <- occu(~ visites.bin ~ (1|Bloc), data = ord.symp.data)
m1.ord.symp <- occu(~ visites.bin ~ Cut + (1|Bloc), data = ord.symp.data)
m2.ord.symp <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = ord.symp.data)
m3.ord.symp <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = ord.symp.data)
m4.ord.symp <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = ord.symp.data)
m5.ord.symp <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = ord.symp.data)
m6.ord.symp <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = ord.symp.data)
m7.ord.symp <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = ord.symp.data)
m8.ord.symp <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = ord.symp.data)
m9.ord.symp <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = ord.symp.data)
m10.ord.symp <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = ord.symp.data)
m11.ord.symp <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = ord.symp.data)
m12.ord.symp <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = ord.symp.data)
m13.ord.symp <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = ord.symp.data)
m14.ord.symp <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = ord.symp.data)
m15.ord.symp <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = ord.symp.data)

# inspection des modèles candidats

summary(m0.ord.symp)
summary(m1.ord.symp)# n'arrive a pas a estimer
summary(m2.ord.symp)# ne converge pas
summary(m3.ord.symp)# nan  visite et intercepte
summary(m4.ord.symp)
summary(m5.ord.symp)# se elevée
summary(m6.ord.symp)# se elevée
summary(m7.ord.symp)# ne converge pas
summary(m8.ord.symp)# nan precip et litter
summary(m9.ord.symp)# nan precip et canop
summary(m10.ord.symp) # nan precip et canop
summary(m11.ord.symp)
summary(m12.ord.symp)
summary(m13.ord.symp)# nan litter et cwd
summary(m14.ord.symp)
summary(m15.ord.symp)# nan cwd et analogue




###################### Occu pour FAMILLES des collemboles #################################

# Poduromorpha

det.fam.hypo<-read.csv("det_hypo.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

fam.hypo.data <- unmarkedFrameOccu(y = det.fam.hypo[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.hypo.data@siteCovs$Cut<-relevel(fam.hypo.data@siteCovs$Cut, ref = "temoin")
fam.hypo.data@siteCovs$Analogue<-relevel(fam.hypo.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.hypo.data)
detHist(fam.hypo.data)

# modèles candidats

m0.fam.hypo <- occu(~ visites.bin ~ (1|Bloc), data = fam.hypo.data)
m1.fam.hypo <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.hypo.data)
m2.fam.hypo <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.hypo.data)
m3.fam.hypo <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.hypo.data)
m4.fam.hypo <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.hypo.data)
m5.fam.hypo <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.hypo.data)
m6.fam.hypo <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.hypo.data)
m7.fam.hypo <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.hypo.data)
m8.fam.hypo <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.hypo.data)
m9.fam.hypo <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.hypo.data)
m10.fam.hypo <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.hypo.data)
m11.fam.hypo <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.hypo.data)
m12.fam.hypo <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.hypo.data)
m13.fam.hypo <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.hypo.data)
m14.fam.hypo <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.hypo.data)
m15.fam.hypo <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.hypo.data)

# inspection des modèles candidats

summary(m1.fam.hypo) #SE elevée sur cut
summary(m2.fam.hypo)
summary(m3.fam.hypo)# nan sur litter
summary(m4.fam.hypo)# SE élevé sur canop
summary(m5.fam.hypo)
summary(m6.fam.hypo)
summary(m7.fam.hypo)# SE élevée sur cut
summary(m8.fam.hypo)
summary(m9.fam.hypo)# Nan sur precip
summary(m10.fam.hypo) # Nan sur analogue
summary(m11.fam.hypo)# nan sur cut
summary(m12.fam.hypo) 
summary(m13.fam.hypo)# nan sur litter et cwd
summary(m14.fam.hypo)
summary(m15.fam.hypo)# nan sur analogue




# Isotomidae

det.fam.iso<-read.csv("det_iso.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

fam.iso.data <- unmarkedFrameOccu(y = det.fam.iso[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.iso.data@siteCovs$Cut<-relevel(fam.iso.data@siteCovs$Cut, ref = "temoin")
fam.iso.data@siteCovs$Analogue<-relevel(fam.iso.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.iso.data)
detHist(fam.iso.data)

# modèles candidats

m0.fam.iso <- occu(~ visites.bin ~ (1|Bloc), data = fam.iso.data)
m1.fam.iso <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.iso.data)
m2.fam.iso <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.iso.data)
m3.fam.iso <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.iso.data)
m4.fam.iso <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.iso.data)
m5.fam.iso <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.iso.data)
m6.fam.iso <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.iso.data)
m7.fam.iso <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.iso.data)
m8.fam.iso <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.iso.data)
m9.fam.iso <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.iso.data)
m10.fam.iso <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.iso.data)
m11.fam.iso <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.iso.data)
m12.fam.iso <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.iso.data)
m13.fam.iso <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.iso.data)
m14.fam.iso <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.iso.data)
m15.fam.iso <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.iso.data)

# inspection des modèles candidats

summary(m1.fam.iso)# Nan sur cut
summary(m2.fam.iso)# SE élevé sur cwd
summary(m3.fam.iso)# SE elevée sur litter
summary(m4.fam.iso)# Nan sur canopée
summary(m5.fam.iso)# n'arrive pas a estimer
summary(m6.fam.iso)# Nan sur canope et precip
summary(m7.fam.iso)
summary(m8.fam.iso)
summary(m9.fam.iso)# Nan sur précip
summary(m10.fam.iso) # Nan sur analodgue
summary(m11.fam.iso)# nan sur cut
summary(m12.fam.iso)# se elevée sur cwd
summary(m13.fam.iso)# se elevée sur litter
summary(m14.fam.iso)# n'arrive pas a estimer
summary(m15.fam.iso)# n'arrive pas a estimer




# Onychiuridae ( fonctionne sauf pour un modèle)

det.fam.ony<-read.csv("det_ony.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.ony.data <- unmarkedFrameOccu(y = det.fam.ony[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.ony.data@siteCovs$Cut<-relevel(fam.ony.data@siteCovs$Cut, ref = "temoin")
fam.ony.data@siteCovs$Analogue<-relevel(fam.ony.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.ony.data)
detHist(fam.ony.data)

# modèles candidats

m0.fam.ony <- occu(~ visites.bin ~ (1|Bloc), data = fam.ony.data)
m1.fam.ony <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.ony.data)
m2.fam.ony <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.ony.data)
m3.fam.ony <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.ony.data)
m4.fam.ony <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.ony.data)
m5.fam.ony <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.ony.data)
m6.fam.ony <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.ony.data)
m7.fam.ony <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.ony.data)
m8.fam.ony <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.ony.data)
m9.fam.ony <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.ony.data)
m10.fam.ony <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.ony.data)
m11.fam.ony <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.ony.data)
m12.fam.ony <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.ony.data)
m13.fam.ony <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.ony.data)
m14.fam.ony <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.ony.data)
m15.fam.ony <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.ony.data)

# inspection des modèles candidats

summary(m0.fam.ony)
summary(m1.fam.ony)
summary(m2.fam.ony)
summary(m3.fam.ony)
summary(m4.fam.ony)
summary(m5.fam.ony)
summary(m6.fam.ony)
summary(m7.fam.ony)
summary(m8.fam.ony)
summary(m9.fam.ony)
summary(m10.fam.ony) 
summary(m11.fam.ony)# nan sur cwd et cut
summary(m12.fam.ony)
summary(m13.fam.ony)
summary(m14.fam.ony)
summary(m15.fam.ony)




# Tullbergidae

det.fam.tull<-read.csv("det_tull.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.tull.data <- unmarkedFrameOccu(y = det.fam.tull[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.tull.data@siteCovs$Cut<-relevel(fam.tull.data@siteCovs$Cut, ref = "temoin")
fam.tull.data@siteCovs$Analogue<-relevel(fam.tull.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.tull.data)
detHist(fam.tull.data)

# modèles candidats

m0.fam.tull <- occu(~ visites.bin ~ (1|Bloc), data = fam.tull.data)
m1.fam.tull <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.tull.data)
m2.fam.tull <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.tull.data)
m3.fam.tull <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.tull.data)
m4.fam.tull <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.tull.data)
m5.fam.tull <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.tull.data)
m6.fam.tull <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.tull.data)
m7.fam.tull <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.tull.data)
m8.fam.tull <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.tull.data)
m9.fam.tull <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.tull.data)
m10.fam.tull <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.tull.data)
m11.fam.tull <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.tull.data)
m12.fam.tull <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.tull.data)
m13.fam.tull <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.tull.data)
m14.fam.tull <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.tull.data)
m15.fam.tull <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.tull.data)

# inspection des modèles candidats

summary(m1.fam.tull) # nan viste et cut
summary(m2.fam.tull)# nan cwd
summary(m3.fam.tull)# nan visite et litter
summary(m4.fam.tull)# se elevé canopé
summary(m5.fam.tull)# se elevée analogue
summary(m6.fam.tull)# nan precip et cut
summary(m7.fam.tull)# nan cwd
summary(m8.fam.tull)# n'arrive pas a estimer
summary(m9.fam.tull)# ne converge pas
summary(m10.fam.tull) # se elevée analogue
summary(m11.fam.tull)# nan cut
summary(m12.fam.tull)
summary(m13.fam.tull)
summary(m14.fam.tull)# nan canope
summary(m15.fam.tull)# se elevée analogue




# Dicyrtomidae

det.fam.dicy<-read.csv("det_dicy.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.dicy.data <- unmarkedFrameOccu(y = det.fam.dicy[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.dicy.data@siteCovs$Cut<-relevel(fam.dicy.data@siteCovs$Cut, ref = "temoin")
fam.dicy.data@siteCovs$Analogue<-relevel(fam.dicy.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.dicy.data)
detHist(fam.dicy.data)

# modèles candidats

m0.fam.dicy <- occu(~ visites.bin ~ (1|Bloc), data = fam.dicy.data)
m1.fam.dicy <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.dicy.data)
m2.fam.dicy <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.dicy.data)
m3.fam.dicy <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.dicy.data)
m4.fam.dicy <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.dicy.data)
m5.fam.dicy <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.dicy.data)
m6.fam.dicy <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.dicy.data)
m7.fam.dicy <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.dicy.data)
m8.fam.dicy <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.dicy.data)
m9.fam.dicy <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.dicy.data)
m10.fam.dicy <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.dicy.data)
m11.fam.dicy <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.dicy.data)
m12.fam.dicy <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.dicy.data)
m13.fam.dicy <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.dicy.data)
m14.fam.dicy <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.dicy.data)
m15.fam.dicy <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.dicy.data)

# inspection des modèles candidats

summary(m1.fam.dicy) #SE élevée
summary(m2.fam.dicy) # impossible d'estimer
summary(m3.fam.dicy)# ne converge pas
summary(m4.fam.dicy)# impossible a estimer
summary(m5.fam.dicy)# SE élevée
summary(m6.fam.dicy)# SE élevée
summary(m7.fam.dicy)# impossible a estimer
summary(m8.fam.dicy)# ne converge pas
summary(m9.fam.dicy)# impossible à estimer
summary(m10.fam.dicy) # SE elevé
summary(m11.fam.dicy)
summary(m12.fam.dicy)# ne converge pas
summary(m13.fam.dicy)# ne converge pas
summary(m14.fam.dicy)
summary(m15.fam.dicy)# se elevée




# Entomobryidae

det.fam.ento<-read.csv("det_ento.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.ento.data <- unmarkedFrameOccu(y = det.fam.ento[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.ento.data@siteCovs$Cut<-relevel(fam.ento.data@siteCovs$Cut, ref = "temoin")
fam.ento.data@siteCovs$Analogue<-relevel(fam.ento.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.ento.data)
detHist(fam.ento.data)

# modèles candidats

m0.fam.ento <- occu(~ visites.bin ~ (1|Bloc), data = fam.ento.data)
m1.fam.ento <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.ento.data)
m2.fam.ento <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.ento.data)
m3.fam.ento <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.ento.data)
m4.fam.ento <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.ento.data)
m5.fam.ento <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.ento.data)
m6.fam.ento <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.ento.data)
m7.fam.ento <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.ento.data)
m8.fam.ento <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.ento.data)
m9.fam.ento <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.ento.data)
m10.fam.ento <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.ento.data)
m11.fam.ento <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.ento.data)
m12.fam.ento <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.ento.data)
m13.fam.ento <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.ento.data)
m14.fam.ento <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.ento.data)
m15.fam.ento <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.ento.data)

# inspection des modèles candidats

summary(m1.fam.ento)# se elevée
summary(m2.fam.ento)
summary(m3.fam.ento)
summary(m4.fam.ento)
summary(m5.fam.ento)
summary(m6.fam.ento)# SE elevée
summary(m7.fam.ento)
summary(m8.fam.ento)# nan precip
summary(m9.fam.ento)
summary(m10.fam.ento) 
summary(m11.fam.ento)# SE elevée
summary(m12.fam.ento)
summary(m13.fam.ento)# nan litter
summary(m14.fam.ento)
summary(m15.fam.ento)




# Katiannidae

det.fam.kati<-read.csv("det_kati.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.kati.data <- unmarkedFrameOccu(y = det.fam.kati[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.kati.data@siteCovs$Cut<-relevel(fam.kati.data@siteCovs$Cut, ref = "temoin")
fam.kati.data@siteCovs$Analogue<-relevel(fam.kati.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.kati.data)
detHist(fam.kati.data)

#10% des site avec une detection




# Neelidae

det.fam.neel<-read.csv("det_neel.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

fam.neel.data <- unmarkedFrameOccu(y = det.fam.neel[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.neel.data@siteCovs$Cut<-relevel(fam.neel.data@siteCovs$Cut, ref = "temoin")
fam.neel.data@siteCovs$Analogue<-relevel(fam.neel.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.neel.data)
detHist(fam.neel.data)

# modèles candidats

m0.fam.neel <- occu(~ visites.bin ~ (1|Bloc), data = fam.neel.data)
m1.fam.neel <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.neel.data)
m2.fam.neel <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.neel.data)
m3.fam.neel <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.neel.data)
m4.fam.neel <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.neel.data)
m5.fam.neel <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.neel.data)
m6.fam.neel <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.neel.data)
m7.fam.neel <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.neel.data)
m8.fam.neel <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.neel.data)
m9.fam.neel <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.neel.data)
m10.fam.neel <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.neel.data)
m11.fam.neel <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.neel.data)
m12.fam.neel <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.neel.data)
m13.fam.neel <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.neel.data)
m14.fam.neel <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.neel.data)
m15.fam.neel <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.neel.data)

# inspection des modèles candidats

summary(m1.fam.neel)# Nan
summary(m2.fam.neel)# incapable d'estimer
summary(m3.fam.neel)# nan
summary(m4.fam.neel)
summary(m5.fam.neel)# nan
summary(m6.fam.neel)# nan
summary(m7.fam.neel)# impossible d'estimer
summary(m8.fam.neel)# nan
summary(m9.fam.neel)
summary(m10.fam.neel) #nan
summary(m11.fam.neel)#nan
summary(m12.fam.neel)
summary(m13.fam.neel)
summary(m14.fam.neel)#nan
summary(m15.fam.neel)#nan




# Neanuridae

det.fam.nean<-read.csv("det_nean.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.nean.data <- unmarkedFrameOccu(y = det.fam.nean[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.nean.data@siteCovs$Cut<-relevel (fam.nean.data@siteCovs$Cut, ref = "temoin")
fam.nean.data@siteCovs$Analogue<-relevel(fam.nean.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.nean.data)
detHist(fam.nean.data)

# modèles candidats

m0.fam.nean <- occu(~ visites.bin ~ (1|Bloc), data = fam.nean.data)
m1.fam.nean <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.nean.data)
m2.fam.nean <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.nean.data)
m3.fam.nean <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.nean.data)
m4.fam.nean <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.nean.data)
m5.fam.nean <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.nean.data)
m6.fam.nean <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.nean.data)
m7.fam.nean <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.nean.data)
m8.fam.nean <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.nean.data)
m9.fam.nean <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.nean.data)
m10.fam.nean <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.nean.data)
m11.fam.nean <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.nean.data)
m12.fam.nean <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.nean.data)
m13.fam.nean <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.nean.data)
m14.fam.nean <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.nean.data)
m15.fam.nean <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.nean.data)

# inspection des modèles candidats

summary(m1.fam.nean) # SE
summary(m2.fam.nean)# se
summary(m3.fam.nean)
summary(m4.fam.nean)# se
summary(m5.fam.nean)# se elevée
summary(m6.fam.nean)# se elevée
summary(m7.fam.nean)
summary(m8.fam.nean)
summary(m9.fam.nean)
summary(m10.fam.nean) # se elevée
summary(m11.fam.nean)# SE elevèée
summary(m12.fam.nean)# se elevée
summary(m13.fam.nean)
summary(m14.fam.nean)
summary(m15.fam.nean)# se elevée




# Sminthuridae

det.fam.smind<-read.csv("det_smind.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.smind.data <- unmarkedFrameOccu(y = det.fam.smind[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.smind.data@siteCovs$Cut<-relevel(fam.smind.data@siteCovs$Cut, ref = "temoin")
fam.smind.data@siteCovs$Analogue<-relevel(fam.smind.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.smind.data)
detHist(fam.smind.data)

# modèles candidats

m0.fam.smind <- occu(~ visites.bin ~ (1|Bloc), data = fam.smind.data)
m1.fam.smind <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.smind.data)
m2.fam.smind <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.smind.data)
m3.fam.smind <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.smind.data)
m4.fam.smind <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.smind.data)
m5.fam.smind <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.smind.data)
m6.fam.smind <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.smind.data)
m7.fam.smind <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.smind.data)
m8.fam.smind <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.smind.data)
m9.fam.smind <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.smind.data)
m10.fam.smind <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.smind.data)
m11.fam.smind <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.smind.data)
m12.fam.smind <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.smind.data)
m13.fam.smind <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.smind.data)
m14.fam.smind <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.smind.data)
m15.fam.smind <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.smind.data)

# inspection des modèles candidats

summary(m1.fam.smind)# SE elevée
summary(m2.fam.smind)# SE elevée
summary(m3.fam.smind)# SE elevée
summary(m4.fam.smind)# SE elevée
summary(m5.fam.smind)# SE elevée
summary(m6.fam.smind)
summary(m7.fam.smind)
summary(m8.fam.smind)
summary(m9.fam.smind)
summary(m10.fam.smind) #nan
summary(m11.fam.smind)# SE elevée
summary(m12.fam.smind)
summary(m13.fam.smind)
summary(m14.fam.smind)
summary(m15.fam.smind)# nan




# Sminthurididae

det.fam.smindd<-read.csv("det_smindd.csv",header = TRUE, stringsAsFactors = TRUE)

# Unmarked - occu

fam.smindd.data <- unmarkedFrameOccu(y = det.fam.smindd[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.smindd.data@siteCovs$Cut<-relevel(fam.smindd.data@siteCovs$Cut, ref = "temoin")
fam.smindd.data@siteCovs$Analogue<-relevel(fam.smindd.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.smindd.data)
detHist(fam.smindd.data)

# modèles candidats

m0.fam.smindd <- occu(~ visites.bin ~ (1|Bloc), data = fam.smindd.data)
m1.fam.smindd <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.smindd.data)
m2.fam.smindd <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.smindd.data)
m3.fam.smindd <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.smindd.data)
m4.fam.smindd <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.smindd.data)
m5.fam.smindd <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.smindd.data)
m6.fam.smindd <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.smindd.data)
m7.fam.smindd <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.smindd.data)
m8.fam.smindd <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.smindd.data)
m9.fam.smindd <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.smindd.data)
m10.fam.smindd <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.smindd.data)
m11.fam.smindd <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.smindd.data)
m12.fam.smindd <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.smindd.data)
m13.fam.smindd <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.smindd.data)
m14.fam.smindd <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.smindd.data)
m15.fam.smindd <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.smindd.data)

# inspection des modèles candidats

summary(m1.fam.smindd)#nan visite
summary(m2.fam.smindd)#nan cwd et visites
summary(m3.fam.smindd)#nan litter et visites
summary(m4.fam.smindd)#nan canope et visite
summary(m5.fam.smindd)#nan visite
summary(m6.fam.smindd)#nan cut
summary(m7.fam.smindd)# nan
summary(m8.fam.smindd)# nan
summary(m9.fam.smindd)# nan
summary(m10.fam.smindd) # nan
summary(m11.fam.smindd)# nan
summary(m12.fam.smindd)
summary(m13.fam.smindd)
summary(m14.fam.smindd)
summary(m15.fam.smindd)




# Tomoceridae

det.fam.tomo<-read.csv("det_tomo.csv",header = TRUE, stringsAsFactors = TRUE)
# Unmarked - occu

fam.tomo.data <- unmarkedFrameOccu(y = det.fam.tomo[,-1], siteCovs = sites_cov[, -1],
                                   obsCovs = list(visites.bin = visites.bin[,-1],
                                                  precip.3D.bin = precip.3D.bin[,-1],
                                                  CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

fam.tomo.data@siteCovs$Cut<-relevel(fam.tomo.data@siteCovs$Cut, ref = "temoin")
fam.tomo.data@siteCovs$Analogue<-relevel(fam.tomo.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(fam.tomo.data)
detHist(fam.tomo.data)

# modèles candidats

m0.fam.tomo <- occu(~ visites.bin ~ (1|Bloc), data = fam.tomo.data)
m1.fam.tomo <- occu(~ visites.bin ~ Cut + (1|Bloc), data = fam.tomo.data)
m2.fam.tomo <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = fam.tomo.data)
m3.fam.tomo <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = fam.tomo.data)
m4.fam.tomo <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = fam.tomo.data)
m5.fam.tomo <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = fam.tomo.data)
m6.fam.tomo <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = fam.tomo.data)
m7.fam.tomo <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = fam.tomo.data)
m8.fam.tomo <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = fam.tomo.data)
m9.fam.tomo <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = fam.tomo.data)
m10.fam.tomo <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = fam.tomo.data)
m11.fam.tomo <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = fam.tomo.data)
m12.fam.tomo <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = fam.tomo.data)
m13.fam.tomo <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = fam.tomo.data)
m14.fam.tomo <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = fam.tomo.data)
m15.fam.tomo <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = fam.tomo.data)

# inspection des modèles candidats

summary(m1.fam.tomo)
summary(m2.fam.tomo)
summary(m3.fam.tomo)
summary(m4.fam.tomo)#nan
summary(m5.fam.tomo)
summary(m6.fam.tomo)# se elevée
summary(m7.fam.tomo)# se elevée
summary(m8.fam.tomo)# nan
summary(m9.fam.tomo)# nan
summary(m10.fam.tomo) 
summary(m11.fam.tomo)# se elevée
summary(m12.fam.tomo)# non converge
summary(m13.fam.tomo)# se elevée
summary(m14.fam.tomo)# nan
summary(m15.fam.tomo)# se elevée






