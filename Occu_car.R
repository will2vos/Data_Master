# occupations carabes 
#Packages
#install.packages("unmarked")
library(unmarked)

#install.packages("AICcmodavg")
library(AICcmodavg)

library(TMB)

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

###################### PREPARATION DES DONNÉES #################################

# importation du data frame d'abondance de carabes
ID_car2<- read.csv("ID_CARrabe.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")


abun_car_tab2 <- table(ID_car2$ID_site, ID_car2$mois)
abun_car2 <- as.data.frame.matrix(abun_car_tab2)
abun_ca2<- abun_car[,c("5","6","7", "8")]

det_car2<- as.data.frame(ifelse(abun_car2 == 0,0,1))

colnames(det_car2)<-c("mai","juin", "juillet", "aout")





ID_car<- read.csv("ID_CAR.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

abun_car_tab <- table(ID_car$site, ID_car$month)
abun_car <- as.data.frame.matrix(abun_car_tab)
abun_car<- abun_car[,c("mai","juin","juillet", "aout")]

# abondance vers detection

det_car<- as.data.frame(ifelse(abun_car == 0,0,1))
#write.csv(det_car, file = "det_car.csv", row.names = TRUE)
#colnames(det_car)<-c("X1","X2","X3","X4")


# certain site ne n'ont pas de detection il n'y a donc que 50 lignes

sites<-row.names(det_car)
det_car$ID<-sites

data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

data_car<- merge(x=data60, y=det_car, by="ID", all.x = TRUE)

det_car<-data_car[,c("mai","juin","juillet", "aout")]

det_car[is.na(det_car)]<-0



# importantion des variables influencant l'occupation

sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importation des variable de détéction standardisé (construite dans le script NMixture.R)
#airtemp<- read.csv("airtemp_std.csv", header = TRUE, stringsAsFactors = TRUE)
#rh<- read.csv("rh_std.csv", header = TRUE, stringsAsFactors = TRUE)
#jday.std<- read.csv("jday_std.csv", header = TRUE, stringsAsFactors = TRUE)
#precip.3D.std<- read.csv("precip3D_std.csv",header = TRUE,stringsAsFactors = TRUE)
visites.bin<- read.csv("visites_bin.csv", header = TRUE, stringsAsFactors = TRUE)
precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)
CWD.tot<- read.csv("cwd_total_std.csv",header = TRUE,stringsAsFactors = TRUE)


#changement du nom de ligne et retrait de la colonne X

# row.names(airtemp)<-airtemp[,1]
# airtemp<-airtemp[,c("X1","X2","X3","X4")]
# 
# row.names(rh)<-rh[,1]
# rh<-rh[,c("X1","X2","X3","X4")]
# 
# row.names(jday.std)<-jday.std[,1]
# jday.std<-jday.std[,c("X1","X2","X3","X4")]
# 
# row.names(precip.3D.bin)<-precip.3D.bin[,1]
# precip.3D.bin<-precip.3D.bin[,c("X1","X2","X3","X4")]
# 
# row.names(precip.3D.std)<-precip.3D.std[,1]
# precip.3D.std<-precip.3D.std[,c("X1","X2","X3","X4")]

# création d'une variable visite

# visites <- as.data.frame(matrix(nrow = 60, ncol = 5))
# visites$V1 <- sites_cov$ID_site
# visites$V2 <- factor(1, levels = c("1", "2", "3", "4"))
# visites$V3 <- factor(2, levels = c("1", "2", "3", "4"))
# visites$V4 <- factor(3, levels = c("1", "2", "3", "4"))
# visites$V5 <- factor(4, levels = c("1", "2", "3", "4"))
# 
# colnames(visites)[]<-c("ID","X1","X2","X3","X4")
# 
# row.names(visites)<-visites[,c("ID")]
# visites<-visites[,c("X1","X2","X3","X4")]

# création matrice de visites binaire

# visites.bin<- as.data.frame(matrix(nrow = 60, ncol = 5))
# visites.bin$V1 <- sites_cov$ID_site
# visites.bin$V2 <- 0
# visites.bin$V3 <- 0
# visites.bin$V4 <- 1
# visites.bin$V5 <- 1
# colnames(visites.bin)[]<-c("ID","X1","X2","X3","X4")
# row.names(visites.bin)<-visites.bin[,c("ID")]
# visites.bin<-visites.bin[,c("X1","X2","X3","X4")]


# création d'une matrice pour inclure le CWD dans les variables affectant la détéction

# CWD.tot <- as.data.frame(matrix(nrow = 60, ncol = 5))
# CWD.tot$V1 <-sites_cov$ID_site
# CWD.tot$V2 <-sites_cov$CWD_STD
# CWD.tot$V3 <-sites_cov$CWD_STD
# CWD.tot$V4 <-sites_cov$CWD_STD
# CWD.tot$V5 <-sites_cov$CWD_STD
# 
# row.names(CWD.tot)<-CWD.tot$V1
# CWD.tot <-CWD.tot[,c("V2","V3","V4","V5")]
# colnames(CWD.tot)[]<-c("X1","X2","X3","X4")

#write.csv(CWD.tot,"cwd_total_std.csv")

# ajout d'une colonne exclos en binaire dans les variables de site

#sites_cov$Excl.bin<-as.factor(ifelse(sites_cov$Exclos=="avec",1,0))

# Unmarked - occu

car.data <- unmarkedFrameOccu(y = det_car, siteCovs = sites_cov[, -1],
                              obsCovs = list(visites.bin = visites.bin[,-1],
                                             precip.3D.bin = precip.3D.bin[,-1],
                                             CWD.tot = CWD.tot[,-1]))

# Changement de référence dans les comparaisons de traitements

car.data@siteCovs$Cut<-relevel(car.data@siteCovs$Cut, ref = "temoin")
car.data@siteCovs$Analogue<-relevel(car.data@siteCovs$Analogue, ref = "temoin")

# inspection de pcin.data

summary(car.data)
detHist(car.data)

# modèles candidats

m0 <- occu(~ visites.bin ~ (1|Bloc), data = car.data)
m1 <- occu(~ visites.bin ~ Cut + (1|Bloc), data = car.data)
m2 <- occu(~ visites.bin ~ CWD_STD + (1|Bloc), data = car.data)
m3 <- occu(~ visites.bin ~ Litter_STD + (1|Bloc), data = car.data)
m4 <- occu(~ visites.bin ~ Canope130_STD + (1|Bloc), data = car.data)
m5 <- occu(~ visites.bin ~ Analogue + (1|Bloc), data = car.data)
m6 <- occu(~ precip.3D.bin ~ Cut + (1|Bloc), data = car.data)
m7 <- occu(~ precip.3D.bin ~ CWD_STD + (1|Bloc), data = car.data)
m8 <- occu(~ precip.3D.bin ~ Litter_STD + (1|Bloc), data = car.data)
m9 <- occu(~ precip.3D.bin ~ Canope130_STD + (1|Bloc), data = car.data)
m10 <- occu(~ precip.3D.bin ~ Analogue + (1|Bloc), data = car.data)
m11 <- occu(~ CWD.tot ~ Cut + (1|Bloc), data = car.data)
m12 <- occu(~ CWD.tot ~ CWD_STD + (1|Bloc), data = car.data)
m13 <- occu(~ CWD.tot ~ Litter_STD + (1|Bloc), data = car.data)
m14 <- occu(~ CWD.tot ~ Canope130_STD + (1|Bloc), data = car.data)
m15 <- occu(~ CWD.tot ~ Analogue + (1|Bloc), data = car.data)

# inspection des modèles candidats

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10) 
summary(m11)
summary(m12)
summary(m13)
summary(m14)
summary(m15)

# séléction de modèles

Can_car <- list( m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
Names <- c("m1","m2","m3","m4","m5","m6", "m7","m8","m9","m10","m11","m12","m13","m14","m15")

aictab(cand.set = Can_car, modnames = Names, c.hat = 1.05)

# test d'ajustement

m9.2 <- occu(~ precip.3D.bin ~ Canope130_STD, data = car.data)

gof.car.m9 <- mb.gof.test(m9.2, nsim = 5000, plot.hist = TRUE) 
save(gof.car.m9, file = "gof.car.m9.Rdata")
load(file = "gof.car.m9.Rdata")

# gof.car.m9.png

gof.car.m9$chisq.table
gof.car.m9$c.hat.est
#c.hat=1.045605

# extraction de l'estimé de paramètres
coef(m9)

# psi
# psi.beta0.car <- coef(m9)["psi(Int)"]
# psi.beta.Canope130STD <- coef(m9)["psi(Canope130_STD)"] 
# Canope130STD <- sites_cov$Canope130_STD
# 
# # p
# p.beta0.car <- coef(m9)["p(Int)"]
# p.beta.precip.3D.std <- coef(m9)["p(precip.3D.std)"]
# 
# precip.3D<- read.csv("pcin_precip3D.csv",header = TRUE,stringsAsFactors = TRUE)
# 
# 
# # nombre de visites et de sites
# nsites <- nrow(det_car)
# nvisits <- ncol(det_car)
# 
# #calcule du prédicteur linéaire
# # psi - vector
# psi.logit <- psi.beta0.car + psi.beta.Canope130STD * Canope130STD
# psi.logit[1:8]
# 
# # p - matrix
# p.logit <- p.beta0.car + p.beta.precip.3D.std * precip.3D[,-1]
# head(p.logit)


# selection de modèle avec AICcmodavg

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.05, parm = "Canope130_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.05, parm = "precip.3D.bin",
             parm.type = "detect")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.05, parm = "Litter_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.05, parm = "CWD_STD",
             parm.type = "psi")

modavgShrink(cand.set = Can_car, modnames = Names, c.hat = 1.05, parm = "Cut",
             parm.type = "psi")

str(Can_car@ siteCovs)

# prédiction

dat.precip <- data.frame(precip.3D.bin = factor(c("0", 
                                                 "1"),
                                               levels = c("0","1")))

dat.precip$visites.bin<- 0
dat.precip$CWD.tot<- 0

preds.precip <- modavgPred(cand.set = Can_car, modnames = Names,
                            c.hat = 1, newdata = dat.precip,
                            type = "response", parm.type = "detect")

dat.precip$mod.avg.pred <- preds.precip$mod.avg.pred
dat.precip$low95 <- preds.precip$lower.CL
dat.precip$upp95 <- preds.precip$upper.CL

xvals <- c(0.25, 0.75)

par(mar = c(4.5, 5, 5, 2.1))

plot(mod.avg.pred ~ xvals,
     data = dat.precip,
     main = "Probabilité de détection des carabidae, 
     selon le niveau de précipitation.",
     ylab = "Probabilité de détection",
     xlab = "Précipitation",
     ylim = range(c(low95, upp95)),
     type = "p", cex = 1.5, 
     cex.lab = 1.5, cex.axis = 1.5,cex.main = 1.5,
     xaxt = "n", xlim = c(0, 1))

##add x axis
axis(side = 1, at = xvals, 
     labels = c("faible", "élevé"), 
     cex.lab = 1.5,cex.axis = 1.5)

##add confidence bands
segments(x0 = xvals, 
         x1 = xvals,
         y0 = dat.precip$low95, 
         y1 = dat.precip$upp95,
         lwd = 3)





