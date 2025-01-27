# models selection
library(unmarked)
library(AICcmodavg)

#telechargement des dataframes
load(file = "SEM_data_JAGS.RData")

## abondance de p.cin
pcin<- data.JAGS$pcin[,-1]
car_comp<- data.JAGS$car_comp[,-1]
car_prey<-data.JAGS$car_prey[,-1]


## covariables influancant la detection

airtemp_std<- read.csv("cov_airtemp_std.csv", header = TRUE, stringsAsFactors = TRUE)
rh_std<- read.csv("cov_rh_std.csv", header = TRUE, stringsAsFactors = TRUE)
jday_bin<- read.csv("cov_jday_bin.csv", header = TRUE, stringsAsFactors = TRUE)
precip_3d_bin<- read.csv("cov_precip_3d_bin.csv",header = TRUE,stringsAsFactors = TRUE)

airtemp_std = airtemp_std[,-1]
rh_std = rh_std[-1]
jday_bin = jday_bin[,-1]
precip_3d_bin = precip_3d_bin[,-1]

# Airtemp_mat <- as.matrix(mat_airtemp)
# airtemp_mean <- mean(Airtemp_mat, na.rm = TRUE)
# airtemp_sd <- sd(Airtemp_mat, na.rm = TRUE)
# airtemp_std <- as.data.frame((Airtemp_mat - airtemp_mean)/airtemp_sd)



##Unmarked
pcin.data<-unmarkedFramePCount(y=pcin, siteCovs = data.JAGS$Obs_cov,
                               obsCovs = list(jday= jday_bin,
                                              precip= precip_3d_bin,
                                              airtemp= airtemp_std,
                                              rh= rh_std))

# changement pour mettre les témoins comme référence
pcin.data@siteCovs$Cut<-relevel(pcin.data@siteCovs$Coupe, ref = "temoin")


summary(pcin.data)
countHist(pcin.data)


## modeles
m0 <-pcount(~ jday ~ (1|Bloc), K=100, mixture = "P", data = pcin.data)
m1 <-pcount(~ jday ~ Cut + (1|Bloc) , K=100, mixture = "P", data = pcin.data)
m2 <-pcount(~ jday ~ Cut_Exclos + (1|Bloc) , K=100, mixture = "P", data = pcin.data)
m3 <-pcount(~ jday + Precip ~ (1|Bloc) , K=100, mixture = "P", data = pcin.data)
m4 <-pcount(~ jday + Precip ~ Cut + (1|Bloc) , K=100, mixture = "P", data = pcin.data)
m5 <-pcount(~ jday + Precip ~ Cut_Exclos + (1|Bloc), K=100 , mixture = "P", data = pcin.data)

logLik(m0)

summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

Cands.pois<- list("m0"=m0, "m1"=m1, "m2"=m2,"m3"=m3,"m4"=m4, "m5"=m5)
#sapply(Cands.pois, FUN = function(i) extractCN(i)$CN)
#extractCN(m1)


aictab(Cands.pois)

m0.gof.pcin <- Nmix.gof.test(mod = m0, nsim = 1000)
save(m0.gof.pcin, file = "Nmix-m0-Pois-pcin.Rdata")

m1.gof$chi.square
# chi carré de 210.8075 p=0.39
m1.gof$c.hat.est
#1.03477

# la valeur de K est trop élevée ce qui provient possiblement d'un problème dans l'estimation
# avec ce modèles et le faible nombre de détection
# 
