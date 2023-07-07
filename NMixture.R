
install.packages("unmarked")
library(unmarked)

install.packages("AICcmodavg")
library(AICcmodavg)

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

#telechargement des dataframes

## abondance de p.cin

mat_abun_pcin<-read.csv("Pcin_abundance.csv", header = TRUE, stringsAsFactors = TRUE)
row.names(mat_abun_pcin)<-mat_abun_pcin[,1]
mat_abun_pcin<-mat_abun_pcin[,c("V1","V2","V3","V4")]

## covariables influancant la detection

mat_airtemp<- read.csv("pcin_airtemp.csv", header = TRUE, stringsAsFactors = TRUE)
mat_rh<- read.csv("pcin_rh.csv", header = TRUE, stringsAsFactors = TRUE)
mat_jday<- read.csv("pcin_jday.csv", header = TRUE, stringsAsFactors = TRUE)
mat_precip_3D<- read.csv("precip_3D.csv.csv",header = TRUE,stringsAsFactors = TRUE)


## covariables influancant l'occupation

mat_CWD<-read.csv("Matrice_CWD.csv", header = TRUE, stringsAsFactors = TRUE)
mat_litter<-read.csv("Matrice_litter_depth.csv", header = TRUE, stringsAsFactors = TRUE)
mat_canop130<-read.csv("Matrice_canope130.csv", header = TRUE, stringsAsFactors = TRUE)

Cov_occu<-read.csv("Data_60.csv",header = TRUE, sep=";",dec= ",", stringsAsFactors = TRUE)
Cov_occu$Coupe<- ifelse(Cov_occu$Coupe =="totale","CT",
                        ifelse(Cov_occu$Coupe == "partielle","CP","temoin"))
Cov_occu$Exclos<- ifelse(Cov_occu$Exclos =="avec","avec","sans")

Cov_occu$Cut_Exclos <-as.factor(paste(Cov_occu$Coupe, Cov_occu$Exclos, sep="_"))


#standardisation des donnees

##jour julien
rownames(mat_jday)<-mat_jday$ID
mat_jday<-mat_jday[,c("X1","X2","X3","X4")]



Jday_mat <- as.matrix(mat_jday)
jday_mean <- mean(Jday_mat, na.rm = TRUE)
jday_sd <- sd(Jday_mat, na.rm = TRUE)
jday_std <- as.data.frame((Jday_mat - jday_mean)/jday_sd)


##precipitation
rownames(mat_precip_3D)<-mat_precip_3D$Jday
mat_precip_3D<-mat_precip_3D[,c("X1","X2","X3","X4")]

precip_3D <- as.matrix(mat_precip_3D)
precip3D_mean <- mean(precip_3D, na.rm = TRUE)
precip3D_sd <- sd(precip_3D, na.rm = TRUE)
precip3D_std <- as.data.frame((precip_3D - precip3D_mean)/precip3D_sd)

##airtemp
rownames(mat_airtemp)<-mat_airtemp$ID
mat_airtemp<-mat_airtemp[,c("X1","X2","X3","X4")]


Airtemp_mat <- as.matrix(mat_airtemp)
airtemp_mean <- mean(Airtemp_mat, na.rm = TRUE)
airtemp_sd <- sd(Airtemp_mat, na.rm = TRUE)
airtemp_std <- as.data.frame((Airtemp_mat - airtemp_mean)/airtemp_sd)

##rh
rownames(mat_rh)<-mat_rh$ID
mat_rh<-mat_rh[,c("X1","X2","X3","X4")]

Rh_mat <- as.matrix(mat_rh)
rh_mean <- mean(Rh_mat, na.rm = TRUE)
rh_sd <- sd(Rh_mat, na.rm = TRUE)
rh_std <- as.data.frame((Rh_mat - rh_mean)/rh_sd)

#write.csv(rh_std,"rh_std.csv")
#write.csv(airtemp_std,"airtemp_std.csv")
#write.csv(jday_std,"jday_std.csv")
#write.csv(precip3D_std,"precip3D_std.csv")

##construction d'un dataframe regroupant les différentes informations des sites

CWD<-as.matrix(mat_CWD$V2)
CWD_mean <- mean(CWD, na.rm = TRUE)
CWD_sd <- sd(CWD, na.rm = TRUE)
CWD_std <- as.data.frame((CWD - CWD_mean)/CWD_sd)

Litter<-as.matrix(mat_litter$V2)
Lit_mean <- mean(Litter, na.rm = TRUE)
Lit_sd <- sd(Litter, na.rm = TRUE)
litter_std <- as.data.frame((Litter - Lit_mean)/Lit_sd)

Canop130<-as.matrix(mat_canop130$V2)
Can130_mean <- mean(Canop130, na.rm = TRUE)
Can130_sd <- sd(Canop130, na.rm = TRUE)
canop130_std <- as.data.frame((Canop130 - Can130_mean)/Can130_sd)

sites_cov<-as.data.frame(matrix(ncol=8, nrow = 60))
sites_cov[,1]<-mat_CWD$V1
sites_cov[,2]<-mat_CWD$V2
sites_cov[,3]<-litter_std
sites_cov[,2]<-CWD_std
sites_cov[,4]<-canop130_std
sites_cov[,5]<-Cov_occu$Bloc
sites_cov[,6]<-as.factor(Cov_occu$Coupe)
sites_cov[,7]<-Cov_occu$Exclos
sites_cov[,8]<-Cov_occu$Cut_Exclos
sites_cov[,9]<-Cov_occu$Analogue

colnames(sites_cov)[]<-c("ID_site","CWD_STD","Litter_STD","Canope130_STD","Bloc","Cut","Exclos","Cut_Exclos","Analogue")
#write.csv(sites_cov,"sites_cov.csv", row.names = FALSE)

rownames(sites_cov)<-sites_cov$V1
sites_cov<-sites_cov[,c("CWD_STD","Litter_STD","Canope130_STD","Bloc","Cut","Exclos","Cut_Exclos")]
sites_cov$Exclos<-as.factor(sites_cov$Exclos)

colnames(jday_std)[]<-c("V1","V2","V3","V4")
colnames(precip3D_std)[]<-c("V1","V2","V3","V4")
colnames(airtemp_std)[]<-c("V1","V2","V3","V4")
colnames(rh_std)[]<-c("V1","V2","V3","V4")


# création data-frame avec visite

mat_visites<-as.data.frame(matrix(nrow = 60, ncol = 5))
mat_visites$V1<-Cov_occu$ID
mat_visites$V2<-as.factor(1)
mat_visites$V3<-as.factor(2)
mat_visites$V4<-as.factor(3)
mat_visites$V5<-as.factor(4)
row.names(mat_visites)<-mat_visites$V1
mat_visites<-mat_visites[,c("V2","V3","V4","V5")]
colnames(mat_visites)[]<-c("V1","V2","V3","V4")

mat_visites$V1<-as.numeric(mat_visites$V1)
mat_visites$V2<-as.numeric(mat_visites$V2)
mat_visites$V3<-as.numeric(mat_visites$V3)
mat_visites$V4<-as.numeric(mat_visites$V4)

mat_visites$V1<-as.factor(mat_visites$V1)
mat_visites$V2<-as.factor(mat_visites$V2)
mat_visites$V3<-as.factor(mat_visites$V3)
mat_visites$V4<-as.factor(mat_visites$V4)

# matrice p. canop

canop130_std$V2<-canop130_std$V1
canop130_std$V3<-canop130_std$V1
canop130_std$V4<-canop130_std$V1


##Unmarked

pcin.data<-unmarkedFramePCount(y=mat_abun_pcin, siteCovs = sites_cov,
                               obsCovs = list(JDay= jday_std,
                                              Precip= precip3D_std,
                                              AirTemp= airtemp_std,
                                              Rh= rh_std,
                                              Canop130=canop130_std))

#changement pour mettre les témoins comme référence

pcin.data@siteCovs$Cut<-relevel(pcin.data@siteCovs$Cut, ref = "temoin")
pcin.data@siteCovs$Cut_Exclos<-relevel(pcin.data@siteCovs$Cut_Exclos, ref = "temoin_sans")


summary(pcin.data)
countHist(pcin.data)


##modeles
m0 <-pcount(~ JDay ~ (1|Bloc), K=1100, mixture = "P", data = pcin.data)
m1 <-pcount(~ JDay ~ Cut + (1|Bloc) , K=1100, mixture = "P", data = pcin.data)
m2 <-pcount(~ JDay ~ Cut_Exclos + (1|Bloc) , K=1100, mixture = "P", data = pcin.data)
m3 <-pcount(~ JDay + Precip ~ (1|Bloc) , K=1100, mixture = "P", data = pcin.data)
m4 <-pcount(~ JDay + Precip ~ Cut + (1|Bloc) , K=1100, mixture = "P", data = pcin.data)
m5 <-pcount(~ JDay + Precip ~ Cut_Exclos + (1|Bloc), K=1100 , mixture = "P", data = pcin.data)

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
