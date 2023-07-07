#Packages
install.packages("unmarked")
library(unmarked)

install.packages("AICcmodavg")
library(AICcmodavg)

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

# importation des matrice d'abondances 
det_car<-read.csv("Car_detection.csv", header = TRUE, stringsAsFactors = TRUE)
#rownames(det_car)<-det_car[,1]
#det_car<-det_car[,c("V1","V2","V3","V4")]
str(det_car)

det_coll<-read.csv("Coll_detection.csv", header = TRUE, stringsAsFactors = TRUE)
#rownames(det_coll)<-det_coll[,1]
#det_coll<-det_coll[,c("V1","V2","V3","V4")]

#importation des variables de sites
sites_cov<-read.csv("sites_cov.csv", header = TRUE, stringsAsFactors = TRUE)

#importations des variables meteo

Airtemp<- read.csv("airtemp_std.csv", header = TRUE, stringsAsFactors = TRUE)
Rh<- read.csv("rh_std.csv", header = TRUE, stringsAsFactors = TRUE)
Jday<- read.csv("jday_std.csv", header = TRUE, stringsAsFactors = TRUE)
Precip_3D<- read.csv("precip3D_std.csv",header = TRUE,stringsAsFactors = TRUE)

#supression de la colonne des sites

row.names(Airtemp)<-Airtemp[,1]
Airtemp<-Airtemp[,c("X1","X2","X3","X4")]

row.names(Rh)<-Rh[,1]
Rh<-Rh[,c("X1","X2","X3","X4")]

row.names(Jday)<-Jday[,1]
Jday<-Jday[,c("X1","X2","X3","X4")]

row.names(Precip_3D)<-Precip_3D[,1]
Precip_3D<-Precip_3D[,c("X1","X2","X3","X4")]

#Occu

car.data <- unmarkedFrameOccu(y = det_car[, -1], siteCovs = sites_cov[, -1],
                                obsCovs = list(airtemp.std = Airtemp,
                                               rh = Rh,
                                               jday = Jday,
                                               precip.3d = Precip_3D))

summary(car.data)
detHist(car.data)

#modèele null
m0 <- occu(~ jday ~ (1|Bloc), data = car.data)
summary(m0)

#modèles candidat
#test d'ajustement
gof_car <- mb.gof.test(m0, nsim = 5000, plot.hist = TRUE) 
save(gof_car, file = "car_gof.Rdata")

#################################

#historique de detection

#carabes
nsites_car <- nrow(det_car)
histD_car <- rep(x = NA, times = nsites_car)
for (i in 1:nsites_car) {
  histD_car[i] <- paste(det_car[i,], collapse = "")
}
histD_car

tabSum_car <- table(histD_car)
tabSum_car

#collemboles
nsites_coll <- nrow(det_coll)
histD_coll <- rep(x = NA, times = nsites_coll)
for (i in 1:nsites_coll) {
  histD_coll[i] <- paste(det_coll[i,], collapse = "")
}
histD_coll

tabSum_coll <- table(histD_coll)
tabSum_coll
