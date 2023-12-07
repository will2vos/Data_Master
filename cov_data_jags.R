
# regroupement des données pour analyses JAGS

##SALAMANDRES
pcin_det <-
  read.csv("pcin_det.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

# pcin_det2<- pcin_det[order(pcin_det$site),]
# write.csv(pcin_det2, "pcin_det.csv", row.names = FALSE )

pcin <- pcin_det[,-1]

## CARABES
# Groupe proies de salamandres
car_det_prey <-
  read.csv("car_det_prey.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

car_prey <- car_det_prey[,-1]

# Groupe competiteurs de salamandres
car_det_comp <-
  read.csv("car_det_comp.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

car_comp <- car_det_comp[,-1]

## COLLEMBOLES
coll_biom <-
  read.csv("coll_biomass.csv",
           header = TRUE,
           stringsAsFactors = TRUE)



## VARIABLES ENVIRONNEMENTALES
# Sites
Obs_cov <-
  read.csv("ObsCov.csv", header = TRUE, stringsAsFactors = TRUE)

# precipitation (binaire)
precip.3D.bin <-
  read.csv("cov_precip_3d_bin.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

precip <- precip.3D.bin[,-1]

# Volume de débris ligneux (Standardisé) en isolé en dataframe pour detection
CWD.STD <- as.data.frame(list(Obs_cov$sites, Obs_cov$CWD_STD))
colnames(CWD.STD) <- c("sites", "CWD_STD")


# verification si les sites donner sont dans le même ordre

identical(
  pcin_det$site,
  car_det_comp$ID,
  car_det_prey$ID,
  coll_biom$site,
  Obs_cov$sites,
  precip.3D.bin$X,
  CWD.STD$sites
)

data.JAGS <- list(
  pcin = pcin_det,
  car_prey = car_det_prey,
  car_comp = car_det_comp,
  coll = coll_biom,
  Obs_cov = Obs_cov,
  precip = precip.3D.bin,
  CWD_STD = CWD.STD
)

save(data.JAGS, file = "SEM_data_JAGS.RData")

load("SEM_data_JAGS.RData")
