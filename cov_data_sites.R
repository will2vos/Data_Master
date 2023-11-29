# Transformation des mesures de débris ligneux en volume de débris ligneux (CWD)

# import data
CWD <-
  read.csv("cov_cwd_mesures.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

# transform in factor
CWD$Analogue <- as.factor(CWD$Analogue)
CWD$Classe <- as.factor(CWD$Classe)
str(CWD)

# calcul area from diameter
#Diam_bas_m = diamètre basal en mètre
#Diam_api_m = diamètre apical en mètre
CWD$area.bas <- pi * (CWD$Diam_bas_m / 2) ^ 2
CWD$area.api <- pi * (CWD$Diam_api_m / 2) ^ 2

# tranform length form cm in m
CWD$length_m <- CWD$Longueur / 100

# calcul CWD in m3
CWD$cwd <- (CWD$length_m / 12) * (5 * CWD$Diam_bas_m + 5 * CWD$Diam_api_m +
                                    2 * sqrt(CWD$Diam_bas_m * CWD$Diam_api_m))

#import basic informations for 60 sites
Data60 <-
  read.csv(
    "Data_60.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = TRUE
  )

# sum of CWD for each sites
colnames(CWD)[1] <- "sites"
Data60$CWD <- with(tapply(cwd, INDEX = sites, FUN = sum), data = CWD)

#importation data litter depth
litt <-
  read.csv("cov_litter_mesures.csv",
           header = TRUE,
           stringsAsFactors = TRUE)
Data60$litter <- litt$Moyenne

#importation data canopy at 130 cm from ground
Canop_130 <-
  read.csv("cov_canop130_mesures.csv",
           header = TRUE,
           stringsAsFactors = TRUE)
Data60$canopy <- Canop_130$Ouverture

identical(Data60$sites, litt$ID_site, Canop_130$ID_site)
any(is.na(Data60))

# obsCov standardized
CWD_mat <- as.matrix(Data60$CWD)
CWD_mean <- mean(CWD_mat, na.rm = TRUE)
CWD_sd <- sd(CWD_mat, na.rm = TRUE)
CWD_std <- as.data.frame((CWD_mat - CWD_mean) / CWD_sd)
Data60$CWD_STD<- CWD_std$V1

Litter<-as.matrix(Data60$litter)
Lit_mean <- mean(Litter, na.rm = TRUE)
Lit_sd <- sd(Litter, na.rm = TRUE)
litter_std <- as.data.frame((Litter - Lit_mean)/Lit_sd)
Data60$litter_STD<- litter_std$V1

Canop130<-as.matrix(Data60$canopy)
Can130_mean <- mean(Canop130, na.rm = TRUE)
Can130_sd <- sd(Canop130, na.rm = TRUE)
canop130_std <- as.data.frame((Canop130 - Can130_mean)/Can130_sd)
Data60$canopy_STD<- canop130_std$V1

# write.csv(Data60, "ObsCov.csv", row.names = FALSE)

# inspect correlations

plot(~ CWD + litter + canopy,  data = Data60)

cor(Data60$CWD, Data60$litter, method = "pearson")
cor(Data60$CWD, Data60$canopy, method = "pearson")
cor(Data60$litter, Data60$canopy, method = "pearson")

# refaire modeéles d'occupation, sélection de modèle(pcin, carabes), model mixte ( collemboles, cwd~treatments)

