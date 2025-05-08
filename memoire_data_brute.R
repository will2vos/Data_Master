# positionnement
setwd("/Users/williamdevos/Documents/Maitrise/Data.Git")

# descompte des salamandres en fonction des traitements
pcin <- read.csv("pcin_abun.csv", header = TRUE, stringsAsFactors = TRUE)
pcin$total <- rowSums(pcin[,2:5])

info.sites <- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE)

info.sites$pcin <- with(tapply(total, INDEX = sites, FUN = sum),
                       data = pcin)

resultat_aggregate <- aggregate(pcin ~ Coupe, data = info.sites, sum)
resultat_aggregate

# descompte des carabes en fonction des traitements
car <- read.csv("ID_CAR.csv", stringsAsFactors = TRUE, header = TRUE, sep = ";")

table( car$coupe, car$species)

# données brute sur les variables environnementales

# CWD

CWD <- read.csv("cov_cwd_m3.csv", header = TRUE, stringsAsFactors = TRUE)
CWD_agg <- aggregate(CWD_tot ~ Coupe, data = CWD, sum)
CWD_agg

CWD_sd <- aggregate(CWD_tot ~ Coupe, data = CWD, sd)
CWD_sd

boxplot(CWD$CWD_tot ~ CWD$Coupe)

# Litter

litter <- read.csv("cov_litter_mesures.csv", header = TRUE, stringsAsFactors = TRUE)
litter_agg <- aggregate(Moyenne ~ Coupe, data = litter, mean)
litter_agg

litter_sd <- aggregate(Moyenne ~ Coupe, data = litter, sd)
litter_sd

litter$Coupe <- factor(litter$Coupe, levels = c("temoin", "partielle", "totale"))

boxplot(litter$Moyenne ~ litter$Coupe,
        xlab = "Treatments",
        ylab = "Litter depth (cm)",
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

mtext("C", side=3, line=1, cex=2, adj=0.1, col="black") 

#Canop

canop <- read.csv("cov_canop130_mesures.csv", header = TRUE, stringsAsFactors = TRUE)
canop_agg <- aggregate(Ouverture ~ Coupe, data = canop, mean)
canop_agg

canop_sd <- aggregate(Ouverture ~ Coupe, data = canop, sd)
canop_sd

canop$Coupe <- factor(canop$Coupe, levels = c("temoin", "partielle", "totale"))


boxplot(canop$Ouverture ~ canop$Coupe,
        xlab = NA,
        ylab = "Canopy openning (%)",
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

mtext("B", side=3, line=1, cex=2, adj=0.1, col="black") 
