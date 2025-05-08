## Master Degree
# Results presentation
library(dplyr)

##load clean file
load(file = "SEM_data_JAGS.RData") # données de base
# load(file = "out.jags.sem6.Rdata") # résultats d'analyse SEM
load(file = "out.jags.sem6.CWD.Rdata") # résultats d'analyse SEM avec CWD non standardis/ pour reponse

# load(file = "out.jags.sem6.2.Rdata")

str(data.JAGS)


### correlations 

Obs_cov = data.JAGS$Obs_cov[, c("CWD", "litter", "canopy")]
colnames(Obs_cov) = c("CWD", "Litter", "Canopy")

# occupancy variables
names(data.JAGS$Obs_cov)
plot(~ CWD + Litter + Canopy,  data = Obs_cov)

cor(data.JAGS$Obs_cov$CWD, data.JAGS$Obs_cov$litter, method = "pearson")
cor(data.JAGS$Obs_cov$CWD, data.JAGS$Obs_cov$canopy, method = "pearson")
cor(data.JAGS$Obs_cov$litter, data.JAGS$Obs_cov$canopy, method = "pearson")

# detection variables
Meteo<-read.csv("Donnees_meteo_20221212.csv",header = TRUE, sep=";",dec = ",", stringsAsFactors = TRUE)
names(Meteo)

Meteo_select = Meteo[,c("date", "rh", "airtemp", "soltemp", "precip")] 

Meteo_select$date <- as.Date(Meteo_select$date)

Meteo_select <- Meteo_select %>%
  filter(date >= as.Date("2022-05-01") & date <= as.Date("2022-08-31"))

grouped_meteo <- Meteo_select %>% 
  group_by(date) %>%
  summarise(airtemp = mean(airtemp, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE))

plot(~ airtemp + rh + precip , data = grouped_meteo)

cor(grouped_meteo$total_precip, grouped_meteo$avg_airtemp, method = "pearson")
# 0.03792767
cor(grouped_meteo$total_precip, grouped_meteo$avg_rh, method = "pearson")
# 0.386192
cor(grouped_meteo$avg_airtemp, grouped_meteo$avg_rh, method = "pearson")
# 0.2096332

cor(grouped_meteo[, c("total_preci", "avg_rh", "avg_airtemp")])


#########

# Cut ~ Environmental variables  

#########

############

# CWD

#############

## distribution des  différences de postérieurs
# control vs partial
diff.1v2.cwd <- out.jags$sims.list$beta.Cutpartial.cwd

hist(diff.1v2.cwd)

quant1v2.cwd <- quantile(diff.1v2.cwd, probs = c(0.025, 0.975))
abline(
  v = quant1v2.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.cwd
#pas d'effet

# control vs total
diff.1v3.cwd <- out.jags$sims.list$beta.Cutclear.cwd

hist(diff.1v3.cwd)

quant1v3.cwd <- quantile(diff.1v3.cwd, probs = c(0.025, 0.975))
abline(
  v = quant1v3.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.cwd

quant1v3.cwd.90 <- quantile(diff.1v3.cwd, probs = c(0.05, 0.95))
abline(
  v = quant1v3.cwd.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant1v3.cwd.90
#pas d'effet


# partial vs clear
#calcule des moyennes au lieu de differences
cwd.cutpartial <- out.jags$sims.list$beta0.cwd + out.jags$sims.list$beta.Cutpartial.cwd
cwd.cutclear <- out.jags$sims.list$beta0.cwd + out.jags$sims.list$beta.Cutclear.cwd

diff.2v3.cwd <-cwd.cutclear - cwd.cutpartial

hist(diff.2v3.cwd)

quant2v3.cwd <- quantile(diff.2v3.cwd, probs = c(0.025, 0.975))
abline(
  v = quant2v3.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.cwd
#effet a 95%

# figure 

beta.CWD<- as.data.frame(matrix(data = NA, 
                                nrow = 125000,
                                ncol = 3))

colnames(beta.CWD)<- c("beta.Control", "beta.Cutpartial", "beta.Cutclear")

beta.CWD[,1]<- out.jags$sims.list$beta0.cwd
beta.CWD[,2]<- out.jags$sims.list$beta0.cwd + out.jags$sims.list$beta.Cutpartial.cwd
beta.CWD[,3]<- out.jags$sims.list$beta0.cwd + out.jags$sims.list$beta.Cutclear.cwd

data.CWD<- as.data.frame(matrix(data = NA, 
                  nrow = 3,
                  ncol = 3))

colnames(data.CWD)<- c("mean", "2.5%", "97.5%")

data.CWD[1,1]<- mean(beta.CWD[,1])
data.CWD[2,1]<- mean(beta.CWD[,2])
data.CWD[3,1]<- mean(beta.CWD[,3])

data.CWD[1,2]<- quantile(beta.CWD[,1], probs = 0.025)
data.CWD[2,2]<- quantile(beta.CWD[,2], probs = 0.025)
data.CWD[3,2]<- quantile(beta.CWD[,3], probs = 0.025)

data.CWD[1,3]<- quantile(beta.CWD[,1], probs = 0.975)
data.CWD[2,3]<- quantile(beta.CWD[,2], probs = 0.975)
data.CWD[3,3]<- quantile(beta.CWD[,3], probs = 0.975)






# CWD_tot<-read.csv("cov_cwd_tot.csv",header = TRUE, sep=",",dec= ".", stringsAsFactors = TRUE)
# CWD_tot<- CWD_tot[,-1]
# 
# CWD_mat<-as.matrix(CWD_tot)
# CWD_mean<- mean(CWD_mat, na.rm = TRUE)
# CWD_sd<- sd(CWD_mat, na.rm = TRUE)
# CWD_std<- as.data.frame((CWD_mat - CWD_mean)/CWD_sd)


# CWD<-as.data.frame(out.jags$summary[c("beta0.cwd","beta.Cutpartial.cwd","beta.Cutclear.cwd"),
#                      c("mean", "2.5%", "97.5%")], 2)
# 
# CWD_data<- as.data.frame(CWD[1,])
# CWD_data[2,]<- CWD[1,]+CWD[2,]
# CWD_data[3,]<- CWD[1,]+CWD[3,]

# CWD_init<- as.data.frame((CWD_data * CWD_sd) + CWD_mean)

par(mfrow = c(1, 3), mar = c(5, 4.5, 4, 2) + 0.1)

plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 4),
  xlab = "Treatments",
  ylab = expression("CWD (m"^{3} * ")"),
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

points(
  x = 1:3,
  y = data.CWD$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

segments(
  x0 = 1:3,
  y0 = data.CWD$`2.5%`,
  1:3,
  data.CWD$`97.5%`,
  col = "black",
  lwd = 2
)

overstory = c("Control", "Partial-cut", "Clear-cut")

axis(
  1,
  at = 1:3,
  labels = overstory,
  lwd = 1,
  padj = 0.5
)

mtext("A", side=3, line=1, cex=2, adj=0.1, col="black") 

############

# Canopy

#############

## distribution des  différences de postérieurs
# control vs partial
diff.1v2.can <- out.jags$sims.list$beta.Cutpartial.can

hist(diff.1v2.can)

quant1v2.can <- quantile(diff.1v2.can, probs = c(0.025, 0.975))
abline(
  v = quant1v2.can,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.can
#effet: plus grande ouverture de la canopé dans coupe partiel

# control vs total
diff.1v3.can <- out.jags$sims.list$beta.Cutclear.can

hist(diff.1v3.can)

quant1v3.can <- quantile(diff.1v3.can, probs = c(0.025, 0.975))
abline(
  v = quant1v3.can,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.can
#effet : plus grand ouverture de la canope dans les coupe totale


# partial vs total
#calcule des moyennes au lieu de differences
can.cutpartial <- out.jags$sims.list$beta0.can + out.jags$sims.list$beta.Cutpartial.can
can.cutclear <- out.jags$sims.list$beta0.can + out.jags$sims.list$beta.Cutclear.can

diff.2v3.can <- can.cutclear - can.cutpartial

hist(diff.2v3.can)

quant2v3.can <- quantile(diff.2v3.can, probs = c(0.025, 0.975))
abline(
  v = quant2v3.can,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.can
#effet : plus grande ouverture dans la coupe total

# Canope<-as.data.frame(
#   out.jags$summary[c("beta0.can","beta.Cutpartial.can","beta.Cutclear.can"),
#                      c("mean", "sd", "2.5%", "97.5%")], 2)
# 
# Can_data<- as.data.frame(Canope[1,]*100)
# Can_data[2,]<- (Canope[1,]+Canope[2,])*100
# Can_data[3,]<- (Canope[1,]+Canope[3,])*100

beta.can<- as.data.frame(matrix(data = NA, 
                                nrow = 125000,
                                ncol = 3))

colnames(beta.can)<- c("beta.Control", "beta.Cutpartial", "beta.Cutclear")

beta.can[,1]<- (out.jags$sims.list$beta0.can) * 100
beta.can[,2]<- (out.jags$sims.list$beta0.can + out.jags$sims.list$beta.Cutpartial.can) * 100
beta.can[,3]<- (out.jags$sims.list$beta0.can + out.jags$sims.list$beta.Cutclear.can) * 100

data.can<- as.data.frame(matrix(data = NA, 
                                nrow = 3,
                                ncol = 3))

colnames(data.can)<- c("mean", "2.5%", "97.5%")

data.can[1,1]<- mean(beta.can[,1])
data.can[2,1]<- mean(beta.can[,2])
data.can[3,1]<- mean(beta.can[,3])

data.can[1,2]<- quantile(beta.can[,1], probs = 0.025)
data.can[2,2]<- quantile(beta.can[,2], probs = 0.025)
data.can[3,2]<- quantile(beta.can[,3], probs = 0.025)

data.can[1,3]<- quantile(beta.can[,1], probs = 0.975)
data.can[2,3]<- quantile(beta.can[,2], probs = 0.975)
data.can[3,3]<- quantile(beta.can[,3], probs = 0.975)


# png("fig_canopy.png", width = 800, height = 600, res = 130)

plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 100),
  xlab = "Treatments",
  ylab = "Canopy openess (%)",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

points(
  x = 1:3,
  y = data.can$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

segments(
  x0 = 1:3,
  y0 = data.can$`2.5%`,
  1:3,
  data.can$`97.5%`,
  col = "black",
  lwd = 2
)

axis(1, at = 1:3, labels = overstory, lwd = 1, padj = 1) 

mtext("B", side=3, line=1, cex=2, adj=0.1, col="black") 

# dev.off()

#############

# Litter

#############

## distribution des  différences de postérieurs
# control vs partial
diff.1v2.lit <- out.jags$sims.list$beta.Cutpartial.lit

hist(diff.1v2.lit)

quant1v2.lit <- quantile(diff.1v2.lit, probs = c(0.025, 0.975))
abline(
  v = quant1v2.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.lit
# effet : moins de litiere dans les coupe partiel que dans les témoins


# control vs total
diff.1v3.lit <- out.jags$sims.list$beta.Cutclear.lit

hist(diff.1v3.lit)

quant1v3.lit <- quantile(diff.1v3.lit, probs = c(0.025, 0.975))
abline(
  v = quant1v3.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.lit
#effet : moins de litière dans les coupes totales que dna sles témoins

# partial vs total
#calcule des moyennes au lieu de differences
lit.cutpartial <- out.jags$sims.list$beta0.lit + out.jags$sims.list$beta.Cutpartial.lit
lit.cutclear <- out.jags$sims.list$beta0.lit + out.jags$sims.list$beta.Cutclear.lit

diff.2v3.lit <- lit.cutclear - lit.cutpartial 

hist(diff.2v3.lit)

quant2v3.lit <- quantile(diff.2v3.lit, probs = c(0.025, 0.975))
abline(
  v = quant2v3.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.lit
#effet : moins de litière dans les coupe totale que dans les coupe partielle 



## figure mean + IC

# Litter<-as.data.frame(
#   out.jags$summary[c("beta0.lit","beta.Cutpartial.lit","beta.Cutclear.lit"),
#                      c("mean", "sd", "2.5%", "97.5%")], 2)
# 
# Lit_data<- as.data.frame(Litter[1,])
# Lit_data[2,]<- Litter[1,]+Litter[2,]
# Lit_data[3,]<- Litter[1,]+Litter[3,]

beta.lit<- as.data.frame(matrix(data = NA, 
                                nrow = 125000,
                                ncol = 3))

colnames(beta.lit)<- c("beta.Control", "beta.Cutpartial", "beta.Cutclear")

beta.lit[,1]<- out.jags$sims.list$beta0.lit
beta.lit[,2]<- out.jags$sims.list$beta0.lit + out.jags$sims.list$beta.Cutpartial.lit
beta.lit[,3]<- out.jags$sims.list$beta0.lit + out.jags$sims.list$beta.Cutclear.lit

data.lit<- as.data.frame(matrix(data = NA, 
                                nrow = 3,
                                ncol = 3))

colnames(data.lit)<- c("mean", "2.5%", "97.5%")

data.lit[1,1]<- mean(beta.lit[,1])
data.lit[2,1]<- mean(beta.lit[,2])
data.lit[3,1]<- mean(beta.lit[,3])

data.lit[1,2]<- quantile(beta.lit[,1], probs = 0.025)
data.lit[2,2]<- quantile(beta.lit[,2], probs = 0.025)
data.lit[3,2]<- quantile(beta.lit[,3], probs = 0.025)

data.lit[1,3]<- quantile(beta.lit[,1], probs = 0.975)
data.lit[2,3]<- quantile(beta.lit[,2], probs = 0.975)
data.lit[3,3]<- quantile(beta.lit[,3], probs = 0.975)

plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 8),
  xlab = "Treatments",
  ylab = "Litter depth (cm)",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

points(
  x = 1:3,
  y = data.lit$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

segments(
  x0 = 1:3,
  y0 = data.lit$`2.5%`,
  1:3,
  data.lit$`97.5%`,
  col = "black",
  lwd = 2
)

axis(1, at = 1:3, labels = overstory, lwd = 1, padj = 1) 

mtext("C", side=3, line=1, cex=2, adj=0.1, col="black") 

########## 

# Psi salamander

######### 

# 1v2

par(mfrow = c(1, 1))
diff.1v2.sal <- out.jags$sims.list$psi.sal[, 2] - out.jags$sims.list$psi.sal[, 1]
  
hist(diff.1v2.sal)
quant1v2.sal <- quantile(diff.1v2.sal, probs = c(0.025, 0.975))
abline(
  v = quant1v2.sal,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.sal
mean(diff.1v2.sal)

quant1v2.sal.90 <- quantile(diff.1v2.sal, probs = c(0.05, 0.95))
abline(
  v = quant1v2.sal.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant1v2.sal.90

# 1v3

diff.1v3.sal <- out.jags$sims.list$psi.sal[, 3] - out.jags$sims.list$psi.sal[, 1]
hist(diff.1v3.sal)
quant1v3.sal <- quantile(diff.1v3.sal, probs = c(0.025, 0.975))
abline(
  v = quant1v3.sal,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.sal
 
quant1v3.sal.90 <- quantile(diff.1v3.sal, probs = c(0.05, 0.95))
abline(
  v = quant1v3.sal.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant1v3.sal.90

# 2v3

diff.2v3.sal <- out.jags$sims.list$psi.sal[, 3] - out.jags$sims.list$psi.sal[, 2]
  
hist(diff.2v3.sal)
quant2v3.sal <- quantile(diff.2v3.sal, probs = c(0.025, 0.975))
abline(
  v = quant2v3.sal,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.sal
mean(diff.2v3.sal)

quant2v3.sal.90 <- quantile(diff.2v3.sal, probs = c(0.05, 0.95))
abline(
  v = quant2v3.sal.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant2v3.sal.90
mean(diff.2v3.sal)

## figure estimate + IC

data.sal<- as.data.frame(matrix(data = NA, 
                                nrow = 3,
                                ncol = 3))

colnames(data.sal)<- c("mean", "2.5%", "97.5%")

data.sal[1,1]<- mean(out.jags$sims.list$psi.sal[,1])
data.sal[2,1]<- mean(out.jags$sims.list$psi.sal[,2])
data.sal[3,1]<- mean(out.jags$sims.list$psi.sal[,3])

data.sal[1,2]<- quantile(out.jags$sims.list$psi.sal[,1], probs = 0.025)
data.sal[2,2]<- quantile(out.jags$sims.list$psi.sal[,2], probs = 0.025)
data.sal[3,2]<- quantile(out.jags$sims.list$psi.sal[,3], probs = 0.025)

data.sal[1,3]<- quantile(out.jags$sims.list$psi.sal[,1], probs = 0.975)
data.sal[2,3]<- quantile(out.jags$sims.list$psi.sal[,2], probs = 0.975)
data.sal[3,3]<- quantile(out.jags$sims.list$psi.sal[,3], probs = 0.975)

plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 1),
  xlab = "Treatments",
  ylab = "Psi",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

points(
  x = 1:3,
  y = data.sal$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

segments(
  x0 = 1:3,
  y0 = data.sal$`2.5%`,
  1:3,
  data.sal$`97.5%`,
  col = "black",
  lwd = 2
)

axis(1, at = 1:3, labels = overstory, lwd = 1, padj = 1) 



######### 

# Psi carabid - competitor

######### 

# 1v2

diff.1v2.car.comp <- out.jags$sims.list$psi.car.comp[, 2] - out.jags$sims.list$psi.car.comp[, 1]
hist(diff.1v2.car.comp)
quant1v2.car.comp <- quantile(diff.1v2.car.comp, probs = c(0.025, 0.975))
abline(
  v = quant1v2.car.comp,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.car.comp
mean(diff.1v2.car.comp)

# quant1v2.car.comp.90 <- quantile(diff.1v2.car.comp, probs = c(0.05, 0.95))
# abline(
#   v = quant1v2.car.comp.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant1v2.car.comp.90

# 1v3

diff.1v3.car.comp <- out.jags$sims.list$psi.car.comp[, 3] - out.jags$sims.list$psi.car.comp[, 1]
hist(diff.1v3.car.comp)
quant1v3.car.comp <- quantile(diff.1v3.car.comp, probs = c(0.025, 0.975))
abline(
  v = quant1v3.car.comp,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.car.comp
mean(diff.1v3.car.comp)

# quant1v3.car.comp.90 <- quantile(diff.1v3.car.comp, probs = c(0.05, 0.95))
# abline(
#   v = quant1v3.car.comp.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant1v3.car.comp.90

# 2v3

diff.2v3.car.comp <- out.jags$sims.list$psi.car.comp[, 3] - out.jags$sims.list$psi.car.comp[, 2]
hist(diff.2v3.car.comp)
quant2v3.car.comp <- quantile(diff.2v3.car.comp, probs = c(0.025, 0.975))
abline(
  v = quant2v3.car.comp,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.car.comp
mean(diff.2v3.car.comp)

# quant2v3.car.comp.90 <- quantile(diff.2v3.car.comp, probs = c(0.05, 0.95))
# abline(
#   v = quant2v3.car.comp.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant2v3.car.comp.90

## figure estimate + IC

beta.car.comp<- out.jags$sims.list$psi.car.comp

data.car.comp<- as.data.frame(matrix(data = NA, 
                                nrow = 3,
                                ncol = 3))

colnames(data.car.comp)<- c("mean", "2.5%", "97.5%")

data.car.comp[1,1]<- mean(out.jags$sims.list$psi.car.comp[,1])
data.car.comp[2,1]<- mean(out.jags$sims.list$psi.car.comp[,2])
data.car.comp[3,1]<- mean(out.jags$sims.list$psi.car.comp[,3])

data.car.comp[1,2]<- quantile(out.jags$sims.list$psi.car.comp[,1], probs = 0.025)
data.car.comp[2,2]<- quantile(out.jags$sims.list$psi.car.comp[,2], probs = 0.025)
data.car.comp[3,2]<- quantile(out.jags$sims.list$psi.car.comp[,3], probs = 0.025)

data.car.comp[1,3]<- quantile(out.jags$sims.list$psi.car.comp[,1], probs = 0.975)
data.car.comp[2,3]<- quantile(out.jags$sims.list$psi.car.comp[,2], probs = 0.975)
data.car.comp[3,3]<- quantile(out.jags$sims.list$psi.car.comp[,3], probs = 0.975)



# Car.comp <- as.data.frame(out.jags$summary[c(
#   "psi.beta0.car.prey",
#   "psi.beta.z.sal.car.prey",
#   "psi.beta.Cutpartial.car.prey",
#   "psi.beta.Cutclear.car.prey",
#   "alpha0.car.prey",
#   "alpha.precip.car.prey",
#   "alpha.cwd.car.prey",
#   "sigma.block.car.prey",
#   "finiteOcc.car.prey"
# ), c("mean", "sd", "2.5%", "97.5%")])
# 
# car_prey_data<- as.data.frame(Car.prey[1,])
# car_prey_data[2,]<- Car.prey[1,]+Car.prey[3,]
# car_prey_data[3,]<- Car.prey[1,]+Car.prey[4,]

plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 1),
  xlab = "Treatments",
  ylab = "Probability",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

points(
  x = 1:3,
  y = data.car.comp$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

segments(
  x0 = 1:3,
  y0 = data.car.comp$`2.5%`,
  1:3,
  data.car.comp$`97.5%`,
  col = "black",
  lwd = 2
)

axis(1, at = 1:3, labels = overstory, lwd = 1, padj = 1) 


######### 

# Psi carabid - prey

######### 

## transform logit to probability scale

# p.beta.partial.car.prey <- exp(out.jags$sims.list$psi.beta.Cutpartial.car.prey) / (1 + exp(out.jags$sims.list$psi.beta.Cutpartial.car.prey))
# p.beta.partial.car.prey <- plogis(out.jags$sims.list$psi.beta.Cutpartial.car.prey)
# p.beta.clear.car.prey <- exp(out.jags$sims.list$psi.beta.Cutclear.car.prey) / (1 + exp(out.jags$sims.list$psi.beta.Cutclear.car.prey))
# p.z.sal.car.prey <- exp(out.jags$sims.list$psi.beta.z.sal.car.prey) / (1 + exp(out.jags$sims.list$psi.beta.z.sal.car.prey))
# alpha.precip.car.prey <- exp(out.jags$sims.list$alpha.precip.car.prey) / (1 + exp(out.jags$sims.list$alpha.precip.car.prey))
# alpha.cwd.car.prey <- exp(out.jags$sims.list$alpha.cwd.car.prey) / (1 + exp(out.jags$sims.list$alpha.cwd.car.prey))

# 1v2

diff.1v2.car.prey <- out.jags$sims.list$psi.beta.Cutpartial.car.prey
hist(diff.1v2.car.prey)
quant1v2.car.prey <- quantile(diff.1v2.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant1v2.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.car.prey
mean(diff.1v2.car.prey)

# quant1v2.car.prey.90 <- quantile(diff.1v2.car.prey, probs = c(0.05, 0.95))
# abline(
#   v = quant1v2.car.prey.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant1v2.car.prey.90

# 1v3

diff.1v3.car.prey <- out.jags$sims.list$psi.beta.Cutclear.car.prey
hist(diff.1v3.car.prey)
quant1v3.car.prey <- quantile(diff.1v3.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant1v3.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.car.prey
mean(diff.1v3.car.prey)

# quant1v3.car.prey.90 <- quantile(diff.1v3.car.prey, probs = c(0.05, 0.95))
# abline(
#   v = quant1v3.car.prey.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant1v3.car.prey.90

# 2v3

beta.cutpartial.car.prey <- out.jags$sims.list$psi.beta.Cutpartial.car.prey + out.jags$sims.list$psi.beta0.car.prey
beta.cutclear.car.prey <- out.jags$sims.list$psi.beta.Cutclear.car.prey + out.jags$sims.list$psi.beta0.car.prey

diff.2v3.car.prey <- beta.cutclear.car.prey - beta.cutpartial.car.prey
hist(diff.2v3.car.prey)
quant2v3.car.prey <- quantile(diff.2v3.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant2v3.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.car.prey
mean(diff.2v3.car.prey)

# quant2v3.car.prey.90 <- quantile(diff.2v3.car.prey, probs = c(0.05, 0.95))
# abline(
#   v = quant2v3.car.prey.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant2v3.car.prey.90

# z.sal

z.sal.car.prey <- out.jags$sims.list$psi.beta.z.sal.car.prey
hist(z.sal.car.prey)
quant.z.sal.car.prey <- quantile(z.sal.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant.z.sal.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.z.sal.car.prey
mean(z.sal.car.prey)

# quant.z.sal.car.prey.90 <- quantile(z.sal.car.prey, probs = c(0.05, 0.95))
# abline(
#   v = quant.z.sal.car.prey.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant.z.sal.car.prey.90

######### 

# Springtails biomass

######### 

# 1v2

diff.1v2.col <- out.jags$sims.list$beta.Cutpartial.coll

hist(diff.1v2.col)
quant1v2.col <- quantile(diff.1v2.col, probs = c(0.025, 0.975))
abline(
  v = quant1v2.col,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.col
mean(diff.1v2.col)

quant1v2.col.90 <- quantile(diff.1v2.col, probs = c(0.05, 0.95))
abline(
  v = quant1v2.col.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant1v2.col.90

# 1v3

diff.1v3.col <- out.jags$sims.list$beta.Cutclear.coll
hist(diff.1v3.col)
quant1v3.col <- quantile(diff.1v3.col, probs = c(0.025, 0.975))
abline(
  v = quant1v3.col,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.col
mean(diff.1v3.col)

quant1v3.col.90 <- quantile(diff.1v3.col, probs = c(0.05, 0.95))
abline(
  v = quant1v3.col.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant1v3.col.90

# 2v3

beta.cutpartial.col<- out.jags$sims.list$beta.Cutpartial.coll + out.jags$sims.list$beta0.coll
beta.cutclear.col<- out.jags$sims.list$beta.Cutclear.coll + out.jags$sims.list$beta0.coll

diff.2v3.col <- beta.cutclear.col - beta.cutpartial.col

hist(diff.2v3.col)
quant2v3.col <- quantile(diff.2v3.col, probs = c(0.025, 0.975))
abline(
  v = quant2v3.col,
  lty = 2,
  col = "red",
  lwd = 3
)

quant2v3.col
mean(diff.2v3.col)

quant2v3.col.90 <- quantile(diff.2v3.col, probs = c(0.05, 0.95))
abline(
  v = quant2v3.col.90,
  lty = 2,
  col = "blue",
  lwd = 3
)
quant2v3.col.90
mean(diff.2v3.col)

# z.sal

z.sal.coll <- out.jags$sims.list$beta.z.sal.coll
hist(z.sal.coll)
quant.z.sal.coll <- quantile(z.sal.coll, probs = c(0.025, 0.975))
abline(
  v = quant.z.sal.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.z.sal.coll
mean(z.sal.coll)

# quant.z.sal.coll.90 <- quantile(z.sal.coll, probs = c(0.05, 0.95))
# abline(
#   v = quant.z.sal.coll.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant.z.sal.coll.90


# z.car.comp

z.car.comp.coll <- out.jags$sims.list$beta.z.car.comp.coll
hist(z.car.comp.coll)
quant.z.car.comp.coll <- quantile(z.car.comp.coll, probs = c(0.025, 0.975))
abline(
  v = quant.z.car.comp.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.z.car.comp.coll
mean(z.car.comp.coll)

# quant.z.car.comp.coll.90 <- quantile(z.car.comp.coll, probs = c(0.05, 0.95))
# abline(
#   v = quant.z.car.comp.coll.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant.z.car.comp.coll.90


# z.car.prey

z.car.prey.coll <- out.jags$sims.list$beta.z.car.prey.coll
hist(z.car.prey.coll)
quant.z.car.prey.coll <- quantile(z.car.prey.coll, probs = c(0.025, 0.975))
abline(
  v = quant.z.car.prey.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.z.car.prey.coll
mean(z.car.prey.coll)

# quant.z.car.prey.coll.90 <- quantile(z.car.prey.coll, probs = c(0.05, 0.95))
# abline(
#   v = quant.z.car.prey.coll.90,
#   lty = 2,
#   col = "blue",
#   lwd = 3
# )
# quant.z.car.prey.coll.90


## detection

# alpha.precip.car.comp

alpha.precip.car.comp <- out.jags$sims.list$alpha.precip.car.comp
hist(alpha.precip.car.comp)
quant.alpha.precip.car.comp <- quantile(alpha.precip.car.comp, probs = c(0.025, 0.975))
abline(
  v = quant.alpha.precip.car.comp,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.alpha.precip.car.comp
mean(alpha.precip.car.comp)

# alpha.cwd.car.comp

alpha.cwd.car.comp <- out.jags$sims.list$alpha.cwd.car.comp
hist(alpha.cwd.car.comp)
quant.alpha.cwd.car.comp <- quantile(alpha.cwd.car.comp, probs = c(0.025, 0.975))
abline(
  v = quant.alpha.cwd.car.comp,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.alpha.cwd.car.comp
mean(alpha.cwd.car.comp)

# alpha.precip.car.prey

alpha.precip.car.prey <- out.jags$sims.list$alpha.precip.car.prey
hist(alpha.precip.car.prey)
quant.alpha.precip.car.prey <- quantile(alpha.precip.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant.alpha.precip.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.alpha.precip.car.prey
mean(alpha.precip.car.prey)

# alpha.cwd.car.prey

alpha.cwd.car.prey <- out.jags$sims.list$alpha.cwd.car.prey
hist(alpha.cwd.car.prey)
quant.alpha.cwd.car.prey <- quantile(alpha.cwd.car.prey, probs = c(0.025, 0.975))
abline(
  v = quant.alpha.cwd.car.prey,
  lty = 2,
  col = "red",
  lwd = 3
)
quant.alpha.cwd.car.prey
mean(alpha.cwd.car.prey)

## salamander abundance

pcin<- read.csv("pcin_abun.csv", stringsAsFactors = TRUE, header = TRUE)

sum(pcin[,-1])
obs.pcin<- colSums(pcin[, -1])
mean(obs.pcin)
diff(range(obs.pcin))


# springtail biomass

Biom <- read.csv("coll_biomass.csv", header = TRUE, stringsAsFactors = TRUE)

Biom2<- aggregate(Biom$weight, list(Biom$Coupe), FUN = mean)
Biom3<- aggregate(Biom$weight, list(Biom$Coupe), FUN = sd)
Biom4<- cbind(Biom2,Biom3[,-1])

coll.control <- subset(Biom, Coupe == "temoin")
coll.partial <- subset(Biom, Coupe == "partielle")
coll.clear <- subset(Biom, Coupe == "totale")

mean(coll.control$weight)
sd(coll.control$weight)

mean(coll.partial$weight)
sd(coll.partial$weight)

mean(coll.clear$weight)
sd(coll.clear$weight)
