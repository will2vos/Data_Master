
#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")
#setwd("/home/mazerolm/Documents/Supervision/Devos/Scripts/2023-07-07")

# ### Importation des dataframes de detections des carabes
# 
# ## TOUT LES CARABES
# det_car_all<- read.csv("Car_detection.csv", header = TRUE, stringsAsFactors = TRUE)
# car_all<- det_car_all[,-1]
# 
# 
# ## TROIS GUILDES ~ MICROHABITAT
# # 3 guildes - Forest
# car_det_for_3<- read.csv("car_det_for_3.csv", header = TRUE, stringsAsFactors = TRUE)
# car_for3<- car_det_for_3[,-1]
# 
# # 3 guildes - Open
# car_det_open_3<- read.csv("car_det_open_3.csv", header = TRUE, stringsAsFactors = TRUE)
# car_open3<- car_det_open_3[,-1]
# 
# # 3 guildes - Aquatic
# car_det_aqua_3<- read.csv("car_det_aqua_3.csv", header = TRUE, stringsAsFactors = TRUE)
# car_aqua3<- car_det_aqua_3[,-1]
# 
# 
# 
# ## DEUX GUILDES ~ MICROHABITAT
# # 2 guildes - Forest
# car_det_for_2<- read.csv("car_det_for_2.csv", header = TRUE, stringsAsFactors = TRUE)
# car_for2<- car_det_for_2[,-1]
# 
# # 2 guildes - Open
# car_det_open_2<- read.csv("car_det_open_2.csv", header = TRUE, stringsAsFactors = TRUE)
# car_open2<- car_det_open_2[,-1]
# 
# 
# 
# ## DEUX GUILDES ~ TAILLE
# # 2 guildes - Large
# car_det_large<- read.csv("car_det_large.csv", header = TRUE, stringsAsFactors = TRUE)
# car_large<- car_det_large[,-1]
# 
# # 2 guildes - Small
# car_det_small<- read.csv("car_det_small.csv", header = TRUE, stringsAsFactors = TRUE)
# car_small<- car_det_small[,-1]
# 
# 
# ## DEUX GUILDES ~ PREY + COMPETITOR
# # 2 guildes - Prey
# car_det_prey<- read.csv("car_det_prey.csv", header = TRUE, stringsAsFactors = TRUE)
# car_prey<- car_det_prey[,-1]
# 
# # 2 guildes - Competitor
# car_det_comp<- read.csv("car_det_comp.csv", header = TRUE, stringsAsFactors = TRUE)
# car_comp<- car_det_comp[,-1]
# 
# 
# 
# ## importation variable de site
# Obs_cov<- read.csv("ObsCov.csv", header = TRUE, stringsAsFactors = TRUE)
# 
# 
# ## importation variable de detection
# #matrice des precipitation en binaire
# precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)
# precip<-precip.3D.bin[,-1]
# 
# # matrice du volume de débris ligneux par site pour mesurer les effets sur la detection
# # données standardisés
# CWD.STD<- read.csv("cwd_tot_std.csv",header = TRUE,stringsAsFactors = TRUE)
# CWD.STD<-CWD.STD[,-1]
# 
# 
# car.data.JAGS<- list(car_all = car_all,
#                      car_for3 = car_for3,
#                      car_open3 = car_open3,
#                      car_aqua3 = car_aqua3,
#                      car_for2 = car_for2,
#                      car_open2 = car_open2,
#                      car_large = car_large,
#                      car_small = car_small,
#                      car_prey = car_prey,
#                      car_comp = car_comp,
#                      precip = precip,
#                      Obs_cov = Obs_cov,
#                      CWD.STD = CWD.STD)
# 
# save(car.data.JAGS, file = "car_data_JAGS.RData")

load("SEM_data_JAGS.RData")

# load("car_data_JAGS.RData")
# str(car.data.JAGS)


## choix du data.frame de detection à utiliser

# ## Tous les carabes
# car <- car.data.JAGS$car_all
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde milieu forestier ( division en trois guildes)
# car <- car.data.JAGS$car_for3
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde milieu ouvert ( division en trois guildes)
# car <- car.data.JAGS$car_open3
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde milieu aquatique ( division en trois guildes)
# car <- car.data.JAGS$car_aqua3
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
#  
#  
# ## Guilde milieu forestier ( division en deux guildes)
# car <- car.data.JAGS$car_for2
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde milieu ouvert ( division en deux guildes)
# car <- car.data.JAGS$car_open2
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde grands carabes ( division en deux guildes)
# car <- car.data.JAGS$car_large
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))
# 
# ## Guilde petits carabes ( division en deux guildes)
# car <- car.data.JAGS$car_small
# mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))

## Guilde proies ( division en deux guildes)
car <- data.JAGS$car_prey[,-1]
mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))

# Guilde competiteurs ( division en deux guildes)
car <- data.JAGS$car_comp[,-1]
mean(apply(car, 1, FUN = function(i) ifelse(sum(i) > 0, 1, 0)))


##occupancy variables
Coupe <- data.JAGS$Obs_cov$Coupe

##detection variables
Precip <- data.JAGS$precip[,-1]
CWD <- data.JAGS$Obs_cov$CWD_STD
BlockOrig <- data.JAGS$Obs_cov$Bloc
##converted to numeric variable
Block <- as.numeric(BlockOrig)

##other info
nsites <- nrow(car)
nvisits <- ncol(car)
nblocks <- length(unique(Block))
ngroups <- length(unique(Coupe))


##################################
##################################
##psi(Cut) p(CWD + Prec + BlockRandomEffect)

modelstring <- "
model {

 ##prior
 #prior for psi
 for(k in 1:ngroups) {
    psi[k] ~ dunif(0, 1)
 }

 #prior for p
 alpha0.car ~ dnorm(0, 0.01)
 alpha.precip.car ~ dnorm(0, 0.01)
 alpha.cwd.car ~ dnorm(0, 0.01)

 ##random effects of block
 for(m in 1:nblocks) {
    alpha.block[m] ~ dnorm(0, tau.block)
 }

 tau.block <- pow(sigma.block, -2)
 sigma.block ~ dunif(0, 10)


 ##likelihood
 ##first loop over nsites
 for (i in 1:nsites) {
    ##True occupancy z at site i (biological process)
    z[i] ~ dbern(psi[Group[i]])

    ##linear predictor of psi
    #logit.psi[i] <- beta0.car + beta.litter.car * Litter[i]
    #psi[i] <- exp(logit.psi[i])/(1 + exp(logit.psi[i]))
    

    ##second loop over nvisits
    for (j in 1:nvisits) {
        ##linear predictor of p
        logit.p[i, j] <- alpha0.car + alpha.cwd.car * CWD[i] + alpha.precip.car * Precip[i, j] + alpha.block[Block[i]]
        p[i, j] <- exp(logit.p[i, j])/(1 + exp(logit.p[i, j]))

        ##p = 0 if site not occupied
        eff.p[i, j] <- z[i] * p[i, j]

        ##detection at i j (observation process)
        y[i, j] ~ dbern(eff.p[i, j])
    
      }
 }

## derived parameter
 ##number of occupied sites among nsites
 finiteOcc <- sum(z[])
}


"
writeLines(modelstring, con = "occ.car-psiCutpCWDPrecBlock2.jags")

## named list
linData.car <- list(y = as.matrix(car), 
                    nsites = nsites,
                    nvisits = nvisits,
                    ngroups = ngroups,
                    CWD = CWD,
                    Precip = as.matrix(Precip),
                    Group = ifelse(Coupe == "temoin", 1,
                            ifelse(Coupe == "partielle", 2,
                                   3)),
                    Block = Block, nblocks = 4)


## remove dimension names
dimnames(linData.car$y) <- NULL
dimnames(linData.car$Precip) <- NULL
str(linData.car)

## use known occupied values
zstart <- apply(car, 1, max)
zstart

## function to assign initial values at each iteration
inits <- function(){
    list(z = zstart,
         psi = runif(ngroups, 0, 1),
         alpha0.car = rnorm(1),
         alpha.precip.car = rnorm(1),
         alpha.cwd.car = rnorm(1),
         alpha.block = rnorm(nblocks),
         sigma.block = runif(1, 0, 10))
}

## vector of names
params <- c("psi",
            "alpha0.car",
            "alpha.precip.car",
            "alpha.cwd.car",
            "alpha.block", "sigma.block",
            "finiteOcc")

## MCMC settings
## chains
nc <- 5

## iterations
ni <- 100000

## burn-in
nb<- 50000

## thinning rate (save 1 obs per 5 iterations)
nt <-5

library(jagsUI)

out.occ.car <- jags(data = linData.car,
                    inits = inits,
                    parameters = params,
                    model = "occ.car-psiCutpCWDPrecBlock.jags",
                    n.thin = nt,
                    n.chains = nc,
                    n.burnin = nb,
                    n.iter = ni,
                    n.adapt = 10000)
# save(out.occ.car, file = "out.occ.car-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")
# save(out.occ.car, file = "out.occ.car-PsiCut-PCWDPrecBlock.17oct23.Rdata") #prey
save(out.occ.car, file = "out.occ.car.comp-PsiCut-PCWDPrecBlock.17oct23.Rdata") #comp
load("out.occ.car-PsiCut-PCWDPrecBlock.17oct23.Rdata") # prey
# load("out.occ.car-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")

print(out.occ.car, 4)

outSumcar <- out.occ.car$summary [,c("mean", "sd", "2.5%", "97.5%", "Rhat")]
round(outSumcar, 4)

##trace plots
jagsUI:::traceplot(out.occ.car, parameters = c("psi"))
jagsUI:::traceplot(out.occ.car, parameters = c("alpha0.car",
                                               "alpha.precip.car",
                                               "alpha.cwd.car"))
jagsUI:::traceplot(out.occ.car, parameters = c("alpha.block", "sigma.block"))

##posterior density plots
par(mfrow = c(1, 1))
plot(density(out.occ.car$sims.list$psi[, 1]),
     main = "psi[1]")
plot(density(out.occ.car$sims.list$psi[, 2]),
     main = "psi[2]")
plot(density(out.occ.car$sims.list$psi[, 3]),
     main = "psi[4]")

plot(density(out.occ.car$sims.list$alpha0.car),
     main = "alpha0.car")
plot(density(out.occ.car$sims.list$alpha.precip.car),
     main = "alpha.precip.car")
plot(density(out.occ.car$sims.list$alpha.cwd.car),
     main = "alpha.cwd.car")
plot(density(out.occ.car$sims.list$sigma.block),
     main = "sigma.block")
plot(density(out.occ.car$sims.list$alpha.block[, 1]),
     main = "alpha.block[1]")
plot(density(out.occ.car$sims.list$alpha.block[, 2]),
     main = "alpha.block[2]")
plot(density(out.occ.car$sims.list$alpha.block[, 3]),
     main = "alpha.block[3]")
plot(density(out.occ.car$sims.list$alpha.block[, 4]),
     main = "alpha.block[4]")


library(coda)
coda.out.car <- summary(out.occ.car$samples)
##MC error across all parameters
Ratio.car <- coda.out.car$statistics[, "Time-series SE"]/coda.out.car$statistics[, "SD"]
Ratio.car
any(Ratio.car > 0.05)



##difference between groups
diff.1v2 <- out.occ.car$sims.list$psi[, 1] -
  out.occ.car$sims.list$psi[, 2]
hist(diff.1v2)
quant1v2 <- quantile(diff.1v2, probs = c(0.025, 0.975))
abline(
  v = quant1v2,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2



diff.1v3 <- out.occ.car$sims.list$psi[, 1] -
  out.occ.car$sims.list$psi[, 3]
hist(diff.1v3)
quant1v3 <- quantile(diff.1v3, probs = c(0.025, 0.975))
abline(
  v = quant1v3,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3

diff.2v3 <- out.occ.car$sims.list$psi[, 2] -
  out.occ.car$sims.list$psi[, 3]
hist(diff.2v3)
quant2v3 <- quantile(diff.2v3, probs = c(0.025, 0.975))
abline(
  v = quant2v3,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3


## graphique

# load("out.occ.prey.100K50Kb.Rdata")
load("out.occ.car-PsiCut-PCWDPrecBlock.17oct23.Rdata")

outSum.car <-
  out.occ.car$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]

# par(mar = c(5.1, 4.1, 4.1, 2.1))  # Adjust margin
par(mfcol = c(1, 1))
par(mar = c(5.1, 5.1, 4.1, 4.1))  # Adjust margin

# essaie sample data
occ_data <- data.frame(
  treatments = c("Témoin", "Partielle", "Totale"),
  mean = c(outSum.car[1:3, 1]),
  lower = c(outSum.car[1:3, 3]),
  upper = c(outSum.car[1:3, 4])
)


# Create a  plot
plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 1),
  main = paste0("Carabes du groupe \"proie de salamandres\""),
  xlab = "Coupes forestières",
  ylab = "Probabilité d'occupation",
  xaxt = "n",
  cex.axis = 1,
  cex.lab = 1)

# # Create a  plot
# plot(
#   NA,
#   xlim = c(0, 4),
#   ylim = c(0, 1),
#   xlab = "Treatments",
#   ylab = "Occupation Probability",
#   xaxt = "n",
#   main = "Comparison of mean occupation probabilities\nbetween different cutting treatments"
# )


# Add lines for each treatment
segments(
  x0 = 1:3,
  y0 = occ_data$lower,
  1:3,
  occ_data$upper,
  col = "black",
  lwd = 2
)

# Add points for mean occupation probabilities
points(
  x = 1:3,
  y = occ_data$mean,
  pch = 19,
  col = "black",
  lwd = 2
)

#par(cex.axis = 2.5)

axis(1, at = 1:3, labels = occ_data$treatments, lwd = 1, padj = 1)

