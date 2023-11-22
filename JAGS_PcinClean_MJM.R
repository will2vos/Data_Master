
#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")
#setwd("/home/mazerolm/Documents/Supervision/Devos/Scripts/2023-06-27")

# # importation matrice dedetection pcin
# det_pcin<- read.csv("det_pcin.csv", header = TRUE, stringsAsFactors = TRUE)
# pcin<- det_pcin[,-1]
# 
# # importation variable de site
# Obs_cov<- read.csv("ObsCov.csv", header = TRUE, stringsAsFactors = TRUE)
# 
# # importation variable de detection
# 
# #matrice des precipitation en binaire
# precip.3D.bin<- read.csv("precip.3D.bin.csv",header = TRUE,stringsAsFactors = TRUE)
# precip<-precip.3D.bin[,-1]
# 
# # matrice du volume de débris ligneux par site pour mesurer les effets sur la detection
# # données standardisés
# CWD.STD<- read.csv("cwd_total_std.csv",header = TRUE,stringsAsFactors = TRUE)
# CWD.STD<-CWD.STD[,-1]
# 
# pcin.data.JAGS<- list(pcin = pcin, precip = precip,
#                       Obs_cov = Obs_cov, CWD.STD = CWD.STD)
# 
# save(pcin.data.JAGS, file = "pcin_data_JAGS.RData")

# load("pcin_data_JAGS.RData")
# str(pcin.data.JAGS)

load("SEM_data_JAGS.RData")

##Extract data
pcin <- data.JAGS$pcin[,-1]

##occupancy variables
Coupe <- data.JAGS$Obs_cov$Coupe

##detection variables
CWD <- data.JAGS$Obs_cov$CWD_STD
Precip <- data.JAGS$precip[,-1]
BlockOrig <- data.JAGS$Obs_cov$Bloc
##convert to numeric variable
Block <- as.numeric(BlockOrig)

##other info
nsites <- nrow(pcin)
nvisits <- ncol(pcin)
nblocks <- length(unique(Block))
ngroups <- length(unique(Coupe))


######################################
######################################
##psi(Cut) p(CWD + Prec + Block random effect)

modelstring <- "

model {

 ##prior
 #prior for psi
 for(k in 1:ngroups) {
    psi[k] ~ dunif(0, 1)
 }

 #prior for p
 alpha0.pcin ~ dnorm(0, 0.01)
 alpha.precip.pcin ~ dnorm(0, 0.01)
 alpha.cwd.pcin ~ dnorm(0, 0.01)

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
    #logit.psi[i] <- beta0.pcin + beta.litter.pcin * Litter[i]
    #psi[i] <- exp(logit.psi[i])/(1 + exp(logit.psi[i]))


    ##second loop over nvisits
    for (j in 1:nvisits) {
        ##linear predictor of p
        logit.p[i, j] <- alpha0.pcin + alpha.cwd.pcin * CWD[i] + alpha.precip.pcin * Precip[i, j] + alpha.block[Block[i]]
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
writeLines(modelstring, con = "occ.pcin-psiCutpCWDPrecBlock2.jags")


## named list
linData <- list(y = as.matrix(pcin), 
                nsites = nsites,
                nvisits = nvisits,
                ngroups = ngroups,
                CWD = CWD,
                Precip = as.matrix(Precip),
                Group = ifelse(Coupe == "temoin", 1,
                        ifelse(Coupe == "partielle", 2,
                               3)),
                Block = Block, nblocks = 4)

## remove dimention names
dimnames(linData$y) <- NULL
dimnames(linData$Precip) <- NULL
str(linData)

## use known occupied values
zstart <- apply (pcin, 1, max)
zstart

## function to assign initial values at each iteration
inits <- function(){
    list(z = zstart,
         psi = runif(ngroups, 0, 1),
         alpha0.pcin = rnorm(1),
         alpha.precip.pcin = rnorm(1),
         alpha.cwd.pcin = rnorm(1),
         alpha.block = rnorm(nblocks),
         sigma.block = runif(1, 0, 10))
}

## vector of names
params <- c("psi",
            "alpha0.pcin", "alpha.precip.pcin",
            "alpha.cwd.pcin",
            "alpha.block", "sigma.block",
            "finiteOcc")

## MCMC settings
## chains
nc <- 3#5

## iterations
ni <- 200000

## burn-in
nb <- 150000

## thinning rate (save 1 obs per 5 iterations)
nt <- 10

## call JAGS
library(jagsUI)

out.occ <- jags(data = linData,
                inits = inits,
                parameters = params,
                model = "occ.pcin-psiCutpCWDPrecBlock.jags",
                n.thin = nt,
                n.chains = nc,
                n.burnin = nb,
                n.iter = ni,
                n.adapt = 10000)

#save(out.occ, file = "out.occ.pci-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")
save(out.occ, file = "out.occ.pcin.100K50Kb.17oct23.Rdata")
#load("out.occ.pci-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")

load("out.occ.pcin.100K50Kb.17oct23.Rdata")

## check output
print(out.occ, 3)
outSum <- out.occ$summary [,c("mean", "sd", "2.5%", "97.5%", "Rhat")]
round(outSum, 4)


##difference between groups
diff.1v2 <- out.occ$sims.list$psi[, 1] -
  out.occ$sims.list$psi[, 2]
hist(diff.1v2)
quantile(diff.1v2, probs = c(0.025, 0.975))

diff.1v3 <- out.occ$sims.list$psi[, 1] -
  out.occ$sims.list$psi[, 3]
hist(diff.1v3)
quantile(diff.1v3, probs = c(0.025, 0.975))

diff.2v3 <- out.occ$sims.list$psi[, 2] -
  out.occ$sims.list$psi[, 3]
hist(diff.2v3)
quantile(diff.2v3, probs = c(0.025, 0.975))


##to view some diagnostics
library(mcmcplots)
mcmcplot(out.occ$samples)

##trace plots
jagsUI:::traceplot(out.occ, parameters = c("psi"))
jagsUI:::traceplot(out.occ, parameters = c("alpha0.pcin",
                                           "alpha.precip.pcin",
                                           "alpha.cwd.pcin"))
jagsUI:::traceplot(out.occ, parameters = c("alpha.block", "sigma.block"))

##posterior density plots
par(mfrow = c(1, 1))
plot(density(out.occ$sims.list$psi[, 1]),
     main = "psi[1]")
plot(density(out.occ$sims.list$psi[, 2]),
     main = "psi[2]")
plot(density(out.occ$sims.list$psi[, 3]),
     main = "psi[4]")

plot(density(out.occ$sims.list$alpha0.pcin),
     main = "alpha0.pcin")
plot(density(out.occ$sims.list$alpha.precip.pcin),
     main = "alpha.precip.pcin")
plot(density(out.occ$sims.list$alpha.cwd.pcin),
     main = "alpha.cwd.pcin")
plot(density(out.occ$sims.list$sigma.block),
     main = "sigma.block")
plot(density(out.occ$sims.list$alpha.block[, 1]),
     main = "alpha.block[1]")
plot(density(out.occ$sims.list$alpha.block[, 2]),
     main = "alpha.block[2]")
plot(density(out.occ$sims.list$alpha.block[, 3]),
     main = "alpha.block[3]")
plot(density(out.occ$sims.list$alpha.block[, 4]),
     main = "alpha.block[4]")


library(coda)
coda.out.pcin <- summary(out.occ$samples)
##MC error across all parameters
Ratio.pcin <- with(data = coda.out.pcin,
                   statistics[, "Time-series SE"]/statistics[, "SD"])
Ratio.pcin
any(Ratio.pcin > 0.05)

