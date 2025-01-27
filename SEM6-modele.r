##setup for SEM
# setwd("/home/mazerolm/Documents/Supervision/Devos/Data")

##load clean file
load(file = "SEM_data_JAGS.RData")
str(data.JAGS)

data.JAGS$Obs_cov$CWD

###########################################################
###########################################################
####model with CWD, Canopy openness and forest litter vs Cut
##-dominant salamanders and carab competitors
##-carab prey dependent on salamanders,
##-collembola dependent on salamanders and carab competitors

##salamander data
y.sal <- as.matrix(data.JAGS$pcin[, -1])
dimnames(y.sal) <- NULL

##carab competitor
y.car.comp <- as.matrix(data.JAGS$car_comp[, -1])
dimnames(y.car.comp) <- NULL

##carab prey
y.car.prey <- as.matrix(data.JAGS$car_prey[, -1])
dimnames(y.car.prey) <- NULL

##collembola
y.coll <- data.JAGS$coll$weight

##occupancy variables
Coupe <- data.JAGS$Obs_cov$Coupe

##detection variables
CWD <- data.JAGS$Obs_cov$CWD_STD
data.JAGS$Obs_cov$CWD_tot_m3
Precip <- as.matrix(data.JAGS$precip[, -1])
dimnames(Precip) <- NULL
BlockOrig <- data.JAGS$Obs_cov$Bloc
##convert to numeric variable
Block <- as.numeric(BlockOrig)

##other info
nsites <- nrow(y.sal)
nvisits <- ncol(y.sal)
nblocks <- length(unique(Block))
ngroups <- length(unique(Coupe))


## named list
linData <- list(y.sal = y.sal, 
                y.car.comp = y.car.comp,
                y.car.prey = y.car.prey,
                y.coll = y.coll,
                
                nsites = nsites,
                nvisits = nvisits,
                ngroups = ngroups,
                CWD = CWD,
                Canopy = data.JAGS$Obs_cov$canopy/100,
                Litter = data.JAGS$Obs_cov$litter,
                Precip = Precip,
                Group = ifelse(Coupe == "temoin", 1,
                        ifelse(Coupe == "partielle", 2,
                               3)),
                Cutpartial = ifelse(data.JAGS$Obs_cov$Coupe == "partielle", 1, 0),
                Cutclear = ifelse(data.JAGS$Obs_cov$Coupe == "totale", 1, 0),
                Block = Block, nblocks = 4)
str(linData)

## use known occupied values
zstartSal <- apply(y.sal, 1, max)
zstartSal

##carab prey
zstartCar.prey <- apply(y.car.prey, 1, max)
zstartCar.prey

##carab competitor
zstartCar.comp <- apply(y.car.comp, 1, max)
zstartCar.comp


## function to assign initial values at each iteration
inits <- function(){
    list(beta0.cwd = rnorm(1),
         beta.Cutpartial.cwd = rnorm(1),
         beta.Cutclear.cwd = rnorm(1),
         alpha.block.cwd = rnorm(nblocks),
         sigma.block.cwd = runif(1, 0, 10),
         sigma.cwd = runif(3, 0, 10),

         beta0.can = rnorm(1),
         beta.Cutpartial.can = rnorm(1),
         beta.Cutclear.can = rnorm(1),
         alpha.block.can = rnorm(nblocks),
         sigma.block.can = runif(1, 0, 10),
         sigma.can = runif(3, 0, 10),

         beta0.lit = rnorm(1),
         beta.Cutpartial.lit = rnorm(1),
         beta.Cutclear.lit = rnorm(1),
         alpha.block.lit = rnorm(nblocks),
         sigma.block.lit = runif(1, 0, 10),
         sigma.lit = runif(1, 0, 10),

         z.sal = zstartSal,
         psi.sal = runif(ngroups, 0, 1),
         alpha0.sal = rnorm(1),
         alpha.precip.sal = rnorm(1),
         alpha.cwd.sal = rnorm(1),
         alpha.block.sal = rnorm(nblocks),
         sigma.block.sal = runif(1, 0, 10),

         z.car.comp = zstartCar.comp,
         psi.car.comp = runif(3, 0, 1),
         alpha0.car.comp = rnorm(1),
         alpha.precip.car.comp = rnorm(1),
         alpha.cwd.car.comp = rnorm(1),
         alpha.block.car.comp = rnorm(nblocks),
         sigma.block.car.comp = runif(1, 0, 10),

         z.car.prey = zstartCar.prey,
         psi.beta0.car.prey = rnorm(1),
         psi.beta.z.sal.car.prey = rnorm(1),
         psi.beta.Cutpartial.car.prey = rnorm(1),
         psi.beta.Cutclear.car.prey = rnorm(1),
         alpha0.car.prey = rnorm(1),
         alpha.precip.car.prey = rnorm(1),
         alpha.cwd.car.prey = rnorm(1),
         alpha.block.car.prey = rnorm(nblocks),
         sigma.block.car.prey = runif(1, 0, 10),

         beta0.coll = rnorm(1),
         beta.Cutpartial.coll = rnorm(1),
         beta.Cutclear.coll = rnorm(1),
         beta.z.sal.coll = rnorm(1),
         beta.z.car.prey.coll = rnorm(1),
         beta.z.car.comp.coll = rnorm(1),
         alpha.block.coll = rnorm(nblocks),
         sigma.block.coll = runif(1, 0, 10),
         sigma.coll = runif(3, 0, 10))

}

## vector of names
params <- c("beta0.cwd",
            "beta.Cutpartial.cwd",
            "beta.Cutclear.cwd",
            "alpha.block.cwd",
            "sigma.block.cwd",
            "sigma.cwd",
            "res.pearson.cwd",

            "beta0.can",
            "beta.Cutpartial.can",
            "beta.Cutclear.can",
            "alpha.block.can",
            "sigma.block.can",
            "sigma.can",
            "res.pearson.can",

            "beta0.lit",
            "beta.Cutpartial.lit",
            "beta.Cutclear.lit",
            "alpha.block.lit",
            "sigma.block.lit",
            "sigma.lit",
            "res.pearson.lit",

            "psi.sal",
            "alpha0.sal", "alpha.precip.sal",
            "alpha.cwd.sal",
            "alpha.block.sal", "sigma.block.sal",
            "finiteOcc.sal",

            "psi.car.comp", 
            "alpha0.car.comp", "alpha.precip.car.comp",
            "alpha.cwd.car.comp",
            "alpha.block.car.comp", "sigma.block.car.comp",
            "finiteOcc.car.comp",

            "psi.beta0.car.prey", "psi.beta.z.sal.car.prey",
            "psi.beta.Cutpartial.car.prey", "psi.beta.Cutclear.car.prey",
            "alpha0.car.prey", "alpha.precip.car.prey",
            "alpha.cwd.car.prey",
            "alpha.block.car.prey", "sigma.block.car.prey",
            "finiteOcc.car.prey",

            "beta0.coll", 
            "beta.Cutpartial.coll",
            "beta.Cutclear.coll",
            "beta.z.sal.coll",
            "beta.z.car.prey.coll",
            "beta.z.car.comp.coll",
            "alpha.block.coll",
            "sigma.block.coll",
            "sigma.coll",
            "res.pearson")


## MCMC settings
## chains
nc <- 5

## iterations
ni <- 200000

## burn-in
nb <- 75000

## thinning rate (save 1 obs per 5 iterations)
nt <- 5

library(jagsUI)
# out.jags <- jags(data = linData,
                 # inits = inits,
                 # parameters = params,
                 # model = "SEM6.jags",
                 # n.thin = nt,
                 # n.chains = nc,
                 # n.burnin = nb,
                 # n.iter = ni)#,
                 # #parallel = TRUE,
                 # #n.adapt = 10000)

save(out.jags, file = "out.jags.sem6.Rdata")

##check output
print(out.jags, 3)

round(out.jags$summary[, c("mean", "sd", "2.5%", "97.5%", "n.eff")], 2)

round(out.jags$summary[c("beta0.cwd",
                         "beta.Cutpartial.cwd",
                         "beta.Cutclear.cwd",
                         "sigma.block.cwd",
                         #"sigma.cwd",
                         
                         "beta0.can",
                         "beta.Cutpartial.can",
                         "beta.Cutclear.can",
                         "sigma.block.can",
                         #"sigma.can",
                         
                         "beta0.lit",
                         "beta.Cutpartial.lit",
                         "beta.Cutclear.lit",
                         "sigma.block.lit",
                         "sigma.lit",
                         
                         "psi.sal[1]",
                         "psi.sal[2]",
                         "psi.sal[3]",
                         "alpha0.sal", "alpha.precip.sal",
                         "alpha.cwd.sal",
                         "sigma.block.sal",
                         "finiteOcc.sal",
                         
                         "psi.car.comp[1]", "psi.car.comp[3]", "psi.car.comp[3]",
                         "alpha0.car.comp", "alpha.precip.car.comp",
                         "alpha.cwd.car.comp",
                         "sigma.block.car.comp",
                         "finiteOcc.car.comp",
                         
                         "psi.beta0.car.prey", "psi.beta.z.sal.car.prey",
                         "psi.beta.Cutpartial.car.prey", "psi.beta.Cutclear.car.prey",
                         "alpha0.car.prey", "alpha.precip.car.prey",
                         "alpha.cwd.car.prey",
                         "sigma.block.car.prey",
                         "finiteOcc.car.prey",
                         
                         "beta0.coll", 
                         "beta.Cutpartial.coll",
                         "beta.Cutclear.coll",
                         "beta.z.sal.coll",
                         "beta.z.car.prey.coll",
                         "beta.z.car.comp.coll",
                         "sigma.block.coll"),
                       c("mean", "sd", "2.5%", "97.5%", "n.eff")], 2)

round(out.jags$summary[c("sigma.cwd[1]", "sigma.cwd[2]", "sigma.cwd[3]",
                         "sigma.can[1]","sigma.can[2]", "sigma.can[3]",
                         "sigma.lit", 
                         "sigma.coll[1]", "sigma.coll[2]", "sigma.coll[3]"),
                       c("mean", "sd", "2.5%", "97.5%", "n.eff")], 2)

##check for Rhat > 1
any(out.jags$summary[, "Rhat"] > 1.1)
which(out.jags$summary[, "Rhat"] > 1.1)

##check ratio between MCMC error and naive SE
library(coda)
combo <- as.mcmc.list(out.jags$samples)
sumOut <- summary(combo)
ratio <- sumOut$statistics[, "Time-series SE"]/sumOut$statistics[, "SD"]
hist(ratio)
max(ratio)
which(ratio > 0.05)


##check trace plots and posterior density plots
library(mcmcplots)
mcmcplot(out.jags$samples[1:5],
         parms = c("beta0.cwd",
                   "beta.Cutpartial.cwd",
                   "beta.Cutclear.cwd",
                   "sigma.block.cwd",
                   "sigma.cwd[1]",
                   "sigma.cwd[2]",
                   "sigma.cwd[3]",
                   
                   "beta0.can",
                   "beta.Cutpartial.can",
                   "beta.Cutclear.can",
                   "sigma.block.can",
                   "sigma.can[1]",
                   "sigma.can[2]",
                   "sigma.can[3]",
                         
                   "beta0.lit",
                   "beta.Cutpartial.lit",
                   "beta.Cutclear.lit",
                   "sigma.block.lit",
                   "sigma.lit",
                         
                   "psi.sal[1]",
                   "psi.sal[2]",
                   "psi.sal[3]",
                   "alpha0.sal", "alpha.precip.sal",
                   "alpha.cwd.sal",
                   "sigma.block.sal",
                   ##"finiteOcc.sal",
                         
                   "psi.car.comp[1]", "psi.car.comp[3]", "psi.car.comp[3]",
                   "alpha0.car.comp", "alpha.precip.car.comp",
                   "alpha.cwd.car.comp",
                   "sigma.block.car.comp",
                   ##"finiteOcc.car.comp",
                         
                   "psi.beta0.car.prey", "psi.beta.z.sal.car.prey",
                   "psi.beta.Cutpartial.car.prey", "psi.beta.Cutclear.car.prey",
                   "alpha0.car.prey", "alpha.precip.car.prey",
                   "alpha.cwd.car.prey",
                   "sigma.block.car.prey",
                   ##"finiteOcc.car.prey",
                         
                   "beta0.coll", 
                   "beta.Cutpartial.coll",
                   "beta.Cutclear.coll",
                   "beta.z.sal.coll",
                   "beta.z.car.prey.coll",
                   "beta.z.car.comp.coll",
                   "sigma.block.coll",
                   "sigma.coll[1]",
                   "sigma.coll[2]",
                   "sigma.coll[3]"))
