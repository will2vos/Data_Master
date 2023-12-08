
# importation du jeu de données
load("SEM_data_JAGS.RData")

## Guilde proies ( division en deux guildes)
car <- data.JAGS$car_prey[,-1]

# Guilde competiteurs ( division en deux guildes)
car <- data.JAGS$car_comp[,-1]

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
#writeLines(modelstring, con = "occ.car-psiCutpCWDPrecBlock2.jags")

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
ni <- 200000

## burn-in
nb <- 150000

## thinning rate (save 1 obs per 5 iterations)
nt <-10

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

## out.occ.car prey
# save(out.occ.car, file = "out.occ.car.prey-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")
load("out.occ.car.prey-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")

## out.occ.car competitor
# save(out.occ.car, file = "out.occ.car.comp-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")
load("out.occ.car.prey-PsiCut-PCWDPrecBlock-200K150Kb.Rdata")

print(out.occ.car, 4)

outSumcar <- out.occ.car$summary [,c("mean", "sd", "2.5%", "97.5%", "Rhat")]
round(outSumcar, 4)

##to view some diagnostics
library(mcmcplots)
mcmcplot(out.occ.car$samples)

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
diff.1v2.car <- out.occ.car$sims.list$psi[, 1] -
  out.occ.car$sims.list$psi[, 2]
hist(diff.1v2.car)
quant1v2.car <- quantile(diff.1v2.car, probs = c(0.025, 0.975))
abline(
  v = quant1v2.car,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.car



diff.1v3.car <- out.occ.car$sims.list$psi[, 1] -
  out.occ.car$sims.list$psi[, 3]
hist(diff.1v3.car)
quant1v3.car <- quantile(diff.1v3.car, probs = c(0.025, 0.975))
abline(
  v = quant1v3.car,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.car

diff.2v3.car <- out.occ.car$sims.list$psi[, 2] -
  out.occ.car$sims.list$psi[, 3]
hist(diff.2v3.car)
quant2v3.car <- quantile(diff.2v3.car, probs = c(0.025, 0.975))
abline(
  v = quant2v3.car,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.car


## graphique

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

