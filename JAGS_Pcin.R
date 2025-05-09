
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
#writeLines(modelstring, con = "occ.pcin-psiCutpCWDPrecBlock2.jags")


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
nc <- 5

## iterations
ni <- 200000

## burn-in
nb <- 150000

## thinning rate (save 1 obs per 5 iterations)
nt <- 10

## call JAGS
library(jagsUI)

out.occ.pcin <- jags(data = linData,
                inits = inits,
                parameters = params,
                model = "occ.pcin-psiCutpCWDPrecBlock.jags",
                n.thin = nt,
                n.chains = nc,
                n.burnin = nb,
                n.iter = ni,
                n.adapt = 10000)


# save(out.occ.pcin, file = "out.occ.pcin.200K150Kb.7dec23.Rdata")
load("out.occ.pcin.200K150Kb.7dec23.Rdata")

## check output
print(out.occ.pcin, 3)
outSum.pcin <- out.occ.pcin$summary [,c("mean", "sd", "2.5%", "97.5%", "Rhat")]
round(outSum.pcin, 4)

## Rhat
hist(out.occ.pcin$summary[, "Rhat"])
any(out.occ.pcin$summary[, "Rhat"] > 1.1)

##to view some diagnostics
library(mcmcplots)
mcmcplot(out.occ.pcin$samples)

##trace plots
jagsUI:::traceplot(out.occ.pcin, parameters = c("psi"))
jagsUI:::traceplot(out.occ.pcin, parameters = c("alpha0.pcin",
                                           "alpha.precip.pcin",
                                           "alpha.cwd.pcin"))
jagsUI:::traceplot(out.occ.pcin, parameters = c("alpha.block", "sigma.block"))


## difference between groups
diff.1v2.pcin <- out.occ.pcin$sims.list$psi[, 1] -
  out.occ.pcin$sims.list$psi[, 2]
hist(diff.1v2.pcin)
quant1v2.pcin <- quantile(diff.1v2.pcin, probs = c(0.025, 0.975))
abline(
  v = quant1v2.pcin,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.pcin

diff.1v3.pcin <- out.occ.pcin$sims.list$psi[, 1] -
  out.occ.pcin$sims.list$psi[, 3]
hist(diff.1v3.pcin)
quant1v3.pcin <- quantile(diff.1v3.pcin, probs = c(0.025, 0.975))
abline(
  v = quant1v3.pcin,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.pcin

diff.2v3.pcin <- out.occ.pcin$sims.list$psi[, 2] -
  out.occ.pcin$sims.list$psi[, 3]
hist(diff.2v3.pcin)
quant2v3.pcin <- quantile(diff.2v3.pcin, probs = c(0.025, 0.975))
abline(
  v = quant2v3.pcin,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.pcin


##posterior density plots
par(mfrow = c(1, 1))
plot(density(out.occ.pcin$sims.list$psi[, 1]),
     main = "psi[1]")
plot(density(out.occ.pcin$sims.list$psi[, 2]),
     main = "psi[2]")
plot(density(out.occ.pcin$sims.list$psi[, 3]),
     main = "psi[4]")

plot(density(out.occ.pcin$sims.list$alpha0.pcin),
     main = "alpha0.pcin")
plot(density(out.occ.pcin$sims.list$alpha.precip.pcin),
     main = "alpha.precip.pcin")
plot(density(out.occ.pcin$sims.list$alpha.cwd.pcin),
     main = "alpha.cwd.pcin")
plot(density(out.occ.pcin$sims.list$sigma.block),
     main = "sigma.block")
plot(density(out.occ.pcin$sims.list$alpha.block[, 1]),
     main = "alpha.block[1]")
plot(density(out.occ.pcin$sims.list$alpha.block[, 2]),
     main = "alpha.block[2]")
plot(density(out.occ.pcin$sims.list$alpha.block[, 3]),
     main = "alpha.block[3]")
plot(density(out.occ.pcin$sims.list$alpha.block[, 4]),
     main = "alpha.block[4]")


library(coda)
coda.out.pcin <- summary(out.occ.pcin$samples)

##MC error across all parameters
Ratio.pcin <- with(data = coda.out.pcin,
                   statistics[, "Time-series SE"]/statistics[, "SD"])
Ratio.pcin
any(Ratio.pcin > 0.05)


## graphique

outSum.pcin <-
  out.occ.pcin$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]

# par(mar = c(5.1, 4.1, 4.1, 2.1))  # Adjust margin
par(mfcol = c(1, 1))
par(mar = c(5.1, 5.1, 4.1, 4.1))  # Adjust margin

# essaie sample data
occ_data <- data.frame(
  treatments = c("Témoin", "Partielle", "Totale"),
  mean = c(outSum.pcin[1:3, 1]),
  lower = c(outSum.pcin[1:3, 3]),
  upper = c(outSum.pcin[1:3, 4])
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
