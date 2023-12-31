
# importation du jeu de données
load("SEM_data_JAGS.RData")

##collembola biomass
Biomasse <- data.JAGS$coll$weight

##cutting treatment
Coupe <- data.JAGS$Obs_cov$Coupe

##numeric ID to select groups
CoupeNum <- as.numeric(data.JAGS$Obs_cov$Coupe)
#3=témoin, 1=partielle, 2=totale

BlockOrig <- data.JAGS$Obs_cov$Bloc
##converted to numeric variable
Block <- as.numeric(BlockOrig)

# transform cutting treatment in binary
Cutcontrol<- ifelse(Coupe == "temoin", 1, 0)
Cutpartial<- ifelse(Coupe == "partielle", 1, 0)
Cutclear<- ifelse(Coupe == "totale", 1, 0)

# nsites
nsites <- length(Biomasse)
nblocks <- length(unique(Block))
#ngroups<-length(unique(Coupe$traitements))


## model biomass ~ dnorm()
modelstring <- "
model {
  ## Priors
  beta0 ~ dnorm(0, 0.01)
  beta.Cutpartial ~ dnorm(0, 0.01)
  beta.Cutclear ~ dnorm(0, 0.01)

##random effects of block
for(m in 1:nblocks) {
  alpha.block[m] ~ dnorm(0, tau.block)
}

tau.block <- pow(sigma.block, -2)
sigma.block ~ dunif(0, 10)

##different variance for each group
for(j in 1:3) {
  tau[j] <- pow(sigma[j], -2)
  sigma[j] ~ dunif(0, 150)
}


  ## Likelihood
  for (i in 1:nsites) {
     mu[i] <- beta0 + beta.Cutpartial*Cutpartial[i] + beta.Cutclear*Cutclear[i] + alpha.block[Block[i]]

     Biomasse[i] ~ dnorm(mu[i], tau[CoupeNum[i]])
  }

##predicted values
for(i in 1:nsites) {
pred[i] <- mu[i]

##raw residuals
res[i] <- Biomasse[i] - mu[i]
##Pearson residuals
res.pearson[i] <- res[i]/sigma[CoupeNum[i]]

}

}
"
writeLines(modelstring, con = "Coll_BioMasse_Cut-hetVar-block.jags")



## list of data
lin.data <- list(
  Biomasse = as.numeric(Biomasse),
  Cutpartial = as.numeric(Cutpartial),
  Cutclear = as.numeric(Cutclear),
  CoupeNum = CoupeNum,
  nsites = nsites,
  Block = Block, nblocks = 4
)
str(lin.data)


# initial values of intercepts
inits <- function( ){
  list(beta0 = rnorm(1),
       beta.Cutpartial = rnorm(1),
       beta.Cutclear = rnorm(1),
       sigma = rlnorm(3),
      alpha.block = rnorm(nblocks),
      sigma.block = runif(1, 0, 10))
}


# output
params <- c("beta0", 
            "beta.Cutpartial",
            "beta.Cutclear",
            "sigma",
            "pred", "res", "res.pearson", "sigma.block")

## MCMC settings
## chains
nc <- 5

## iterations
ni <- 100000

## burn-in
nb <- 50000

## thinning rate (save 1 obs per 5 iterations)
nt <- 5

library(jagsUI)

out.coll <- jags(data = lin.data,
                 inits = inits,
                 parameters = params,
                 model = "Coll_BioMasse_Cut-hetVar-block.jags",
                 n.thin = nt,
                 n.chains = nc,
                 n.burnin = nb,
                 n.iter = ni,
                 n.adapt = 10000)






save(out.coll, file = "out_coll_100K50Kb_BMxCut_hetVar_blockRE.Rdata")
load("out_coll_100K50Kb_BMxCut_hetVar_blockRE.Rdata")

print(out.coll, digits = 3)

out.coll$summary[c("beta0", 
                   "beta.Cutpartial",
                   "beta.Cutclear",
                   "sigma[1]", "sigma[2]", "sigma[3]"), 
                 c("mean", "sd", "2.5%", "97.5%", "Rhat")]

##Rhat
hist(out.coll$summary[, "Rhat"])
any(out.coll$summary[, "Rhat"] > 1.1)


##to view some diagnostics
library(mcmcplots)
mcmcplot(out.coll$samples)

##trace plots
jagsUI:::traceplot(out.coll, parameters = c("beta0","beta.Cutpartial",
                                            "beta.Cutclear"))
jagsUI:::traceplot(out.coll, parameters = c("sigma[1]","sigma[2]","sigma[3]"))



# par(mfrow = c(2, 3), 
#     mar = c(4, 4, 2, 2))
# 
# matplot(cbind(out.coll$samples[[1]][, "beta0"], 
#               out.coll$samples[[2]][, "beta0"], 
#               out.coll$samples[[3]][, "beta0"],
#               out.coll$samples[[4]][, "beta0"],
#               out.coll$samples[[5]][, "beta0"]),
#         type = "l", 
#         ylab = "beta0", xlab = "iteration", cex.lab = 1.2)
# 
# matplot(cbind(out.coll$samples[[1]][, "beta.Cutpartial"], 
#               out.coll$samples[[2]][, "beta.Cutpartial"], 
#               out.coll$samples[[3]][, "beta.Cutpartial"],
#               out.coll$samples[[4]][, "beta.Cutpartial"],
#               out.coll$samples[[5]][, "beta.Cutpartial"]),
#         type = "l", 
#         ylab = "beta.Cutpartial", xlab = "iteration", cex.lab = 1.2)
# 
# matplot(cbind(out.coll$samples[[1]][, "beta.Cutclear"], 
#               out.coll$samples[[2]][, "beta.Cutclear"], 
#               out.coll$samples[[3]][, "beta.Cutclear"],
#               out.coll$samples[[4]][, "beta.Cutclear"],
#               out.coll$samples[[5]][, "beta.Cutclear"]),
#         type = "l", 
#         ylab = "beta.Cutclear", xlab = "iteration", cex.lab = 1.2)
# 
# matplot(cbind(out.coll$samples[[1]][, "sigma[1]"], 
#               out.coll$samples[[2]][, "sigma[1]"], 
#               out.coll$samples[[3]][, "sigma[1]"],
#               out.coll$samples[[4]][, "sigma[1]"],
#               out.coll$samples[[5]][, "sigma[1]"]),
#         type = "l", 
#         ylab = "sigma[1]", xlab = "iteration", cex.lab = 1.2)
# 
# matplot(cbind(out.coll$samples[[1]][, "sigma[2]"], 
#               out.coll$samples[[2]][, "sigma[2]"], 
#               out.coll$samples[[3]][, "sigma[2]"],
#               out.coll$samples[[4]][, "sigma[2]"],
#               out.coll$samples[[5]][, "sigma[2]"]),
#         type = "l", 
#         ylab = "sigma[2]", xlab = "iteration", cex.lab = 1.2)
# 
# 
# matplot(cbind(out.coll$samples[[1]][, "sigma[3]"], 
#               out.coll$samples[[2]][, "sigma[3]"], 
#               out.coll$samples[[3]][, "sigma[3]"],
#               out.coll$samples[[4]][, "sigma[3]"],
#               out.coll$samples[[5]][, "sigma[3]"]),
#         type = "l", 
#         ylab = "sigma[3]", xlab = "iteration", cex.lab = 1.2)


##using coda package for additional diagnostics
library(coda)
##convert to mcmc.list( ) for coda
outmc <- mcmc.list(out.coll$samples[[1]],
                   out.coll$samples[[2]],
                   out.coll$samples[[3]],
                   out.coll$samples[[4]],
                   out.coll$samples[[5]])


##save summary in object
coda.out <- summary(outmc)
range(coda.out$statistics[, "Time-series SE"]/coda.out$statistics[, "SD"])

##graphics to compare groups, IC 95%

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))

diff.1v2.coll <- out.coll$sims.list$beta0 -
  out.coll$sims.list$beta.Cutpartial
hist(diff.1v2.coll, main = "Différence des distributions de postérieurs, \npour la probabilité d'occupation des collemboles",
     xlab = "Témoin vs coupe partielle",
     ylab = "Fréquence"
)
quant1v2.coll <- quantile(diff.1v2.coll, probs = c(0.025, 0.975))
abline(
  v = quant1v2.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.coll

diff.1v3.coll <- out.coll$sims.list$beta0 -
  out.coll$sims.list$beta.Cutclear
hist(diff.1v3.coll, main = "Différence des distributions de postérieurs, \npour la probabilité d'occupation des collemboles",
     xlab = "Témoin vs coupe totale",
     ylab = "Fréquence")
quant1v3.coll <- quantile(diff.1v3.coll, probs = c(0.025, 0.975))
abline(
  v = quant1v3.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.coll

diff.2v3.coll <- out.coll$sims.list$beta.Cutpartial -
  out.coll$sims.list$beta.Cutclear
hist(diff.2v3.coll, main = "Différence des distributions de postérieurs, \npour la probabilité d'occupation des collemboles",
     xlab = "Coupe partielle vs coupe totale",
     ylab = "Fréquence")
quant2v3.coll <- quantile(diff.2v3.coll, probs = c(0.025, 0.975))
abline(
  v = quant2v3.coll,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.coll


## graphic biomass ~ cut

outSum.coll <-
  out.coll$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]

# sample data
coll_data <- data.frame(
  treatments = c("Témoin", "Coupe partielle", "Coupe totale"),
  mean = c(outSum.coll[1:3, 1]),
  lower = c(outSum.coll[1:3, 3]),
  upper = c(outSum.coll[1:3, 4])
)

coll_data

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))


# # Create a  plot
# plot(
#   NA,
#   xlim = c(0, 4),
#   ylim = c(0, 40),
#   main = "Collemboles",
#   xlab = "Coupes forestières",
#   ylab = expression(paste("Biomasse (", mu, "g)")),
#   xaxt = "n",
#   cex.axis = 1.2,
#   cex.lab = 1.2,
#   cex.main = 1.5)
# 
# # Add lines for each treatment
# segments(
#   x0 = 1:3,
#   y0 = coll_data$lower,
#   1:3,
#   coll_data$upper,
#   col = "black",
#   lwd = 2
# )
# 
# # Add points for mean occupation probabilities
# points(
#   x = 1:3,
#   y = coll_data$mean,
#   pch = 19,
#   col = "black",
#   lwd = 4
# )
# 
# axis(1, at = 1:3, labels = coll_data$treatments, lwd = 1, padj = 0.5)



#distribution des postérieur

par(mfcol = c(2, 3), 
    mar = c(4, 4, 4, 2))

#beta0
hist(out.coll$sims.list$beta0,
     xlab = "beta0", 
     main = "Posterior density")

#beta.Cutpartial
hist(out.coll$sims.list$beta.Cutpartial,
     xlab = "beta.Cutpartial", 
     main = "Posterior density")

#beta.Cutclear
hist(out.coll$sims.list$beta.Cutclear,
     xlab = "beta.Cutclear", 
     main = "Posterior density")

hist(out.coll$sims.list$sigma[, 1],
     xlab = "sigma[1]", 
     main = "Posterior density")

hist(out.coll$sims.list$sigma[, 2],
     xlab = "sigma[2]", 
     main = "Posterior density")

hist(out.coll$sims.list$sigma[, 3],
     xlab = "sigma[3]", 
     main = "Posterior density")


##check diagnostics
res <- out.coll$summary[paste("res[", 1:nsites, "]", sep = ""), "mean"]
res.pearson <- out.coll$summary[paste("res.pearson[", 1:nsites, "]", sep = ""), "mean"]
pred <- out.coll$summary[paste("pred[", 1:nsites, "]", sep = ""), "mean"]
qqnorm(res)
qqline(res)
##raw residuals - evidence of heteroskedasticity
plot(res ~ pred)
##Pearson standardized residuals suggest variance differs across groups
plot(res.pearson ~ pred)



## graphique

outSum.coll <-
  out.coll$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]
outSum.coll

# sample data
coll_data <- data.frame(
  treatments = c("Control", "Partial-Cut", "Clear Cut"),
  mean = c(outSum.coll[1:3, 1]),
  lower = c(outSum.coll[1:3, 3]),
  upper = c(outSum.coll[1:3, 4])
)

coll_data

par(mfcol = c(1, 1),
        mar = c(5.1, 5.1, 4.1, 4.1))

# Create a  plot
plot(
  NA,
  xlim = c(0, 4),
  ylim = c(-15, 40),
  main = "Collemboles",
  xlab = "Coupes forestières",
  ylab = "Biomasse",
  xaxt = "n",
  cex.axis = 1,
  cex.lab = 1)

# Add lines for each treatment
segments(
  x0 = 1:3,
  y0 = coll_data$lower,
  1:3,
  coll_data$upper,
  col = "black",
  lwd = 2
)

# Add points for mean occupation probabilities
points(
  x = 1:3,
  y = coll_data$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

axis(1, at = 1:3, labels = coll_data$treatments, lwd = 1, padj = 0.5)


