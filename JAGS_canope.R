#cut ~ canop
#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

#dataset
load("SEM_data_JAGS.RData")

hist(data.JAGS$Obs_cov$Canop130)

##CWD
Canope <- data.JAGS$Obs_cov$Canop130
##cutting treatment
Coupe <- data.JAGS$Obs_cov$Coupe

##numeric ID to select groups
CoupeNum <- as.numeric(data.JAGS$Obs_cov$Coupe)
#3=témoin, 1=partielle, 2=totale

BlockOrig <- data.JAGS$Obs_cov$Bloc
##converted to numeric variable
Block <- as.numeric(BlockOrig)

# transform cutting treatment in binary
Cutcontrol <- ifelse(Coupe == "temoin", 1, 0)
Cutpartial <- ifelse(Coupe == "partielle", 1, 0)
Cutclear <- ifelse(Coupe == "totale", 1, 0)

# nsites
nsites <- length(Canope)
nblocks <- length(unique(Block))
#ngroups<-length(unique(Coupe$traitements))


## model CWD ~ dnorm()
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
  tau.cnp[j] <- pow(sigma[j], -2)
  sigma[j] ~ dunif(0, 150)
}


  ## Likelihood
  for (i in 1:nsites) {
     mu.cnp[i] <- beta0 + beta.Cutpartial*Cutpartial[i] + beta.Cutclear*Cutclear[i] + alpha.block[Block[i]]

     Canope[i] ~ dnorm(mu.cnp[i], tau.cnp[CoupeNum[i]])
  }

##predicted values
for(i in 1:nsites) {
pred[i] <- mu.cnp[i]

##raw residuals
res[i] <- Canope[i] - mu.cnp[i]
##Pearson residuals
res.pearson[i] <- res[i]/sigma[CoupeNum[i]]

}

}
"
writeLines(modelstring, con = "Canope_Cut-hetVar-block.jags")



## list of data

lin.data <- list(
  Canope = as.numeric(Canope),
  Cutpartial = as.numeric(Cutpartial),
  Cutclear = as.numeric(Cutclear),
  CoupeNum = CoupeNum,
  nsites = nsites,
  Block = Block,
  nblocks = 4
)
str(lin.data)


# initial values of intercepts
inits <- function() {
  list(
    beta0 = rnorm(1),
    beta.Cutpartial = rnorm(1),
    beta.Cutclear = rnorm(1),
    sigma = rlnorm(3),
    alpha.block = rnorm(nblocks),
    sigma.block = runif(1, 0, 10)
  )
}


# output
params <- c(
  "beta0",
  "beta.Cutpartial",
  "beta.Cutclear",
  "sigma",
  "pred",
  "res",
  "res.pearson",
  "sigma.block"
)

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

out.cnp <- jags(
  data = lin.data,
  inits = inits,
  parameters = params,
  model = "Canope_Cut-hetVar-block.jags",
  n.thin = nt,
  n.chains = nc,
  n.burnin = nb,
  n.iter = ni,
  n.adapt = 10000
)

save(out.cnp, file = "out_canope_100K50Kb_Cut_hetVar_blockRE.Rdata")

print(out.cnp, digits = 3)

out.cnp$summary[c("beta0",
                  "beta.Cutpartial",
                  "beta.Cutclear",
                  "sigma[1]",
                  "sigma[2]",
                  "sigma[3]"),
                c("mean", "sd", "2.5%", "97.5%", "Rhat")]

##Rhat
hist(out.cnp$summary[, "Rhat"])
any(out.cnp$summary[, "Rhat"] > 1.1)

par(mfrow = c(2, 3),
    mar = c(4, 4, 2, 2))

matplot(
  cbind(
    out.cnp$samples[[1]][, "beta0"],
    out.cnp$samples[[2]][, "beta0"],
    out.cnp$samples[[3]][, "beta0"],
    out.cnp$samples[[4]][, "beta0"],
    out.cnp$samples[[5]][, "beta0"]
  ),
  type = "l",
  ylab = "beta0",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cnp$samples[[1]][, "beta.Cutpartial"],
    out.cnp$samples[[2]][, "beta.Cutpartial"],
    out.cnp$samples[[3]][, "beta.Cutpartial"],
    out.cnp$samples[[4]][, "beta.Cutpartial"],
    out.cnp$samples[[5]][, "beta.Cutpartial"]
  ),
  type = "l",
  ylab = "beta.Cutpartial",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cnp$samples[[1]][, "beta.Cutclear"],
    out.cnp$samples[[2]][, "beta.Cutclear"],
    out.cnp$samples[[3]][, "beta.Cutclear"],
    out.cnp$samples[[4]][, "beta.Cutclear"],
    out.cnp$samples[[5]][, "beta.Cutclear"]
  ),
  type = "l",
  ylab = "beta.Cutclear",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cnp$samples[[1]][, "sigma[1]"],
    out.cnp$samples[[2]][, "sigma[1]"],
    out.cnp$samples[[3]][, "sigma[1]"],
    out.cnp$samples[[4]][, "sigma[1]"],
    out.cnp$samples[[5]][, "sigma[1]"]
  ),
  type = "l",
  ylab = "sigma[1]",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cnp$samples[[1]][, "sigma[2]"],
    out.cnp$samples[[2]][, "sigma[2]"],
    out.cnp$samples[[3]][, "sigma[2]"],
    out.cnp$samples[[4]][, "sigma[2]"],
    out.cnp$samples[[5]][, "sigma[2]"]
  ),
  type = "l",
  ylab = "sigma[2]",
  xlab = "iteration",
  cex.lab = 1.2
)


matplot(
  cbind(
    out.cnp$samples[[1]][, "sigma[3]"],
    out.cnp$samples[[2]][, "sigma[3]"],
    out.cnp$samples[[3]][, "sigma[3]"],
    out.cnp$samples[[4]][, "sigma[3]"],
    out.cnp$samples[[5]][, "sigma[3]"]
  ),
  type = "l",
  ylab = "sigma[3]",
  xlab = "iteration",
  cex.lab = 1.2
)


##using coda package for additional diagnostics
library(coda)
##convert to mcmc.list( ) for coda
outmc <- mcmc.list(
  out.cnp$samples[[1]],
  out.cnp$samples[[2]],
  out.cnp$samples[[3]],
  out.cnp$samples[[4]],
  out.cnp$samples[[5]]
)

##save summary in object
coda.out <- summary(outmc)
range(coda.out$statistics[, "Time-series SE"] / coda.out$statistics[, "SD"])



## graphic CWD ~ cut

outSum.cnp <-
  out.cnp$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]
outSum.cnp

# sample data
cnp_data <- data.frame(
  treatments = c("Témoin", "Partielle", "Totale"),
  mean = c(outSum.cnp[1:3, 1]),
  lower = c(outSum.cnp[1:3, 3]),
  upper = c(outSum.cnp[1:3, 4])
)

cnp_data

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))


# Create a  plot
plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 60),
  main = "Ouverture de la canopé",
  xlab = "Coupes forestières",
  ylab = "Ouverture (%)",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

# Add lines for each treatment
segments(
  x0 = 1:3,
  y0 = cnp_data$lower,
  1:3,
  cnp_data$upper,
  col = "black",
  lwd = 2
)

# Add points for mean occupation probabilities
points(
  x = 1:3,
  y = cnp_data$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

axis(
  1,
  at = 1:3,
  labels = cnp_data$treatments,
  lwd = 1,
  padj = 0.5
)

##graphics to compare groups, IC 95%

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))

diff.1v2 <- out.cnp$sims.list$beta0 -
  out.cnp$sims.list$beta.Cutpartial
hist(diff.1v2,
     main = "Différence des distributions de postérieurs \npour l'ouverture de la canopé",
     xlab = "Témoin vs coupe partielle",
     ylab = "Fréquence")
quant1v2 <- quantile(diff.1v2, probs = c(0.025, 0.975))
abline(
  v = quant1v2,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2

diff.1v3 <- out.cnp$sims.list$beta0 -
  out.cnp$sims.list$beta.Cutclear
hist(diff.1v3,
     main = "Différence des distributions de postérieurs \npour l'ouverture de la canopé",
     xlab = "Témoin vs coupe totale",
     ylab = "Fréquence")
quant1v3 <- quantile(diff.1v3, probs = c(0.025, 0.975))
abline(
  v = quant1v3,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3

diff.2v3 <- out.cnp$sims.list$beta.Cutpartial -
  out.cnp$sims.list$beta.Cutclear
hist(diff.2v3,
     main = "Différence des distributions de postérieurs, \npour l'ouverture de la canopé",
     xlab = "Coupe partielle vs coupe totale",
     ylab = "Fréquence")
quant2v3 <- quantile(diff.2v3, probs = c(0.025, 0.975))
abline(
  v = quant2v3,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3

