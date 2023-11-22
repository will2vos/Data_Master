#cut ~ litter
#positionnement
# setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

#dataset
load("SEM_data_JAGS.RData")

hist(data.JAGS$Obs_cov$litter.depth)

##CWD
Litter <- data.JAGS$Obs_cov$litter.depth
##cutting treatment
Coupe <- data.JAGS$Obs_cov$Coupe

##numeric ID to select groups
#CoupeNum <- as.numeric(data.JAGS$Obs_cov$Coupe)
#3=témoin, 1=partielle, 2=totale

BlockOrig <- data.JAGS$Obs_cov$Bloc
##converted to numeric variable
Block <- as.numeric(BlockOrig)

# transform cutting treatment in binary
Cutcontrol <- ifelse(Coupe == "temoin", 1, 0)
Cutpartial <- ifelse(Coupe == "partielle", 1, 0)
Cutclear <- ifelse(Coupe == "totale", 1, 0)

# nsites
nsites <- length(Litter)
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


##prior for residual variance
sigma ~ dunif(0, 10)
tau.lit <- 1/(sigma * sigma)


  ## Likelihood
  for (i in 1:nsites) {
     mu.lit[i] <- beta0 + beta.Cutpartial*Cutpartial[i] + beta.Cutclear*Cutclear[i] + alpha.block[Block[i]]

     Litter[i] ~ dnorm(mu.lit[i], tau.lit)
  }

## Derived values
for (i in 1:nsites) {
   res[i] <- Litter[i] - mu.lit[i]
   pred[i] <- mu.lit[i] #linear predictor
}

## Derived values
#  res.pearson[i] <- res[i]/sigma[i]


}


"
writeLines(modelstring, con = "Litter_Cut-hetVar-block.jags")



## list of data

lin.data <- list(
  Litter = as.numeric(Litter),
  Cutpartial = as.numeric(Cutpartial),
  Cutclear = as.numeric(Cutclear),
  nsites = nsites,
  Block = Block,
  nblocks = 4
)
str(lin.data)

# CoupeNum = CoupeNum,


# initial values of intercepts
inits <- function() {
  list(
    beta0 = rnorm(1),
    beta.Cutpartial = rnorm(1),
    beta.Cutclear = rnorm(1),
    sigma = rlnorm(1),
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

out.lit <- jags(
  data = lin.data,
  inits = inits,
  parameters = params,
  model = "Litter_Cut-hetVar-block.jags",
  n.thin = nt,
  n.chains = nc,
  n.burnin = nb,
  n.iter = ni,
  n.adapt = 10000
)

save(out.lit, file = "out_litter_100K50Kb_Cut_hetVar_blockRE.Rdata")

print(out.lit, digits = 3)

out.lit$summary[c("beta0",
                  "beta.Cutpartial",
                  "beta.Cutclear",
                  "sigma"),
                c("mean", "sd", "2.5%", "97.5%", "Rhat")]

##Rhat
hist(out.lit$summary[, "Rhat"])
any(out.lit$summary[, "Rhat"] > 1.1)

par(mfrow = c(2, 2),
    mar = c(4, 4, 2, 2))

matplot(
  cbind(
    out.lit$samples[[1]][, "beta0"],
    out.lit$samples[[2]][, "beta0"],
    out.lit$samples[[3]][, "beta0"],
    out.lit$samples[[4]][, "beta0"],
    out.lit$samples[[5]][, "beta0"]
  ),
  type = "l",
  ylab = "beta0",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.lit$samples[[1]][, "beta.Cutpartial"],
    out.lit$samples[[2]][, "beta.Cutpartial"],
    out.lit$samples[[3]][, "beta.Cutpartial"],
    out.lit$samples[[4]][, "beta.Cutpartial"],
    out.lit$samples[[5]][, "beta.Cutpartial"]
  ),
  type = "l",
  ylab = "beta.Cutpartial",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.lit$samples[[1]][, "beta.Cutclear"],
    out.lit$samples[[2]][, "beta.Cutclear"],
    out.lit$samples[[3]][, "beta.Cutclear"],
    out.lit$samples[[4]][, "beta.Cutclear"],
    out.lit$samples[[5]][, "beta.Cutclear"]
  ),
  type = "l",
  ylab = "beta.Cutclear",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.lit$samples[[1]][, "sigma"],
    out.lit$samples[[2]][, "sigma"],
    out.lit$samples[[3]][, "sigma"],
    out.lit$samples[[4]][, "sigma"],
    out.lit$samples[[5]][, "sigma"]
  ),
  type = "l",
  ylab = "sigma",
  xlab = "iteration",
  cex.lab = 1.2
)



##using coda package for additional diagnostics
library(coda)
##convert to mcmc.list( ) for coda
outmc <- mcmc.list(
  out.lit$samples[[1]],
  out.lit$samples[[2]],
  out.lit$samples[[3]],
  out.lit$samples[[4]],
  out.lit$samples[[5]]
)

##save summary in object
coda.out <- summary(outmc)
range(coda.out$statistics[, "Time-series SE"] / coda.out$statistics[, "SD"])



## graphic CWD ~ cut

outSum.lit <-
  out.lit$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]
outSum.lit




#```{r, echo=FALSE}

# sample data
lit_data <- data.frame(
  treatments = c("Témoin", "Partielle", "Totale"),
  mean = c(outSum.lit[1:3, 1]),
  lower = c(outSum.lit[1:3, 3]),
  upper = c(outSum.lit[1:3, 4])
)

lit_data
##graphics to compare groups, IC 95%

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))

diff.1v2.lit <- out.lit$sims.list$beta0 -
  out.lit$sims.list$beta.Cutpartial
hist(diff.1v2.lit,
     main = "Différence des distributions de postérieurs \npour la profondeur de litière",
     xlab = "Témoin vs coupe partielle",
     ylab = "Fréquence")
quant1v2.lit <- quantile(diff.1v2.lit, probs = c(0.025, 0.975))
abline(
  v = quant1v2.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.lit

diff.1v3.lit <- out.lit$sims.list$beta0 -
  out.lit$sims.list$beta.Cutclear
hist(diff.1v3.lit,
     main = NA,
     xlab = "Témoin vs coupe totale",
     ylab = "Fréquence")
quant1v3.lit <- quantile(diff.1v3.lit, probs = c(0.025, 0.975))
abline(
  v = quant1v3.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.lit

diff.2v3.lit <- out.lit$sims.list$beta.Cutpartial -
  out.lit$sims.list$beta.Cutclear
hist(diff.2v3.lit,
     main = NA,
     xlab = "Coupe partielle vs coupe totale",
     ylab = "Fréquence")
quant2v3.lit <- quantile(diff.2v3.lit, probs = c(0.025, 0.975))
abline(
  v = quant2v3.lit,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.lit

#```

#```{r, echo=FALSE}
par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))


# Create a  plot
plot(
  NA,
  xlim = c(0, 4),
  ylim = c(-5, 010),
  main = "Profondeur de litière par coupe forestières",
  xlab = "Coupes forestières",
  ylab = "Profondeur (cm)",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

# Add lines for each treatment
segments(
  x0 = 1:3,
  y0 = lit_data$lower,
  1:3,
  lit_data$upper,
  col = "black",
  lwd = 2
)

# Add points for mean occupation probabilities
points(
  x = 1:3,
  y = lit_data$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

axis(
  1,
  at = 1:3,
  labels = lit_data$treatments,
  lwd = 1,
  padj = 0.5
)

#```
