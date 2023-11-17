#cut ~ litter
#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

#dataset
load("SEM_data_JAGS.RData")

hist(data.JAGS$Obs_cov$litter.depth)

##CWD
Litter <- data.JAGS$Obs_cov$litter.depth
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

##different variance for each group
for(j in 1:3) {
  tau.lit[j] <- pow(sigma[j], -2)
  sigma[j] ~ dunif(0, 150)
}


  ## Likelihood
  for (i in 1:nsites) {
     mu.lit[i] <- beta0 + beta.Cutpartial*Cutpartial[i] + beta.Cutclear*Cutclear[i] + alpha.block[Block[i]]

     Litter[i] ~ dnorm(mu.lit[i], tau.lit[CoupeNum[i]])
  }

##predicted values
for(i in 1:nsites) {
pred[i] <- mu.lit[i]

##raw residuals
res[i] <- Litter[i] - mu.lit[i]
##Pearson residuals
res.pearson[i] <- res[i]/sigma[CoupeNum[i]]

}

}
"
writeLines(modelstring, con = "Litter_Cut-hetVar-block.jags")



## list of data

lin.data <- list(
  Litter = as.numeric(Litter),
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
                  "sigma[1]",
                  "sigma[2]",
                  "sigma[3]"),
                c("mean", "sd", "2.5%", "97.5%", "Rhat")]

##Rhat
hist(out.lit$summary[, "Rhat"])
any(out.lit$summary[, "Rhat"] > 1.1)

par(mfrow = c(2, 3),
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
    out.lit$samples[[1]][, "sigma[1]"],
    out.lit$samples[[2]][, "sigma[1]"],
    out.lit$samples[[3]][, "sigma[1]"],
    out.lit$samples[[4]][, "sigma[1]"],
    out.lit$samples[[5]][, "sigma[1]"]
  ),
  type = "l",
  ylab = "sigma[1]",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.lit$samples[[1]][, "sigma[2]"],
    out.lit$samples[[2]][, "sigma[2]"],
    out.lit$samples[[3]][, "sigma[2]"],
    out.lit$samples[[4]][, "sigma[2]"],
    out.lit$samples[[5]][, "sigma[2]"]
  ),
  type = "l",
  ylab = "sigma[2]",
  xlab = "iteration",
  cex.lab = 1.2
)


matplot(
  cbind(
    out.lit$samples[[1]][, "sigma[3]"],
    out.lit$samples[[2]][, "sigma[3]"],
    out.lit$samples[[3]][, "sigma[3]"],
    out.lit$samples[[4]][, "sigma[3]"],
    out.lit$samples[[5]][, "sigma[3]"]
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
