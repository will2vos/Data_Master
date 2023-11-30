#cut ~ CWD

#dataset
load("SEM_data_JAGS.RData")

hist(data.JAGS$Obs_cov$CWD)

##CWD
CWD <- data.JAGS$Obs_cov$CWD
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
nsites <- length(CWD)
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
  tau.cwd[j] <- pow(sigma[j], -2)
  sigma[j] ~ dunif(0, 150)
}


  ## Likelihood
  for (i in 1:nsites) {
     mu.cwd[i] <- beta0 + beta.Cutpartial*Cutpartial[i] + beta.Cutclear*Cutclear[i] + alpha.block[Block[i]]

     CWD[i] ~ dnorm(mu.cwd[i], tau.cwd[CoupeNum[i]])
  }

##predicted values
for(i in 1:nsites) {
pred[i] <- mu.cwd[i]

##raw residuals
res[i] <- CWD[i] - mu.cwd[i]
##Pearson residuals
res.pearson[i] <- res[i]/sigma[CoupeNum[i]]

}

}
"
writeLines(modelstring, con = "CWD_Cut-hetVar-block.jags")



## list of data

lin.data <- list(
  CWD = as.numeric(CWD),
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

out.cwd <- jags(
  data = lin.data,
  inits = inits,
  parameters = params,
  model = "CWD_Cut-hetVar-block.jags",
  n.thin = nt,
  n.chains = nc,
  n.burnin = nb,
  n.iter = ni,
  n.adapt = 10000
)

save(out.cwd, file = "out_cwd_100K50Kb_Cut_hetVar_blockRE.Rdata")

print(out.cwd, digits = 3)

out.cwd$summary[c("beta0",
                  "beta.Cutpartial",
                  "beta.Cutclear",
                  "sigma[1]",
                  "sigma[2]",
                  "sigma[3]"),
                c("mean", "sd", "2.5%", "97.5%", "Rhat")]

##Rhat
hist(out.cwd$summary[, "Rhat"])
any(out.cwd$summary[, "Rhat"] > 1.1)

par(mfrow = c(2, 3),
    mar = c(4, 4, 2, 2))

matplot(
  cbind(
    out.cwd$samples[[1]][, "beta0"],
    out.cwd$samples[[2]][, "beta0"],
    out.cwd$samples[[3]][, "beta0"],
    out.cwd$samples[[4]][, "beta0"],
    out.cwd$samples[[5]][, "beta0"]
  ),
  type = "l",
  ylab = "beta0",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cwd$samples[[1]][, "beta.Cutpartial"],
    out.cwd$samples[[2]][, "beta.Cutpartial"],
    out.cwd$samples[[3]][, "beta.Cutpartial"],
    out.cwd$samples[[4]][, "beta.Cutpartial"],
    out.cwd$samples[[5]][, "beta.Cutpartial"]
  ),
  type = "l",
  ylab = "beta.Cutpartial",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cwd$samples[[1]][, "beta.Cutclear"],
    out.cwd$samples[[2]][, "beta.Cutclear"],
    out.cwd$samples[[3]][, "beta.Cutclear"],
    out.cwd$samples[[4]][, "beta.Cutclear"],
    out.cwd$samples[[5]][, "beta.Cutclear"]
  ),
  type = "l",
  ylab = "beta.Cutclear",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cwd$samples[[1]][, "sigma[1]"],
    out.cwd$samples[[2]][, "sigma[1]"],
    out.cwd$samples[[3]][, "sigma[1]"],
    out.cwd$samples[[4]][, "sigma[1]"],
    out.cwd$samples[[5]][, "sigma[1]"]
  ),
  type = "l",
  ylab = "sigma[1]",
  xlab = "iteration",
  cex.lab = 1.2
)

matplot(
  cbind(
    out.cwd$samples[[1]][, "sigma[2]"],
    out.cwd$samples[[2]][, "sigma[2]"],
    out.cwd$samples[[3]][, "sigma[2]"],
    out.cwd$samples[[4]][, "sigma[2]"],
    out.cwd$samples[[5]][, "sigma[2]"]
  ),
  type = "l",
  ylab = "sigma[2]",
  xlab = "iteration",
  cex.lab = 1.2
)


matplot(
  cbind(
    out.cwd$samples[[1]][, "sigma[3]"],
    out.cwd$samples[[2]][, "sigma[3]"],
    out.cwd$samples[[3]][, "sigma[3]"],
    out.cwd$samples[[4]][, "sigma[3]"],
    out.cwd$samples[[5]][, "sigma[3]"]
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
  out.cwd$samples[[1]],
  out.cwd$samples[[2]],
  out.cwd$samples[[3]],
  out.cwd$samples[[4]],
  out.cwd$samples[[5]]
)

##save summary in object
coda.out <- summary(outmc)
range(coda.out$statistics[, "Time-series SE"] / coda.out$statistics[, "SD"])



##graphics to compare groups, IC 95%

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))

diff.1v2.cwd <- out.cwd$sims.list$beta0 -
  out.cwd$sims.list$beta.Cutpartial
hist(diff.1v2.cwd,
     main = "Différence des distributions de postérieurs \npour le volume de débris ligneux",
     xlab = "Témoin vs coupe partielle",
     ylab = "Fréquence")
quant1v2.cwd <- quantile(diff.1v2.cwd, probs = c(0.025, 0.975))
abline(
  v = quant1v2.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v2.cwd

diff.1v3.cwd <- out.cwd$sims.list$beta0 -
  out.cwd$sims.list$beta.Cutclear
hist(diff.1v3.cwd,
     main = NA,
     xlab = "Témoin vs coupe totale",
     ylab = "Fréquence")
quant1v3.cwd <- quantile(diff.1v3.cwd, probs = c(0.025, 0.975))
abline(
  v = quant1v3.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant1v3.cwd

diff.2v3.cwd <- out.cwd$sims.list$beta.Cutpartial -
  out.cwd$sims.list$beta.Cutclear
hist(diff.2v3.cwd,
     main = NA,
     xlab = "Coupe partielle vs coupe totale",
     ylab = "Fréquence")
quant2v3.cwd <- quantile(diff.2v3.cwd, probs = c(0.025, 0.975))
abline(
  v = quant2v3.cwd,
  lty = 2,
  col = "red",
  lwd = 3
)
quant2v3.cwd



## graphic CWD ~ cut

outSum.cwd <-
  out.cwd$summary [, c("mean", "sd", "2.5%", "97.5%", "Rhat")]
outSum.cwd

# sample data
cwd_data_sum <- data.frame(
  mean = c(outSum.cwd[1:3, 1]),
  lower = c(outSum.cwd[1:3, 3]),
  upper = c(outSum.cwd[1:3, 4])
)

cwd_data<- as.data.frame(cwd_data_sum[1,])
cwd_data[2,]<- cwd_data_sum[1,]+cwd_data_sum[2,]
cwd_data[3,]<- cwd_data_sum[1,]+cwd_data_sum[3,]

row.names(cwd_data)<- row.names(cwd_data_sum)

cwd_data$treatments<- c("Témoin", "Partielle", "Totale")
cwd_data

par(mfcol = c(1, 1),
    mar = c(5.1, 5.1, 4.1, 4.1))


# Create a  plot
plot(
  NA,
  xlim = c(0, 4),
  ylim = c(0, 25),
  main = "Collemboles",
  xlab = "Coupes forestières",
  ylab = "CWD (m3)",
  xaxt = "n",
  cex.axis = 1.2,
  cex.lab = 1.2,
  cex.main = 1.5
)

# Add lines for each treatment
segments(
  x0 = 1:3,
  y0 = cwd_data$lower,
  1:3,
  cwd_data$upper,
  col = "black",
  lwd = 2
)

# Add points for mean occupation probabilities
points(
  x = 1:3,
  y = cwd_data$mean,
  pch = 19,
  col = "black",
  lwd = 4
)

axis(
  1,
  at = 1:3,
  labels = cwd_data$treatments,
  lwd = 1,
  padj = 0.5
)
