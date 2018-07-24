# load package

library("jagsUI")
library(wiqid) 
library(coda)
library(dplyr)

## read stuff ##
rm(list=ls(all=TRUE))
dec.data<-read.csv("data/final/dec.data.muntjac.csv",header=TRUE, row.names=1)

head(dec.data)
# Separate out the detection history and investigate
DH <- as.matrix(dec.data[, 1:284])
colSums(DH, na.rm=TRUE)
rowSums(DH, na.rm=TRUE)  # no all-NA rows or columns
y <- rowSums(DH, na.rm=TRUE)
table(y)
mean(y > 0) # Naive occupancy
n <- rowSums(!is.na(DH))
plot(table(n))
occSS0(y, n)
occSSrn0(y, n)  # Much lower AIC

# camear level effect of covars
covars <- dec.data[288:290]
covars$cam_angle <- dec.data$cam_angle

# park level effect covars
park.data <- data.frame(park.ind=dec.data$park.ind, parksize=dec.data$parksize, outreach=dec.data$outreach, 
                        punishment=dec.data$punishment,park_outreach=dec.data$park_outreach, 
                        park_punishment=dec.data$park_punishment) %>% distinct() %>% 
  arrange(park.ind)


modelText <- "

model{
for(j in 1:J){ # sites 
# Biological model
N[j] ~ dpois(lambda[j])
log(lambda[j]) <- park[park.ind[j]] + a1*ele[j] + a2*population[j] + a3*distance[j] 

#Detection model
y[j] ~ dbin(p[j], n[j])  # y[j] is the number of 1's for site j, out of n[j] days
p[j] <- 1-(1-r[j])^N[j]
logit(r[j]) <- b0 + b1*cam_angle[j]
}

# Priors:
sd.p ~ dunif(0,20) 
tau.p <- 1/(sd.p*sd.p)

for(i in 1:6) {  # loop over parks
mu.park[i] <- a0 + a4*parksize[i] + a5*outreach[i] + a6*punishment[i] # random parks effects
park[i] ~ dnorm(mu.park[i], tau.p)
}

# Priors:
a0 ~ dunif(-50,50) # norm distributed prior causing slow mixing problem, using uniform priors instead
a1 ~ dunif(-50,50)
a2 ~ dunif(-50,50)
a3 ~ dunif(-50,50)
a4 ~ dunif(-50,50)
a5 ~ dunif(-50,50)
a6 ~ dunif(-50,50)  
b0 ~ dunif(-10,10)
b1 ~ dunif(-50,50)

}
"
writeLines(modelText, con="R/model_parkEff.txt")

jagsData <- as.list(covars)
jagsData$y <- y
jagsData$n <- n
jagsData$J <- length(y)
jagsData$parksize <- park.data$parksize
jagsData$park.ind <- dec.data$park.ind

# village reported data 
jagsData$outreach <- park.data$outreach
jagsData$punishment <- park.data$punish

#park reported data 
jagsData.park <- jagsData
jagsData.park$outreach <- park.data$park_outreach
jagsData.park$punishment <- park.data$park_punishment


str(jagsData)

wanted <- c( "a0","a1", "a2", "a3", "a4", "a5", "a6", "b0", "b1","sd.p","N")

inits <- function() list(N = y+1)

# Takes 10+ mins:
( fit.village <- jags(jagsData, inits, wanted, model.file="R/model_parkEff.txt", DIC=FALSE,    
                      n.adapt = 10000, n.chains=3, n.iter=1500000, n.burnin =50000, n.thin=100, parallel = T))

( fit.park <- jags(jagsData.park, inits, wanted, model.file="R/model_parkEff.txt", DIC=FALSE,   
                   n.adapt = 10000, n.chains=3, n.iter=1500000, n.burnin =50000, n.thin=100, parallel = T))

save.image("~/Desktop/data/Chen_etal_2018/Workplace/RN_model_park_eff_muntjac.RData")


