# load package

library("jagsUI")
library(wiqid) 
library(coda)
library(dplyr)

## read stuff ##
rm(list=ls(all=TRUE))
dec.data<-read.csv("data/final/dec.data.commonpalmcivet.csv",header=TRUE, row.names=1)

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
occSSrn0(y, n)  # lower AIC

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
a0 ~ dunif(-10,10) # norm distributed prior causing slow mixing problem, using uniform priors instead
a1 ~ dunif(-10,10)
a2 ~ dunif(-10,10)
a3 ~ dunif(-10,10)
a4 ~ dunif(-10,10)
a5 ~ dunif(-10,10)
a6 ~ dunif(-10,10)  
b0 ~ dunif(-10,10)
b1 ~ dunif(-10,10)

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

# Takes 1 hour:
( fit.village <- jags(jagsData, inits, wanted, model.file="R/model_parkEff.txt", DIC=FALSE,    
                      n.chains=3, n.iter=1500000, n.burnin =50000, n.thin=100, parallel = T))

( fit.park <- jags(jagsData.park, inits, wanted, model.file="R/model_parkEff.txt", DIC=FALSE,   
                   n.chains=3, n.iter=1500000, n.burnin =50000, n.thin=100, parallel = T))

save.image("~/Desktop/data/Chen_etal_2018/Workplace/RN_model_park_eff_commonpalmcivet.RData")



#post-analysis Common palm civet

par <- c("a1", "a2", "a3", "a4", "a5", "a6", "b1")
plot(fit.village, parameters = par)
jagsUI::whiskerplot(fit.village, parameters = par, quantiles=c(0.025,0.975)) # quick caterplot 
jagsUI::whiskerplot(fit.village, parameters = par, quantiles=c(0.05,0.95))

jagsUI::whiskerplot(fit.park, parameters = par, quantiles=c(0.025,0.975))
jagsUI::whiskerplot(fit.park, parameters = par, quantiles=c(0.05,0.95))
plot(fit.park, parameters = par)

abu <- fit.village$sims.list$N 
co <- colMeans(abu)
mean(co) #  site abundance mean
sd(co) #  site abundance sd
sum(co) # overall abundance

a1 <- fit.village$sims.list$a1
a2 <- fit.village$sims.list$a2
a3 <- fit.village$sims.list$a3
a4 <- fit.village$sims.list$a4
a5 <- fit.village$sims.list$a5
a6 <- fit.village$sims.list$a6
b1 <- fit.village$sims.list$b1

##### beta conf table ######
preds<-as.data.frame(cbind(a1,a2,a3,a4,a5,a6,b1))

cater <- matrix(rep(0), ncol=length(preds),nrow =6)
for ( i in 1:ncol(preds)) {
  cater[1,i] <-mean(preds[,i])
  cater[2,i] <-sd(preds[,i])
  cater[3,i] <-quantile(preds[,i],prob=0.025)
  cater[4,i] <-quantile(preds[,i],prob=0.975)
  cater[5,i] <-quantile(preds[,i],prob=0.05)
  cater[6,i] <-quantile(preds[,i],prob=0.95)
}
labs <- c("Elevation","Human Population","Distance","Park Size","Village outreach","Village punishment","Camera Angle")
cater <- as.data.frame(cater)
cater[7,] <- labs
rownames(cater) <- c("mean","sd","lower","higher","90lower","90higher","labels")
cater <- as.data.frame(t(cater)) # reverse row and col

cater[,1] <- as.numeric(as.character(cater[,1])) # the type of data
cater[,2] <- as.numeric(as.character(cater[,2]))
cater[,3] <- as.numeric(as.character(cater[,3]))
cater[,4] <- as.numeric(as.character(cater[,4]))
cater[,5] <- as.numeric(as.character(cater[,5]))
cater[,6] <- as.numeric(as.character(cater[,6]))
cater[,1:6] <- round(cater[,1:6], digits=2)

# order of the bars
cater$labels <- factor(cater$labels, levels = c("Elevation","Human Population","Distance","Park Size","Village outreach",
                                                "Village punishment","Camera Angle"))

write.csv(cater, file = "Result/beta_conf_villagereport_commonpalmcivet.csv")


##### park reproted model ###### ###### beta conf table #####
a1 <- fit.park$sims.list$a1
a2 <- fit.park$sims.list$a2
a3 <- fit.park$sims.list$a3
a4 <- fit.park$sims.list$a4
a5 <- fit.park$sims.list$a5
a6 <- fit.park$sims.list$a6
b1 <- fit.park$sims.list$b1

preds<-as.data.frame(cbind(a1,a2,a3,a4,a5,a6,b1))

cater <- matrix(rep(0), ncol=length(preds),nrow =6)
for ( i in 1:ncol(preds)) {
  cater[1,i] <-mean(preds[,i])
  cater[2,i] <-sd(preds[,i])
  cater[3,i] <-quantile(preds[,i],prob=0.025)
  cater[4,i] <-quantile(preds[,i],prob=0.975)
  cater[5,i] <-quantile(preds[,i],prob=0.05)
  cater[6,i] <-quantile(preds[,i],prob=0.95)
}
labs <- c("Elevation","Human Population","Distance","Park Size","Park outreach","Park punishment","Camera Angle")
cater <- as.data.frame(cater)
cater[7,] <- labs
rownames(cater) <- c("mean","sd","lower","higher","90lower","90higher","labels")
cater <- as.data.frame(t(cater)) # reverse row and col

cater[,1] <- as.numeric(as.character(cater[,1])) # the type of data
cater[,2] <- as.numeric(as.character(cater[,2]))
cater[,3] <- as.numeric(as.character(cater[,3]))
cater[,4] <- as.numeric(as.character(cater[,4]))
cater[,5] <- as.numeric(as.character(cater[,5]))
cater[,6] <- as.numeric(as.character(cater[,6]))
cater[,1:6] <- round(cater[,1:6], digits=2)
# order of the bars
cater$labels <- factor(cater$labels, levels = c("Elevation","Human Population","Distance","Park Size","Park outreach",
                                                "Park punishment","Camera Angle"))

write.csv(cater, file = "Result/beta_conf_parkreport_commonpalmcivet.csv")

