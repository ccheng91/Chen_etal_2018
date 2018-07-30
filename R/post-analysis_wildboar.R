#post-analysis wildboar 
rm(list=ls(all=TRUE))
load("Workplace/RN_model_park_eff_wildboar.RData")

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

##### beta conf plot ############ beta conf plot ############ beta conf plot #####
### caterpiller plot 
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
write.csv(cater, file = "Result/beta_conf_villagereport_wildboar.csv")


##### park reproted model ############ beta conf plot ############ beta conf plot #####
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

write.csv(cater, file = "Result/beta_conf_parkreport_wildboar.csv")








#### line graph ###
sd.p <- fit.village$sims.list$sd.p
a0 <- fit.village$sims.list$a0
hparksize <- seq(-0.938, 1.437, length.out=100)
hpunishment <- seq(-1.88,1.49, length.out=100)
moutreach <- mean(park.data$outreach)
mpunishment <- mean(park.data$punishment)
mele <- mean(covars$ele)
mpopulation <- mean(covars$population)
mdistance <- mean(covars$distance)
mparksize <- mean(park.data$parksize)
##
psi <- rep(0, length(a1))

vec.mean.park <- rep(0, length(a0))
park <- rep(0, length(a0))
 for(j in 1:100) {
   for(k in 1:length(a0)) {
    
  vec.mean.park[k] <- a0[k] + a4[k]*hparksize[j] + a5[k]*moutreach + a6[k]*mpunishment
  park[k] <- pnorm(vec.mean.park[k], (1/(sd.p[k]*sd.p[k])))
  abundance[j] <- rpois(100,exp(park[k] + a1[k]*mele + a2[k]*mpopulation + a3[k]*mdistance )) 
  
  
  }
 }

nsamp <- fit.village$mcmc.info$n.samples
pred.parksize <-  matrix(0, nrow = 100, ncol = nsamp )
vec.mean.park <- matrix(0, nrow = 100, ncol = nsamp )
park <- matrix(0, nrow = 100, ncol = nsamp )
lambda <- matrix(0, nrow = 100, ncol = nsamp )
for(k in 1:nsamp){
  for(j in 1:100){
  vec.mean.park[j,k] <- a0[k] + a4[k]*mparksize + a5[k]*moutreach + a6[k]*hpunishment[j]
  #park[j,k] <- rnorm(1,vec.mean.park[j,k], (1/(sd.p[k]*sd.p[k])))
  lambda[j,k] <- exp(vec.mean.park[j,k] + a1[k]*mele + a2[k]*mpopulation + a3[k]*mdistance )
  pred.parksize[j,k] <- rpois(1, lambda[j,k] )
  }
}
abund <- rowMeans(pred.parksize, na.rm = T)
abund <- rowSums(pred.parksize, na.rm = T)
abund <- rowMeans(lambda)

pred.parksize[1,]
lambda[1,]
slection <- sample(1:nsamp,100)
matplot(hparksize,pred.parksize[,slection])
sd.p <- fit.village$sims.list$sd.p


hpunishment <-seq(-1.88,1.49, length.out=100)
pred.parksize <-  matrix(0, nrow = 100, ncol = nsamp )
vec.mean.park <- matrix(0, nrow = 100, ncol = nsamp )
park <- matrix(0, nrow = 100, ncol = nsamp )
lambda <- matrix(0, nrow = 100, ncol = nsamp )

for(i in 1:100){
  vec.mean.park[j,] <- a0 + a4*mparksize + a5*moutreach + a6*hpunishment[j]
  park[j,] <- rnorm(100, mean =vec.mean.park[j,], sd =(1/(sd.p^2)))
  lambda[j,]<- exp(park[j,] + a1*mele + a2*mpopulation + a3*mdistance) 
  pred.parksize[j,] <- rpois(100, lambda[j,])
}


show <- sample(1:100, 100)
matplot(hpunishment, pred.parksize[,show], type = "l", lty=1,lwd=1, col = "grey", frame=F)
lines(hpunishment, apply(pred.parksize, 1, mean,na.rm =T), lwd =3, col="blue")
a <- apply(pred.parksize, 1, mean, na.omit =T)
pred.parksize[1,]
is.na(pred.parksize[1,])
ab <- rpois(100,lambda)
  
fit.village$summary
rnorm(1, mean =1, sd =1)

#for(j in 1:J){ # sites 
#   Biological model
#  N[j] ~ dpois(lambda[j])
#  log(lambda[j]) <- park[park.ind[j]] + a1*ele[j] + a2*population[j] + a3*distance[j] 
  
#Detection model
#y[j] ~ dbin(p[j], n[j])  # y[j] is the number of 1's for site j, out of n[j] days
#p[j] <- 1-(1-r[j])^N[j]
#logit(r[j]) <- b0 + b1*cam_angle[j]
#}

# Priors:
#sd.p ~ dunif(0,20) 
#tau.p <- 1/(sd.p*sd.p)

#for(i in 1:6) {  # loop over parks
#  mu.park[i] <- a0 + a4*parksize[i] + a5*outreach[i] + a6*punishment[i] # random parks effects
#  park[i] ~ dnorm(mu.park[i], tau.p)
#}



