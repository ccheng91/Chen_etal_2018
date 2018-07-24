#post-analysis masked palm civet
rm(list=ls(all=TRUE))
load("~/Desktop/data/Chen_etal_2018/Workplace/RN_model_park_eff_maskedpalmcivet.RData")

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

write.csv(cater, file = "Result/beta_conf_villagereport_maskedpalmcivet.csv")


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

write.csv(cater, file = "Result/beta_conf_parkreport_maskedpalmcivet.csv")
