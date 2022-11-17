# post analysis
rm(list=ls(all=TRUE))
load("Workplace/MSOM_villager_reported.RData")
library(ggplot2)
library(jagsUI)
fit$Rhat
#############################################################
############# check traceplot & density plots ###############
#############################################################
par <- c( 'mu.a1','mu.a3','mu.a11','mu.a4', 'mu.a5', 'mu.a6', 
          'mu.b1', 'mu.b2')
par.name <-c("Elevation","Human Population","Distance","Park Size","Village Punishment","Village Outreach","Camera hours","Camera Angle")

jagsUI::whiskerplot(fit,parameters = par)

######################################
### making params table #############
#######################################
tab <- matrix(999, ncol=2, nrow=8)
tab <- as.data.frame(tab)

mu.a1 <- fit$sims.list$mu.a1
mu.a3 <- fit$sims.list$mu.a3
mu.a4 <- fit$sims.list$mu.a4
mu.a5 <- fit$sims.list$mu.a5
mu.a6 <- fit$sims.list$mu.a6
mu.a11 <- fit$sims.list$mu.a11
mu.b1 <- fit$sims.list$mu.b1
mu.b2 <- fit$sims.list$mu.b2


tab[1,1] <- round(mean(mu.a1),digits = 2) 
tab[1,2] <- paste(round(quantile(mu.a1, probs = 0.025), digits = 2),", ",round(quantile(mu.a1, probs = 0.975), digits = 2), sep = "")

tab[2,1] <- round(mean(mu.a3),digits = 2) 
tab[2,2] <- paste(round(quantile(mu.a3, probs = 0.025), digits = 2),", ",round(quantile(mu.a3, probs = 0.975), digits = 2), sep = "")

tab[3,1] <- round(mean(mu.a11),digits = 2) 
tab[3,2] <- paste(round(quantile(mu.a11, probs = 0.025), digits = 2),", ",round(quantile(mu.a11, probs = 0.975), digits = 2), sep = "")

tab[4,1] <- round(mean(mu.a4),digits = 2) 
tab[4,2] <- paste(round(quantile(mu.a4, probs = 0.025), digits = 2),", ",round(quantile(mu.a4, probs = 0.975), digits = 2), sep = "")

tab[5,1] <- round(mean(mu.a5),digits = 2) 
tab[5,2] <- paste(round(quantile(mu.a5, probs = 0.025), digits = 2),", ",round(quantile(mu.a5, probs = 0.975), digits = 2), sep = "")

tab[6,1] <- round(mean(mu.a6),digits = 2) 
tab[6,2] <- paste(round(quantile(mu.a6, probs = 0.025), digits = 2),", ",round(quantile(mu.a6, probs = 0.975), digits = 2), sep = "")

tab[7,1] <- round(mean(mu.b1),digits = 2) 
tab[7,2] <- paste(round(quantile(mu.b1, probs = 0.025), digits = 2),", ",round(quantile(mu.b1, probs = 0.975), digits = 2), sep = "")

tab[8,1] <- round(mean(mu.b2),digits = 2) 
tab[8,2] <- paste(round(quantile(mu.b2, probs = 0.025), digits = 2),", ",round(quantile(mu.b2, probs = 0.975), digits = 2), sep = "")

names(tab) <- c("Mean estimate", "95% credibility interval")
rownames(tab) <-c("Elevation","Human Population","Distance","Park Size","Village Punishment","Village Outreach","Camera hours","Camera Angle")

write.csv(tab, file = "result/MSOM_beta_coef_village.csv" )


############################
### Check model fit ########
############################

p.fit = fit$sims.list$p.fit
p.fitnew =fit$sims.list$p.fitnew
model.fit= length(which(p.fit-p.fitnew>0))/length(p.fit)
#If model.fit between 0.05 and 0.95, then model fit is good
model.fit
#################################################################

# Estimated site richness
Nsite <- fit$sims.list$Nsite
mean(Nsite)
sd(Nsite)

##############################
######## caterplot  ######## 
#############################
preds<-as.data.frame(cbind(mu.a1,mu.a3,mu.a11,mu.a4,mu.a5,mu.a6,mu.b1,mu.b2))
cater <- matrix(rep(0), ncol=length(preds),nrow =6)
for ( i in 1:ncol(preds)) {
  cater[1,i] <-mean(preds[,i])
  cater[2,i] <-sd(preds[,i])
  cater[3,i] <-quantile(preds[,i],prob=0.025)
  cater[4,i] <-quantile(preds[,i],prob=0.975)
  cater[5,i] <-quantile(preds[,i],prob=0.05)
  cater[6,i] <-quantile(preds[,i],prob=0.95)
}
labs <- c("Elevation","Human Population","Distance","Park Size","Village punishment","village outreach","Camera hours","Camera angle")
cater <- as.data.frame(cater)
cater[7,] <- labs
rownames(cater) <- c("mean","sd","lower","higher","X90lower","X90higher","labels")
cater <- as.data.frame(t(cater)) 

cater[,1] <- as.numeric(as.character(cater[,1])) # the type of data
cater[,2] <- as.numeric(as.character(cater[,2]))
cater[,3] <- as.numeric(as.character(cater[,3]))
cater[,4] <- as.numeric(as.character(cater[,4]))
cater[,5] <- as.numeric(as.character(cater[,5]))
cater[,6] <- as.numeric(as.character(cater[,6]))
cater[,1:6] <- round(cater[,1:6], digits=2)

cater$labels <- factor(cater$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Village punishment","village outreach","Camera hours","Camera angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(cater, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)  # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))


#############################################################
### plot estimated species richness vs single covariate ###
############################################################

u <- fit$sims.list$u
elev=sitecov.std$elevation; pop=sitecov.std$population; pasize=sitecov.std$pasize; 
punish=sitecov.std$punishment; reach=sitecov.std$outreach;
distance=sitecov.std$distance
a1 <- fit$sims.list$a5
a3 <- fit$sims.list$a3
a11 <- fit$sims.list$a11
a4 <- fit$sims.list$a4
a5 <- fit$sims.list$a5
a6 <- fit$sims.list$a6

######################################################
######## villager-reproted outreach ################
######################################################

#### outreach effect using 1.5 as minial outreach 7 as max

r.dummy <-seq(1.5,7,length.out=100) 
mreach <- mean(sitecov$reach)
sdreach <- sd(sitecov$reach)
hreach1 <- (r.dummy-mreach)/sdreach # standardized covariate gradient
head(sitecov)
psi=matrix(0,nrow=length(spp),ncol=dim(u)[1])
mu.park=matrix(0,nrow=dim(u)[1],ncol=length(spp))
richness2 = matrix(0, nrow=length(r.dummy), ncol=dim(u)[1])
u1 <- u[,,1] # use u from park 1, park 1 has meandiam mean vaule of u
for (j in 1:length(r.dummy)) {
  for(i in 1: length(spp)) {
    for (k in 1:dim(u)[1]) {
      
      mu.park[k,i] <- u1[k,i] + a4[k,i]*mean(pasize) + a5[k,i]*mean(punish) + a6[k,i]*hreach1[j] 
      psi[i,k] <- plogis(mu.park[k,i] + a1[k,i]*mean(elev) + a3[k,i]*mean(pop) + a11[k,i]*mean(distance))
      
    }}
  richness2[j,] <- apply(psi,2,sum)  
}

richness1<-cbind(apply(richness2,1,mean),apply(richness2,1,quantile,0.975),apply(richness2,1,quantile,0.025))
Reachrichness<-cbind(r.dummy, richness1)
Reachrichness <- as.data.frame(Reachrichness)
names(Reachrichness) <- c("reach", "richness", "up", "low")

## plot using ggplot2 
ggplot(Reachrichness, aes(x=reach)) +geom_line(aes(y = richness), colour="black") + theme_bw() +
  geom_line(aes(y = up),colour="gray", linetype=2) + geom_line(aes(y = low),colour="gray",linetype=2) +
  theme(axis.text.x = element_text(size=18))+ theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=25),axis.title=element_text(size=25)) +
  xlab("Villager-reproted outreach (times/year/village)") + ylab("Estimated species richness")+
  theme(text = element_text(size=15))

###########################
######## park size ########
###########################

s.dummy=seq(0.1,12,length.out=100)
msize <- mean(sitecov$size.m)
sdsize <- sd(sitecov$size.m)
hsize1 <-(s.dummy-msize)/sdsize # standardized covariate gradient

psi <- matrix(0,nrow=length(spp),ncol=dim(u)[1])
mu.park <- matrix(0,nrow=dim(u)[1],ncol=length(spp))
richness4 <-  matrix(0, nrow=length(s.dummy), ncol=dim(u)[1])
u1 <- u[,,1] # use u from park 1, park 1 has medium mean vaule of u
for (j in 1:length(s.dummy)) {
  for(i in 1: length(spp)) {
    for (k in 1:dim(u)[1]) {
      mu.park[k,i] <- u1[k,i] + a4[k,i]*hsize1[j] + a5[k,i]*mean(punish) + a6[k,i]*mean(reach) 
      psi[i,k] <- plogis(mu.park[k,i] + a1[k,i]*mean(elev) + a3[k,i]*mean(pop) + a11[k,i]*mean(distance))
      
    }}
  richness4[j,] <- apply(psi,2,sum)  
}   

richness5<-cbind(apply(richness4,1,mean),apply(richness4,1,quantile,0.975),apply(richness4,1,quantile,0.025))
sizerichness<-cbind(s.dummy*100, richness5)
sizerichness <- as.data.frame(sizerichness)
names(sizerichness) <- c("size", "richness", "up", "low")

## plot 
ggplot(sizerichness, aes(x=size)) +geom_line(aes(y = richness), colour="black") + theme_bw() +
  geom_line(aes(y = up),colour="gray", linetype=2) + geom_line(aes(y = low),colour="gray",linetype=2) +
  theme(axis.text.x = element_text(size=20))+ theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=25),axis.title=element_text(size=25)) +
  xlab(bquote('Park size'~(KM^2))) + ylab("Estimated species richness")+
  theme(text = element_text(size=15)) +theme(axis.text.x = element_text(hjust =1))



###########################################
##### spieces specific beta coef #########
###########################################
a1 <- fit$sims.list$a5
a3 <- fit$sims.list$a3
a11 <- fit$sims.list$a11
a4 <- fit$sims.list$a4
a5 <- fit$sims.list$a5
a6 <- fit$sims.list$a6

######### a1 elevation #############
betaa1 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa1[i,1] <- mean(a1[,i])
  betaa1[i,2] <- sd(a1[,i])
  betaa1[i,3] <- quantile(a1[,i], prob=0.025)
  betaa1[i,4] <- quantile(a1[,i], prob=0.975)
}

betaa1 <- as.data.frame(betaa1)
rownames(betaa1) <- spp
betaa1[,5] <- spp
colnames(betaa1) <- c("Mean", "SD", "lower", "higher","spp")
sig<- (betaa1$higher - abs(betaa1$lower))

betaa1 <- transform(betaa1, spp = reorder(spp,-Mean))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa1, aes(y=Mean, x=spp))
P + geom_point(stat="identity") +
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=18), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Standardized Beta coefficient of elevation") + labs(x = "") +
  scale_y_continuous(breaks = seq(-3, 3, 0.5))

######### a3 human population #############
betaa3 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa3[i,1] <- mean(a3[,i])
  betaa3[i,2] <- sd(a3[,i])
  betaa3[i,3] <- quantile(a3[,i], prob=0.025)
  betaa3[i,4] <- quantile(a3[,i], prob=0.975)
}

betaa3 <- as.data.frame(betaa3)
rownames(betaa3) <- spp
betaa3[,5] <- spp
colnames(betaa3) <- c("Mean", "SD", "lower", "higher","spp")

betaa3 <- transform(betaa3, spp = reorder(spp, -Mean))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa3, aes(y=Mean, x=spp))
P + geom_point(stat="identity") + scale_y_continuous(breaks = seq(-8, 8, 1))+
  theme_bw() +theme(axis.text.x = element_text(angle = 70, hjust = 1, size=15), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Standardized Beta coefficient of human density") +labs(x = "") 


########### a11 distance #############
betaa11 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa11[i,1] <- mean(a11[,i])
  betaa11[i,2] <- sd(a11[,i])
  betaa11[i,3] <- quantile(a11[,i], prob=0.025)
  betaa11[i,4] <- quantile(a11[,i], prob=0.975)
}

betaa11 <- as.data.frame(betaa11)
rownames(betaa11) <- spp
betaa11[,5] <- spp
colnames(betaa11) <- c("Mean", "SD", "lower", "higher","spp")

betaa11 <- transform(betaa11, spp = reorder(spp, -Mean))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa11, aes(y=Mean, x=spp))
P + geom_point(stat="identity")  + 
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=18), axis.line.y=element_line()) + geom_hline(yintercept=0)+  
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Standardized Beta coefficient of distance") + labs(x = "") +
  scale_y_continuous(breaks = seq(-1,2, 0.5))

##### a4 park size #####
betaa4 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa4[i,1] <- mean(a4[,i])
  betaa4[i,2] <- sd(a4[,i])
  betaa4[i,3] <- quantile(a4[,i], prob=0.025)
  betaa4[i,4] <- quantile(a4[,i], prob=0.975)
}

betaa4 <- as.data.frame(betaa1)
rownames(betaa4) <- spp
betaa4[,5] <- spp
colnames(betaa4) <- c("Mean", "SD", "lower", "higher","spp")
sig<- (betaa4$higher - abs(betaa4$lower))

betaa4 <- transform(betaa4, spp = reorder(spp,-Mean))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa4, aes(y=Mean, x=spp))
P + geom_point(stat="identity") +
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=18), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Beta coefficient of park size") + labs(x = "") +
  scale_y_continuous(breaks = seq(-3, 3, 0.5))

##### a5 punishment #####
betaa5 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa5[i,1] <- mean(a5[,i])
  betaa5[i,2] <- sd(a5[,i])
  betaa5[i,3] <- quantile(a5[,i], prob=0.025)
  betaa5[i,4] <- quantile(a5[,i], prob=0.975)
}

betaa5 <- as.data.frame(betaa5)
rownames(betaa5) <- spp
betaa5[,5] <- spp
colnames(betaa5) <- c("Mean", "SD", "lower", "higher","spp")

betaa5 <- transform(betaa5, spp = reorder(spp, -lower))
#betaa5$spp <- reorder(betaa5$spp, -(betaa5$lower))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa5, aes(y=Mean, x=spp))
P + geom_point(stat="identity")  + 
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=18), axis.line.y=element_line()) + geom_hline(yintercept=0)+ 
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Standardized Beta coefficient of punishment") + labs(x = "") +
  scale_y_continuous(breaks = seq(-8,30, 2))


##### a6 outreach ######
betaa6 <- matrix(rep(0), nrow=length(spp), ncol=4)
for (i in 1:length(spp)) {
  betaa6[i,1] <- mean(a6[,i])
  betaa6[i,2] <- sd(a6[,i])
  betaa6[i,3] <- quantile(a6[,i], prob=0.025)
  betaa6[i,4] <- quantile(a6[,i], prob=0.975)
}

betaa6 <- as.data.frame(betaa6)
rownames(betaa6) <- spp
betaa6[,5] <- spp
colnames(betaa6) <- c("Mean", "SD", "lower", "higher","spp")

betaa6 <- transform(betaa6, spp = reorder(spp, -lower))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(betaa6, aes(y=Mean, x=spp))
P + geom_point(stat="identity") + 
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=18), axis.line.y=element_line()) + geom_hline(yintercept=0) + 
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.25) + ylab("Standardized Beta coefficient of outreach")  + labs(x = "") +
  scale_y_continuous(breaks = seq(-4,8, 1))

########################################
# percentage beta param of spp > 0 
########################################

length(which(betaa1$Mean > 0))/ nrow(betaa1)
length(which(betaa3$Mean > 0))/ nrow(betaa1)
length(which(betaa11$Mean > 0))/ nrow(betaa1)
length(which(betaa4$Mean > 0))/ nrow(betaa1)
length(which(betaa5$Mean > 0))/ nrow(betaa1)
length(which(betaa6$Mean > 0))/ nrow(betaa1)

##############################################

################################################
###### loading MSOM that use park data#########
################################################

rm(list=ls(all=TRUE))
load("Workplace/MSOM_park_reported.RData")
mu.a1 <- fit$sims.list$mu.a1
mu.a3 <- fit$sims.list$mu.a3
mu.a4 <- fit$sims.list$mu.a4
mu.a5 <- fit$sims.list$mu.a5
mu.a6 <- fit$sims.list$mu.a6
mu.a11 <- fit$sims.list$mu.a11
mu.b1 <- fit$sims.list$mu.b1
mu.b2 <- fit$sims.list$mu.b2

preds<-as.data.frame(cbind(mu.a1,mu.a3,mu.a11,mu.a4,mu.a5,mu.a6,mu.b1,mu.b2))
cater <- matrix(rep(0), ncol=length(preds),nrow =6)
for ( i in 1:ncol(preds)) {
  cater[1,i] <-mean(preds[,i])
  cater[2,i] <-sd(preds[,i])
  cater[3,i] <-quantile(preds[,i],prob=0.025)
  cater[4,i] <-quantile(preds[,i],prob=0.975)
  cater[5,i] <-quantile(preds[,i],prob=0.05)
  cater[6,i] <-quantile(preds[,i],prob=0.95)
}
labs <- c("Elevation","Human Population","Distance","Park Size","Park punishment","Park outreach","Camera hours","Camera angle")
cater <- as.data.frame(cater)
cater[7,] <- labs
rownames(cater) <- c("mean","sd","lower","higher","X90lower","X90higher","labels")
cater <- as.data.frame(t(cater)) 

cater[,1] <- as.numeric(as.character(cater[,1])) # the type of data
cater[,2] <- as.numeric(as.character(cater[,2]))
cater[,3] <- as.numeric(as.character(cater[,3]))
cater[,4] <- as.numeric(as.character(cater[,4]))
cater[,5] <- as.numeric(as.character(cater[,5]))
cater[,6] <- as.numeric(as.character(cater[,6]))
cater[,1:6] <- round(cater[,1:6], digits=2)

cater$labels <- factor(cater$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Park punishment","Park outreach","Camera hours","Camera angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(cater, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)  # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))
