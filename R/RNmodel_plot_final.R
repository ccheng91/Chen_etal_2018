# caterpiller plot of beta coefficient  
library(ggplot2)
## read stuff ##
rm(list=ls(all=TRUE))

###########################################
####### Village reproted result ##########
###########################################

wildboar <- read.csv("Result/beta_conf_villagereport_wildboar.csv", header = T)
muntjac <- read.csv("Result/beta_conf_villagereport_muntjac.csv", header = T)
common_civet <- read.csv("Result/beta_conf_villagereport_commonpalmcivet.csv", header = T)
masked_civet <- read.csv("Result/beta_conf_villagereport_maskedpalmcivet.csv", header = T)

# making result table
tab <- matrix(999, ncol=7, nrow=4)
tab <- as.data.frame(tab)
for(i in 1:7 ){
tab[1,i] <- paste(wildboar$mean[i]," (", wildboar$lower[i], ", ", wildboar$higher[i],")", 
      " (",wildboar$X90lower[i],",",wildboar$X90higher[i],")", sep = "")

tab[2,i] <- paste(muntjac$mean[i]," (", muntjac$lower[i], ", ", muntjac$higher[i],")", 
                  " (",muntjac$X90lower[i],",",muntjac$X90higher[i],")", sep = "")

tab[3,i] <- paste(common_civet$mean[i]," (", common_civet$lower[i], ", ", common_civet$higher[i],")", 
                  " (",common_civet$X90lower[i],",",common_civet$X90higher[i],")", sep = "")

tab[4,i] <- paste(masked_civet$mean[i]," (", masked_civet$lower[i], ", ", masked_civet$higher[i],")", 
                  " (",masked_civet$X90lower[i],",",masked_civet$X90higher[i],")", sep = "")
}

colnames(tab) <- c("Elevation","Human Population","Distance","Park Size","Village outreach","Village punishment","Camera Angle")
rownames(tab) <- c("wildboar", "muntjac", "common_civet", "masked_civet")
write.csv(tab, file = "result/RN_model_villagereport_4_spp_final.csv")

# cater plot
# wildboar 
wildboar$labels <- factor(wildboar$labels, levels = c("Elevation","Human Population","Distance","Park Size","Village outreach",
                                                      "Village punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(wildboar, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)  # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))

# muntjac
muntjac$labels <- factor(muntjac$labels, levels = c("Elevation","Human Population","Distance","Park Size","Village outreach",
                                                    "Village punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(muntjac, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10) 

# common palm civet
common_civet$labels <- factor(common_civet$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Village outreach",
                                                               "Village punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(common_civet, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)
#+scale_y_continuous(breaks = seq(-3, 3, 1))

# masked palm civet
masked_civet$labels <- factor(masked_civet$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Village outreach",
                                                               "Village punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(masked_civet, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)    # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))


###########################################
####### Park reproted result #############
###########################################

wildboar.p <- read.csv("Result/beta_conf_parkreport_wildboar.csv", header = T)
muntjac.p <- read.csv("Result/beta_conf_parkreport_muntjac.csv", header = T)
common_civet.p <- read.csv("Result/beta_conf_parkreport_commonpalmcivet.csv", header = T)
masked_civet.p <- read.csv("Result/beta_conf_parkreport_maskedpalmcivet.csv", header = T)

# making result table
tab <- matrix(999, ncol=7, nrow=4)
tab <- as.data.frame(tab)
for(i in 1:7 ){
  tab[1,i] <- paste(wildboar.p$mean[i]," (", wildboar.p$lower[i], ", ", wildboar.p$higher[i],")", 
                    " (",wildboar.p$X90lower[i],",",wildboar.p$X90higher[i],")", sep = "")
  
  tab[2,i] <- paste(muntjac.p$mean[i]," (", muntjac.p$lower[i], ", ", muntjac.p$higher[i],")", 
                    " (",muntjac.p$X90lower[i],",",muntjac.p$X90higher[i],")", sep = "")
  
  tab[3,i] <- paste(common_civet.p$mean[i]," (", common_civet.p$lower[i], ", ", common_civet.p$higher[i],")", 
                    " (",common_civet.p$X90lower[i],",",common_civet.p$X90higher[i],")", sep = "")
  
  tab[4,i] <- paste(masked_civet.p$mean[i]," (", masked_civet.p$lower[i], ", ", masked_civet.p$higher[i],")", 
                    " (",masked_civet.p$X90lower[i],",",masked_civet.p$X90higher[i],")", sep = "")
}

colnames(tab) <- c("Elevation","Human Population","Distance","Park Size","Park outreach","Park punishment","Camera Angle")
rownames(tab) <- c("wildboar", "muntjac", "common_civet", "masked_civet")
write.csv(tab, file = "result/RN_model_parkreport_4_spp_final.csv")


# wildboar 
wildboar.p$labels <- factor(wildboar.p$labels, levels = c("Elevation","Human Population","Distance","Park Size","Park outreach",
                                                      "Park punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(wildboar.p, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)  # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))

# muntjac
muntjac.p$labels <- factor(muntjac.p$labels, levels = c("Elevation","Human Population","Distance","Park Size","Park outreach",
                                                    "Park punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(muntjac.p, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10) 

# common palm civet
common_civet.p$labels <- factor(common_civet.p$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Park outreach",
                                                               "Park punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(common_civet.p, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)
#+scale_y_continuous(breaks = seq(-3, 3, 1))

# masked palm civet
masked_civet.p$labels <- factor(masked_civet.p$labels, levels =  c("Elevation","Human Population","Distance","Park Size","Park outreach",
                                                               "Park punishment","Camera Angle"))
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(masked_civet.p, aes(y=mean, x=labels))
P + theme_bw()+ 
  geom_segment(aes(x=labels, y=X90lower,yend=X90higher ,xend=labels),size=10 ,color = "gray35" )+
  geom_segment(aes(x=labels, y=lower,yend=higher ,xend=labels),size=3 ,color = "gray35" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=25, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=25)) + ylab("Standardized Beta coefficient") + labs(x = "") + geom_point(colour = "black", shape=95, size = 10)    # geom_errorbar(limit, na.rm =F, width=0.25) +
#+scale_y_continuous(breaks = seq(-3, 3, 1))



