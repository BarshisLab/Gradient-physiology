#Code to plot Fig. S1a of "Empirically derived thermal thresholds of four coral species along the Red Sea"

#setwd to source file location

###############################
#### Plot standard profile ####
###############################

library(ggplot2)

Standard_profile<-read.csv("Standard_profile.csv")

cols <- c("30A" = "navyblue", "30B" = "navyblue", "33A" = "darkgoldenrod1", "33B" = "darkgoldenrod1", 
          "36A" = "darkorange2", "36B" = "darkorange2", "39A" = "red3", "39B" = "red3")

Standard_plot<-ggplot(Standard_profile, aes(x=Hour, y=Temp, col=Tank)) + scale_colour_manual(values=cols) +
  geom_line(aes(linetype=Tank)) +
  scale_linetype_manual(values=c("solid","dashed","solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  scale_y_continuous(limits=c(29,40), breaks = c(30,32,34,36,38,40)) + 
  scale_x_continuous(limits=c(0,17), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)) +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(text = element_text(size=15, face="bold")) + 
  theme(axis.text = element_text(colour = "black")) + theme(panel.background = element_rect(colour = "black", size=1)) + 
  theme(axis.ticks.length=unit(.2,"cm")) +
  geom_hline(yintercept=c(30,33,36,39), linetype="dashed", color = c("navyblue","darkgoldenrod1","darkorange2","red3"), size=0.5)

Standard_plot