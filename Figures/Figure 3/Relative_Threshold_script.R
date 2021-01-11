#Code for Figure 3 of "A universal, empirically derived proxy for coral bleaching susceptibility"

#setwd to source file location

#######################################
######### RELATIVE THRESHOLDS #########
#######################################

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(plyr)
library(reshape2)
library(dplyr)

Relative_thresholds<-read.csv("Relative_Thresholds.csv")
Relative_thresholds$Site<-as.factor(Relative_thresholds$Site)
Relative_thresholds$Species<-as.factor(Relative_thresholds$Species)
str(Relative_thresholds)

Acropora_relative<-subset(Relative_thresholds, Species=='Acropora')
Pocillopora_relative<-subset(Relative_thresholds, Species=='Pocillopora')
Porites_relative<-subset(Relative_thresholds, Species=='Porites')
Stylophora_relative<-subset(Relative_thresholds, Species=='Stylophora')

#### STYLOPHORA RELATIVE THRESHOLD/MMM ####

Relative_Threshold_mod <- aov(Relative_Threshold ~ Site * Species, data = Relative_thresholds)
plot(Relative_Threshold_mod)
summary(Relative_Threshold_mod)

print(emmeans(Relative_Threshold_mod, list(pairwise ~ Species|Site)), adjust = c("tukey"))
print(emmeans(Relative_Threshold_mod, list(pairwise ~ Site|Species)), adjust = c("tukey"))

#### Plot relative thresholds ####
print(levels(Relative_thresholds$Site))
Relative_thresholds$Site = factor(Relative_thresholds$Site,levels(Relative_thresholds$Site)[c(4,3,6,1,2,5)])

print(levels(Relative_thresholds$Species))
Relative_thresholds$Species = factor(Relative_thresholds$Species,levels(Relative_thresholds$Species)[c(4,3,1,2)])

threshold_summary <- ddply(Relative_thresholds, c("Species", "Site"), summarise, N= length(Relative_Threshold), 
                           mean = mean(Relative_Threshold), sd   = sd(Relative_Threshold), se   = sd / sqrt(N))
threshold_summary

Threshold_by_species<-ggplot(threshold_summary, aes(x = Species, y = mean, fill = Site)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3, lwd=0.7, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1"), drop = FALSE, name = "Site") +
  theme_bw() + expand_limits(y = 0) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines")) + xlab(label = "Species") + ylab(label = "Thermal threshold (°C relative to local MMM)")+
  theme(axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=18, face="plain")) +
  geom_point(data=Relative_thresholds,aes(Species,Relative_Threshold,color=Site),position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.15), shape=21, color='black') +
  scale_colour_manual(values = c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1"))

Threshold_by_species

####

Threshold_by_site<-ggplot(threshold_summary, aes(x = Site, y = mean, fill = Species)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3, lwd=0.7, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#E08963", "#ffaf4d", "#E0BBE4", "#349fa4"), drop = FALSE, name = "Site") +
  theme_bw() + expand_limits(y = 0) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines")) + xlab(label = "Site") + ylab(label = "Thermal threshold (°C relative to local MMM)")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain")) +
  geom_point(data=Relative_thresholds,aes(Site,Relative_Threshold,color=Species),position=position_jitterdodge(dodge.width = 0.9, jitter.width = 0.15), shape=21, color='black') +
  scale_colour_manual(values = c("#E08963", "#ffaf4d", "#E0BBE4", "#349fa4"))

Threshold_by_site


#END