#Code for Figure S2 of "A universal, empirically derived proxy for coral bleaching susceptibility"

#setwd to source file location

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

Full_Physiology<-read.csv("Gradient_physiology_clean.csv")
Full_Physiology$Temp<-as.factor(Full_Physiology$Temp)
Full_Physiology$Geno<-as.factor(Full_Physiology$Geno)
Full_Physiology$Site<-as.factor(Full_Physiology$Site)
Full_Physiology$Species<-as.factor(Full_Physiology$Species)

str(Full_Physiology)

Acr_phys<-subset(Full_Physiology, Species == 'Acropora')
Poc_phys<-subset(Full_Physiology, Species == 'Pocillopora')
Por_phys<-subset(Full_Physiology, Species == 'Porites')
Sty_phys<-subset(Full_Physiology, Species == 'Stylophora')

#### Acropora ####
Acr_sym_log<-log(Acr_phys$Sym_cm2)
Acr_sym<-lmer(Acr_sym_log ~ Temp*Site + (1|Geno/Site) + (1|Rep/Temp), data = Acr_phys)
sjPlot::plot_model(Acr_sym, type="diag")
anova(Acr_sym)
rand(Acr_sym)
step(Acr_sym, reduce.random = F)

Acr_sym_final<-lmer(Acr_sym_log ~ Temp*Site + (1|Geno/Site), data = Acr_phys)
anova(Acr_sym_final)

print(emmeans(Acr_sym_final, list(pairwise ~ Temp|Site)), adjust = c("mvt"))
print(emmeans(Acr_sym_final, list(pairwise ~ Site|Temp)), adjust = c("mvt"))

#plot
print(levels(Acr_phys$Site)) 
Acr_phys$Site = factor(Acr_phys$Site,levels(Acr_phys$Site)[c(4,3,5)]) 

# Colours-> Eilat, Al Wajh, Yanbu, AlFahal, Al Q, Obock
# "royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")
dodge <- position_dodge(width=0.75) 

Acr_sym <- ggplot(data=Acr_phys, 
                  aes(x=Temp, y=Sym_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("royalblue2", "darkgoldenrod1", "darkorange1"), drop = FALSE, name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  theme_bw() +
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
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature (°C)") + ylab(label = "Symbiont density (cm-2)")+
        theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom") 
Acr_sym

#### Pocillopora ####
Poc_sym_log<-log(Poc_phys$Sym_cm2)
Poc_sym<-lmer(Poc_sym_log ~ Temp*Site + (1|Geno/Site), data = Poc_phys)
sjPlot::plot_model(Poc_sym, type="diag")
anova(Poc_sym) 
rand(Poc_sym)
step(Poc_sym, reduce.random = F)

Poc_sym_final<-lmer(Poc_sym_log ~ Temp*Site + (1|Geno/Site), data = Poc_phys)
anova(Poc_sym_final)

print(emmeans(Poc_sym_final, list(pairwise ~ Temp|Site)), adjust = c("mvt"))
print(emmeans(Poc_sym_final, list(pairwise ~ Site|Temp)), adjust = c("mvt"))

#plot
print(levels(Poc_phys$Site))
Poc_phys$Site = factor(Poc_phys$Site,levels(Poc_phys$Site)[c(4,3,5)]) 

# Colours-> Eilat, Al Wajh, Yanbu, AlFahal, Al Q, Obock
# "royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")

Poc_sym <- ggplot(data=Poc_phys, 
                  aes(x=Temp, y=Sym_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("royalblue2", "darkgoldenrod1", "darkorange1"), drop = FALSE, name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
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
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature (°C)") + ylab(label = "Symbiont density (cm-2)")+
        theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")
Poc_sym

#### Stylophora ####
Sty_sym_log<-log(Sty_phys$Sym_cm2)
Sty_sym<-lmer(Sty_sym_log ~ Temp*Site + (1|Geno/Site), data = Sty_phys)
sjPlot::plot_model(Sty_sym, type="diag")
anova(Sty_sym) 
rand(Sty_sym)
step(Sty_sym, reduce.random = F)

Sty_sym_final<-lmer(Sty_sym_log ~ Temp*Site + (1|Rep/Geno) + (1|Geno/Site), data = Sty_phys)
anova(Sty_sym_final)

print(emmeans(Sty_sym_final, list(pairwise ~ Temp|Site)), adjust = c("mvt"))
print(emmeans(Sty_sym_final, list(pairwise ~ Site|Temp)), adjust = c("mvt"))

#plot
print(levels(Sty_phys$Site))
Sty_phys$Site = factor(Sty_phys$Site,levels(Sty_phys$Site)[c(4,5,1)]) 

# Colours-> Eilat, Al Wajh, Yanbu, AlFahal, Al Q, Obock
# "royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")

Sty_sym <- ggplot(data=Sty_phys, 
                  aes(x=Temp, y=Sym_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
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
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature (°C)") + ylab(label = "Symbiont density (cm-2)")+
        theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")

Sty_sym

#### Porites ####
Por_sym_log<-log(Por_phys$Sym_cm2)
Por_sym<-lmer(Por_sym_log ~ Temp*Site + (1|Geno/Site), data = Por_phys)
sjPlot::plot_model(Por_sym, type="diag")
anova(Por_sym) 
rand(Por_sym)
step(Por_sym, reduce.random = F)

Por_sym_final<-lmer(Por_sym_log ~ Temp*Site + (1|Geno/Site), data = Por_phys)
anova(Por_sym_final)

print(emmeans(Por_sym_final, list(pairwise ~ Temp)), adjust = c("mvt"))
print(emmeans(Por_sym_final, list(pairwise ~ Site)), adjust = c("mvt"))

#plot
print(levels(Por_phys$Site))
Por_phys$Site = factor(Por_phys$Site,levels(Por_phys$Site)[c(5,1,2)]) 

# Colours-> Eilat, Al Wajh, Yanbu, AlFahal, Al Q, Obock
# "royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")

Por_sym <- ggplot(data=Por_phys, 
                  aes(x=Temp, y=Sym_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("darkorange1","red3","darkorchid4"), drop = FALSE, name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
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
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature (°C)") + ylab(label = "Symbiont density (cm-2)")+
        theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")

Por_sym


#END