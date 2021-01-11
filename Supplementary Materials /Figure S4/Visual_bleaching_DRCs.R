#Code for Figure S4 of "A universal, empirically derived proxy for coral bleaching susceptibility"
  #Note - Percent pigmentation is inversed and represented as percent bleaching in the final figure

#setwd to source file location

library(lmerTest)
library(emmeans)
library(sjPlot)
library(drc)
library(Rmisc)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(reshape2)

Vis_bleaching<-read.csv("Visual_bleaching_all.csv")
str(Vis_bleaching)

Vis_bleaching$Label<-as.factor(Vis_bleaching$Label)
Vis_bleaching$Site<-as.factor(Vis_bleaching$Site)
Vis_bleaching$Species<-as.factor(Vis_bleaching$Species)
Vis_bleaching$Geno<-as.factor(Vis_bleaching$Geno)
Vis_bleaching$Replicate<-as.factor(Vis_bleaching$Replicate)

Sty_data<-subset(Vis_bleaching, Species=="Stylophora")
Poc_data<-subset(Vis_bleaching, Species=="Pocillopora")
Por_data<-subset(Vis_bleaching, Species=="Porites")
Acr_data<-subset(Vis_bleaching, Species=="Acropora")

######################################## 
      #### Stylophora DRCs ####
######################################## 

levels(Sty_data$Site)
Sty_data$Site = factor(Sty_data$Site,levels(Sty_data$Site)[c(4,3,6,1,2,5)])
# correct N to S order is: "Eilat"  "AlWajh"  "Yanbu"  "AlFahal"  "AlQunfudhah"  "Obock"   

Sty_Eilat_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Eilat",], fct = LL.3())
summary(Sty_Eilat_pop)
plot(Sty_Eilat_pop)

Sty_Eilat <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Eilat",], curveid=Geno, fct = LL.3())
summary(Sty_Eilat)
plot(Sty_Eilat)

#extract coeffs by geno, then compute 95% CIs
Sty_Eilat_genocoeffs_50<-data.frame(ED(Sty_Eilat, c(50)))
Sty_Eilat_coeff_mean<-mean(Sty_Eilat_genocoeffs_50$Estimate)
Sty_Eilat_coeff_mean

Sty_Eilat_summary<-data.frame(CI(Sty_Eilat_genocoeffs_50$Estimate, ci=0.95))
Sty_Eilat_coeff_lower<-Sty_Eilat_summary[3,]
Sty_Eilat_coeff_upper<-Sty_Eilat_summary[1,]

#### Sty - AlWajh ####

Sty_AlWajh_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="AlWajh",], fct = LL.3())
summary(Sty_AlWajh_pop)
plot(Sty_AlWajh_pop)

Sty_AlWajh <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="AlWajh",], curveid=Geno, fct = LL.3())
summary(Sty_AlWajh)
plot(Sty_AlWajh)

#extract coeffs by geno, then compute 95% CIs
Sty_AlWajh_genocoeffs_50<-data.frame(ED(Sty_AlWajh, c(50)))
Sty_AlWajh_coeff_mean<-mean(Sty_AlWajh_genocoeffs_50$Estimate)
Sty_AlWajh_coeff_mean

Sty_AlWajh_summary<-data.frame(CI(Sty_AlWajh_genocoeffs_50$Estimate, ci=0.95))
Sty_AlWajh_coeff_lower<-Sty_AlWajh_summary[3,]
Sty_AlWajh_coeff_upper<-Sty_AlWajh_summary[1,]

#### Sty - Yanbu ####

Sty_Yanbu_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Yanbu",], fct = LL.3())
summary(Sty_Yanbu_pop)
plot(Sty_Yanbu_pop)

Sty_Yanbu <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Yanbu",], curveid=Geno, fct = LL.3())
summary(Sty_Yanbu)
plot(Sty_Yanbu)

#extract coeffs by geno, then compute 95% CIs
Sty_Yanbu_genocoeffs_50<-data.frame(ED(Sty_Yanbu, c(50)))
Sty_Yanbu_coeff_mean<-mean(Sty_Yanbu_genocoeffs_50$Estimate)
Sty_Yanbu_coeff_mean

Sty_Yanbu_summary<-data.frame(CI(Sty_Yanbu_genocoeffs_50$Estimate, ci=0.95))
Sty_Yanbu_coeff_lower<-Sty_Yanbu_summary[3,]
Sty_Yanbu_coeff_upper<-Sty_Yanbu_summary[1,]

#### Sty - AlFahal ####

Sty_AlFahal_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="AlFahal",], fct = LL.3())
summary(Sty_AlFahal_pop)
plot(Sty_AlFahal_pop)

Sty_AlFahal <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="AlFahal",], curveid=Geno, fct = LL.3())
summary(Sty_AlFahal)
plot(Sty_AlFahal)

#extract coeffs by geno, then compute 95% CIs
Sty_AlFahal_genocoeffs_50<-data.frame(ED(Sty_AlFahal, c(50)))
Sty_AlFahal_coeff_mean<-mean(Sty_AlFahal_genocoeffs_50$Estimate)
Sty_AlFahal_coeff_mean

Sty_AlFahal_summary<-data.frame(CI(Sty_AlFahal_genocoeffs_50$Estimate, ci=0.95))
Sty_AlFahal_coeff_lower<-Sty_AlFahal_summary[3,]
Sty_AlFahal_coeff_upper<-Sty_AlFahal_summary[1,]

#### Sty - Obock ####

Sty_Obock_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Obock",], fct = LL.3())
summary(Sty_Obock_pop)
plot(Sty_Obock_pop)

Sty_Obock <- drm(Median ~ Temp, data = Sty_data[Sty_data$Site=="Obock",], curveid=Geno, fct = LL.3())
summary(Sty_Obock)
plot(Sty_Obock)

#extract coeffs by geno, then compute 95% CIs
Sty_Obock_genocoeffs_50<-data.frame(ED(Sty_Obock, c(50)))
Sty_Obock_coeff_mean<-mean(Sty_Obock_genocoeffs_50$Estimate)
Sty_Obock_coeff_mean

Sty_Obock_summary<-data.frame(CI(Sty_Obock_genocoeffs_50$Estimate, ci=0.95))
Sty_Obock_coeff_lower<-Sty_Obock_summary[3,]
Sty_Obock_coeff_upper<-Sty_Obock_summary[1,]

############################################################################################
#### Combine genotpye-ED50s into dataframe for statistical analysis ####
############################################################################################

Sty_Geno_ED50s <- data.frame(cbind(Sty_Obock_genocoeffs_50[,1],Sty_Eilat_genocoeffs_50[,1],Sty_AlWajh_genocoeffs_50[,1],
                               Sty_Yanbu_genocoeffs_50[,1], Sty_AlFahal_genocoeffs_50[,1]))

Sty_Geno_ED50s<-Sty_Geno_ED50s %>% 
  dplyr::rename(Obock= X1,
         Eilat=X2,
         AlWajh=X3,
         Yanbu=X4,
         AlFahal=X5)

Sty_Geno_ED50s$Geno<-as.factor(1:nrow(Sty_Geno_ED50s))
str(Sty_Geno_ED50s)

Sty_Geno_ED50s_long<-melt(Sty_Geno_ED50s, id="Geno")

Sty_Geno_ED50s_long<-Sty_Geno_ED50s_long %>% 
  dplyr::rename(Site= variable,
         ED50=value)

Sty_ED50_mod<-aov(ED50 ~ Site, Sty_Geno_ED50s_long)
summary(Sty_ED50_mod)
TukeyHSD(Sty_ED50_mod)

############################################################################################
#### Combine ED50 data plus predict curves from models for plotting ####
############################################################################################

Sty_coeff_means<-data.frame(Sty_AlWajh_coeff_mean, Sty_Eilat_coeff_mean, Sty_AlFahal_coeff_mean, Sty_Obock_coeff_mean, Sty_Yanbu_coeff_mean)
Sty_coeff_lowers<-data.frame(Sty_AlWajh_coeff_lower, Sty_Eilat_coeff_lower, Sty_AlFahal_coeff_lower, Sty_Obock_coeff_lower, Sty_Yanbu_coeff_lower)
Sty_coeff_uppers<-data.frame(Sty_AlWajh_coeff_upper, Sty_Eilat_coeff_upper, Sty_AlFahal_coeff_upper, Sty_Obock_coeff_upper, Sty_Yanbu_coeff_upper)

Sty_AlWajh_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_AlWajh_pred = as.data.frame(predict(Sty_AlWajh_pop, newdata = Sty_AlWajh_preddata, interval = 'confidence'))
Sty_AlWajh_preddata = data.frame(Sty_AlWajh_preddata, Median = Sty_AlWajh_pred$Prediction, Lower = Sty_AlWajh_pred$Lower, Upper = Sty_AlWajh_pred$Upper)

Sty_Eilat_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_Eilat_pred = as.data.frame(predict(Sty_Eilat_pop, newdata = Sty_Eilat_preddata, interval = 'confidence'))
Sty_Eilat_preddata = data.frame(Sty_Eilat_preddata, Median = Sty_Eilat_pred$Prediction, Lower = Sty_Eilat_pred$Lower, Upper = Sty_Eilat_pred$Upper)

Sty_AlFahal_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_AlFahal_pred = as.data.frame(predict(Sty_AlFahal_pop, newdata = Sty_AlFahal_preddata, interval = 'confidence'))
Sty_AlFahal_preddata = data.frame(Sty_AlFahal_preddata, Median = Sty_AlFahal_pred$Prediction, Lower = Sty_AlFahal_pred$Lower, Upper = Sty_AlFahal_pred$Upper)

Sty_Obock_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_Obock_pred = as.data.frame(predict(Sty_Obock_pop, newdata = Sty_Obock_preddata, interval = 'confidence'))
Sty_Obock_preddata = data.frame(Sty_Obock_preddata, Median = Sty_Obock_pred$Prediction, Lower = Sty_Obock_pred$Lower, Upper = Sty_Obock_pred$Upper)

Sty_Yanbu_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_Yanbu_pred = as.data.frame(predict(Sty_Yanbu_pop, newdata = Sty_Yanbu_preddata, interval = 'confidence'))
Sty_Yanbu_preddata = data.frame(Sty_Yanbu_preddata, Median = Sty_Yanbu_pred$Prediction, Lower = Sty_Yanbu_pred$Lower, Upper = Sty_Yanbu_pred$Upper)

#### PLOT Stylophora ####
Sty_data$Temp<-as.character(Sty_data$Temp)
Sty_data$Temp<-as.numeric(Sty_data$Temp)

Sty_plot<- ggplot() +
  geom_jitter(data = Sty_data, aes(x = Temp, y = Median, color = Site), size = 1, width = 0.25) +
  scale_x_continuous(limits=c(27,42), breaks=c(28,30,32,34,36,38,40)) +
  scale_y_continuous(limits=c(-47, 155), breaks=c(0, 20, 40, 60, 80, 100)) +
  
  geom_line(data = Sty_Eilat_preddata, aes(x = temp, y = Median), color = 'royalblue2', show.legend = FALSE) +
  geom_ribbon(data = Sty_Eilat_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue2', linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_Eilat_coeff_mean), color = 'royalblue2', show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_Eilat_coeff_lower, xmax=Sty_coeff_uppers$Sty_Eilat_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue2',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_Eilat_coeff_mean, digits = 2)), x = 31, y = 30, show.legend = FALSE, color = 'royalblue2') +
  
  geom_line(data = Sty_AlWajh_preddata, aes(x = temp, y = Median), color = 'darkgoldenrod1', show.legend = FALSE) +
  geom_ribbon(data = Sty_AlWajh_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_AlWajh_coeff_mean), color = 'darkgoldenrod1', show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_AlWajh_coeff_lower, xmax=Sty_coeff_uppers$Sty_AlWajh_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkgoldenrod1',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_AlWajh_coeff_mean, digits = 2)), x = 31, y = 25, show.legend = FALSE, color = 'darkgoldenrod1') +
  
  geom_line(data = Sty_Yanbu_preddata, aes(x = temp, y = Median), color = 'darkorange1', show.legend = FALSE) +
  geom_ribbon(data = Sty_Yanbu_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange1', linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_Yanbu_coeff_mean), color = 'darkorange1', show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_Yanbu_coeff_lower, xmax=Sty_coeff_uppers$Sty_Yanbu_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange1',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_Yanbu_coeff_mean, digits = 2)), x = 31, y = 20, show.legend = FALSE, color = 'darkorange1') +
  
  geom_line(data = Sty_AlFahal_preddata, aes(x = temp, y = Median), color = 'red3', show.legend = FALSE) +
  geom_ribbon(data = Sty_AlFahal_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'red3', linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_AlFahal_coeff_mean), color = 'red3', show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_AlFahal_coeff_lower, xmax=Sty_coeff_uppers$Sty_AlFahal_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'red3',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_AlFahal_coeff_mean, digits = 2)), x = 31, y = 15, show.legend = FALSE, color = 'red3') +
  
  geom_line(data = Sty_Obock_preddata, aes(x = temp, y = Median), color = 'springgreen1', show.legend = FALSE) +
  geom_ribbon(data = Sty_Obock_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen1', linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_Obock_coeff_mean), color = 'springgreen1', show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_Obock_coeff_lower, xmax=Sty_coeff_uppers$Sty_Obock_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen1',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_Obock_coeff_mean, digits = 2)), x = 31, y = 10, show.legend = FALSE, color = 'springgreen1') +
  
  ggtitle("S. pistillata") +
  scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "springgreen1")) +
  ylab("Percent pigmentation") +
  xlab("Temperature (°C)") +
  theme_bw()

Sty_plot

######################################## 
        #### Porites DRCs ####
######################################## 

#### Por - AlQunfudhah ####

Por_AlQ_Medians<-Por_data[Por_data$Site=="AlQunfudhah",]
Por_AlQ_Medians<-subset(Por_AlQ_Medians, Geno=="2" | Por_AlQ_Medians$Geno=="4" | Por_AlQ_Medians$Geno=="5" | Por_AlQ_Medians$Geno=="7")
#Remove genotypes with poor fit

Por_AlQunfudhah_pop <- drm(Median ~ Temp, data = Por_AlQ_Medians, fct = LL.3())
summary(Por_AlQunfudhah_pop)
plot(Por_AlQunfudhah_pop)

Por_AlQunfudhah <- drm(Median ~ Temp, data = Por_AlQ_Medians, curveid=Geno, fct = LL.3())
summary(Por_AlQunfudhah)
plot(Por_AlQunfudhah)

#extract coeffs by geno, then compute 95% CIs
Por_AlQunfudhah_genocoeffs_50<-data.frame(ED(Por_AlQunfudhah, c(50)))
Por_AlQunfudhah_coeff_mean<-mean(Por_AlQunfudhah_genocoeffs_50$Estimate)
Por_AlQunfudhah_coeff_mean

Por_AlQunfudhah_summary<-data.frame(CI(Por_AlQunfudhah_genocoeffs_50$Estimate, ci=0.95))
Por_AlQunfudhah_coeff_lower<-Por_AlQunfudhah_summary[3,]
Por_AlQunfudhah_coeff_upper<-Por_AlQunfudhah_summary[1,]

#### Por - AlWajh ####
Por_AlWajh_pop <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="AlWajh",], fct = LL.3())
summary(Por_AlWajh_pop)
plot(Por_AlWajh_pop)

Por_AlWajh <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="AlWajh",], curveid=Geno, fct = LL.3())
summary(Por_AlWajh)
plot(Por_AlWajh)

#extract coeffs by geno, then compute 95% CIs
Por_AlWajh_genocoeffs_50<-data.frame(ED(Por_AlWajh, c(50)))
Por_AlWajh_coeff_mean<-mean(Por_AlWajh_genocoeffs_50$Estimate)
Por_AlWajh_coeff_mean

Por_AlWajh_summary<-data.frame(CI(Por_AlWajh_genocoeffs_50$Estimate, ci=0.95))
Por_AlWajh_coeff_lower<-Por_AlWajh_summary[3,]
Por_AlWajh_coeff_upper<-Por_AlWajh_summary[1,]

#### Por - Yanbu ####
Por_Yanbu_pop <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="Yanbu",], fct = LL.3())
summary(Por_Yanbu_pop)
plot(Por_Yanbu_pop)

Por_Yanbu <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="Yanbu",], curveid=Geno, fct = LL.3())
summary(Por_Yanbu)
plot(Por_Yanbu)

#extract coeffs by geno, then compute 95% CIs
Por_Yanbu_genocoeffs_50<-data.frame(ED(Por_Yanbu, c(50)))
Por_Yanbu_coeff_mean<-mean(Por_Yanbu_genocoeffs_50$Estimate)
Por_Yanbu_coeff_mean

Por_Yanbu_summary<-data.frame(CI(Por_Yanbu_genocoeffs_50$Estimate, ci=0.95))
Por_Yanbu_coeff_lower<-Por_Yanbu_summary[3,]
Por_Yanbu_coeff_upper<-Por_Yanbu_summary[1,]

#### Por - AlFahal ####

Por_AlFahal_Medians<-Por_data[Por_data$Site=="AlFahal",]
Por_AlFahal_Medians<-subset(Por_AlFahal_Medians, Geno=="1" | Geno=="3" | Geno=="4" | Geno=="5" | Geno=="6" | Geno=="7")

Por_AlFahal_pop <- drm(Median ~ Temp, data = Por_AlFahal_Medians, fct = LL.3())
summary(Por_AlFahal_pop)
plot(Por_AlFahal_pop)

Por_AlFahal <- drm(Median ~ Temp, data = Por_AlFahal_Medians, curveid=Geno, fct = LL.3())
summary(Por_AlFahal)
plot(Por_AlFahal)

#extract coeffs by geno, then compute 95% CIs
Por_AlFahal_genocoeffs_50<-data.frame(ED(Por_AlFahal, c(50)))
Por_AlFahal_coeff_mean<-mean(Por_AlFahal_genocoeffs_50$Estimate)
Por_AlFahal_coeff_mean

Por_AlFahal_summary<-data.frame(CI(Por_AlFahal_genocoeffs_50$Estimate, ci=0.95))
Por_AlFahal_coeff_lower<-Por_AlFahal_summary[3,]
Por_AlFahal_coeff_upper<-Por_AlFahal_summary[1,]

#### Por - Obock ####
Por_Obock_pop <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="Obock",], fct = LL.3())
summary(Por_Obock_pop)
plot(Por_Obock_pop)

Por_Obock <- drm(Median ~ Temp, data = Por_data[Por_data$Site=="Obock",], curveid=Geno, fct = LL.3())
summary(Por_Obock)
plot(Por_Obock)

#extract coeffs by geno, then compute 95% CIs
Por_Obock_genocoeffs_50<-data.frame(ED(Por_Obock, c(50)))
Por_Obock_coeff_mean<-mean(Por_Obock_genocoeffs_50$Estimate)
Por_Obock_coeff_mean

Por_Obock_summary<-data.frame(CI(Por_Obock_genocoeffs_50$Estimate, ci=0.95))
Por_Obock_coeff_lower<-Por_Obock_summary[3,]
Por_Obock_coeff_upper<-Por_Obock_summary[1,]

############################################################################################
#### Combine genotpye-ED50s into dataframe for statistical analysis ####
############################################################################################
Por_Geno_ED50s<-merge(Por_Obock_genocoeffs_50[,1],Por_AlQunfudhah_genocoeffs_50[,1], by=0, all=TRUE)
Por_Geno_ED50s<-Por_Geno_ED50s %>% 
  dplyr::rename(Obock= x,
                AlQunfudhah=y)

Por_Geno_ED50s<-merge(Por_Geno_ED50s[,2:3],Por_AlWajh_genocoeffs_50[,1], by=0, all=TRUE)
Por_Geno_ED50s<-merge(Por_Geno_ED50s[,2:4],Por_Yanbu_genocoeffs_50[,1], by=0, all=TRUE)
Por_Geno_ED50s<-merge(Por_Geno_ED50s[,2:5],Por_AlFahal_genocoeffs_50[,1], by=0, all=TRUE)

Por_Geno_ED50s<-Por_Geno_ED50s %>% 
  dplyr::rename(Obock= Obock,
         AlQunfudhah=AlQunfudhah,
         AlWajh=y.x,
         Yanbu=y.y,
         AlFahal=y)

Por_Geno_ED50s<-Por_Geno_ED50s[,2:6]

Por_Geno_ED50s$Geno<-as.factor(1:nrow(Por_Geno_ED50s))
str(Por_Geno_ED50s)

Por_Geno_ED50s_long<-melt(Por_Geno_ED50s, id="Geno")

Por_Geno_ED50s_long<-Por_Geno_ED50s_long %>% 
  dplyr::rename(Site= variable,
         ED50=value)

Por_ED50_mod<-aov(ED50 ~ Site, Por_Geno_ED50s_long)
summary(Por_ED50_mod)
TukeyHSD(Por_ED50_mod)

############################################################################################
#### Combine ED50 data plus predict curves from models for plotting ####
############################################################################################

Por_coeff_means<-data.frame(Por_AlWajh_coeff_mean, Por_AlQunfudhah_coeff_mean, Por_AlFahal_coeff_mean, Por_Obock_coeff_mean, Por_Yanbu_coeff_mean)
Por_coeff_lowers<-data.frame(Por_AlWajh_coeff_lower, Por_AlQunfudhah_coeff_lower, Por_AlFahal_coeff_lower, Por_Obock_coeff_lower, Por_Yanbu_coeff_lower)
Por_coeff_uppers<-data.frame(Por_AlWajh_coeff_upper, Por_AlQunfudhah_coeff_upper, Por_AlFahal_coeff_upper, Por_Obock_coeff_upper, Por_Yanbu_coeff_upper)

Por_AlWajh_preddata = data.frame(temp = seq(30,39, length.out = 100))
Por_AlWajh_pred = as.data.frame(predict(Por_AlWajh_pop, newdata = Por_AlWajh_preddata, interval = 'confidence'))
Por_AlWajh_preddata = data.frame(Por_AlWajh_preddata, Median = Por_AlWajh_pred$Prediction, Lower = Por_AlWajh_pred$Lower, Upper = Por_AlWajh_pred$Upper)

Por_AlQunfudhah_preddata = data.frame(temp = seq(30,39, length.out = 100))
Por_AlQunfudhah_pred = as.data.frame(predict(Por_AlQunfudhah_pop, newdata = Por_AlQunfudhah_preddata, interval = 'confidence'))
Por_AlQunfudhah_preddata = data.frame(Por_AlQunfudhah_preddata, Median = Por_AlQunfudhah_pred$Prediction, Lower = Por_AlQunfudhah_pred$Lower, Upper = Por_AlQunfudhah_pred$Upper)

Por_AlFahal_preddata = data.frame(temp = seq(30,39, length.out = 100))
Por_AlFahal_pred = as.data.frame(predict(Por_AlFahal_pop, newdata = Por_AlFahal_preddata, interval = 'confidence'))
Por_AlFahal_preddata = data.frame(Por_AlFahal_preddata, Median = Por_AlFahal_pred$Prediction, Lower = Por_AlFahal_pred$Lower, Upper = Por_AlFahal_pred$Upper)

Por_Obock_preddata = data.frame(temp = seq(30,39, length.out = 100))
Por_Obock_pred = as.data.frame(predict(Por_Obock_pop, newdata = Por_Obock_preddata, interval = 'confidence'))
Por_Obock_preddata = data.frame(Por_Obock_preddata, Median = Por_Obock_pred$Prediction, Lower = Por_Obock_pred$Lower, Upper = Por_Obock_pred$Upper)

Por_Yanbu_preddata = data.frame(temp = seq(30,39, length.out = 100))
Por_Yanbu_pred = as.data.frame(predict(Por_Yanbu_pop, newdata = Por_Yanbu_preddata, interval = 'confidence'))
Por_Yanbu_preddata = data.frame(Por_Yanbu_preddata, Median = Por_Yanbu_pred$Prediction, Lower = Por_Yanbu_pred$Lower, Upper = Por_Yanbu_pred$Upper)

#### PLOT Porites ####
Por_data$Temp<-as.character(Por_data$Temp)
Por_data$Temp<-as.numeric(Por_data$Temp)

levels(Por_data$Site)
Por_data$Site = factor(Por_data$Site,levels(Por_data$Site)[c(3,6,1,2,5)])

Por_plot<- ggplot() +
  geom_jitter(data = Por_data, aes(x = Temp, y = Median, color = Site), size = 1, width = 0.25) +
  scale_x_continuous(limits=c(29,40), breaks=c(30,32,34,36,38)) +
  scale_y_continuous(limits=c(-5, 125), breaks=c(0, 20, 40, 60, 80, 100)) +
  
  geom_line(data = Por_AlQunfudhah_preddata, aes(x = temp, y = Median), color = 'darkorchid4', show.legend = FALSE) +
  geom_ribbon(data = Por_AlQunfudhah_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid4', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_coeff_means, aes(xintercept = Por_AlQunfudhah_coeff_mean), color = 'darkorchid4', show.legend = FALSE) +
  annotate("rect", xmin=Por_coeff_lowers$Por_AlQunfudhah_coeff_lower, xmax=Por_coeff_uppers$Por_AlQunfudhah_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid4',  alpha = 0.1) +
  geom_text(data = Por_coeff_means, aes(label=round(Por_AlQunfudhah_coeff_mean, digits = 2)), x = 31, y = 40, show.legend = FALSE, color = 'darkorchid4') +
  
  geom_line(data = Por_AlWajh_preddata, aes(x = temp, y = Median), color = 'darkgoldenrod1', show.legend = FALSE) +
  geom_ribbon(data = Por_AlWajh_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_coeff_means, aes(xintercept = Por_AlWajh_coeff_mean), color = 'darkgoldenrod1', show.legend = FALSE) +
  annotate("rect", xmin=Por_coeff_lowers$Por_AlWajh_coeff_lower, xmax=Por_coeff_uppers$Por_AlWajh_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkgoldenrod1',  alpha = 0.1) +
  geom_text(data = Por_coeff_means, aes(label=round(Por_AlWajh_coeff_mean, digits = 2)), x = 31, y = 30, show.legend = FALSE, color = 'darkgoldenrod1') +
  
  geom_line(data = Por_Yanbu_preddata, aes(x = temp, y = Median), color = 'darkorange1', show.legend = FALSE) +
  geom_ribbon(data = Por_Yanbu_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_coeff_means, aes(xintercept = Por_Yanbu_coeff_mean), color = 'darkorange1', show.legend = FALSE) +
  annotate("rect", xmin=Por_coeff_lowers$Por_Yanbu_coeff_lower, xmax=Por_coeff_uppers$Por_Yanbu_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange1',  alpha = 0.1) +
  geom_text(data = Por_coeff_means, aes(label=round(Por_Yanbu_coeff_mean, digits = 2)), x = 31, y = 20, show.legend = FALSE, color = 'darkorange1') +
  
  geom_line(data = Por_AlFahal_preddata, aes(x = temp, y = Median), color = 'red3', show.legend = FALSE) +
  geom_ribbon(data = Por_AlFahal_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'red3', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_coeff_means, aes(xintercept = Por_AlFahal_coeff_mean), color = 'red3', show.legend = FALSE) +
  annotate("rect", xmin=Por_coeff_lowers$Por_AlFahal_coeff_lower, xmax=Por_coeff_uppers$Por_AlFahal_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'red3',  alpha = 0.1) +
  geom_text(data = Por_coeff_means, aes(label=round(Por_AlFahal_coeff_mean, digits = 2)), x = 31, y = 10, show.legend = FALSE, color = 'red3') +
  
  geom_line(data = Por_Obock_preddata, aes(x = temp, y = Median), color = 'springgreen1', show.legend = FALSE) +
  geom_ribbon(data = Por_Obock_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_coeff_means, aes(xintercept = Por_Obock_coeff_mean), color = 'springgreen1', show.legend = FALSE) +
  annotate("rect", xmin=Por_coeff_lowers$Por_Obock_coeff_lower, xmax=Por_coeff_uppers$Por_Obock_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen1',  alpha = 0.1) +
  geom_text(data = Por_coeff_means, aes(label=round(Por_Obock_coeff_mean, digits = 2)), x = 31, y = 0, show.legend = FALSE, color = 'springgreen1') +
  
  ggtitle("P. lobata") +
  scale_color_manual(values=c("darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")) +
  ylab("Percent pigmentation") +
  xlab("Temperature (°C)") +
  theme_bw()

Por_plot

######################################## 
#### Acropora DRCs ####
######################################## 

#### Acr - AlWajh ####
Acr_AlWajh_pop <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="AlWajh",], fct = LL.3())
summary(Acr_AlWajh_pop)
plot(Acr_AlWajh_pop)

Acr_AlWajh <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="AlWajh",], curveid=Geno, fct = LL.3())
summary(Acr_AlWajh)
plot(Acr_AlWajh)

#extract coeffs by geno, then compute 95% CIs
Acr_AlWajh_genocoeffs_50<-data.frame(ED(Acr_AlWajh, c(50)))
Acr_AlWajh_coeff_mean<-mean(Acr_AlWajh_genocoeffs_50$Estimate)
Acr_AlWajh_coeff_mean

Acr_AlWajh_summary<-data.frame(CI(Acr_AlWajh_genocoeffs_50$Estimate, ci=0.95))
Acr_AlWajh_coeff_lower<-Acr_AlWajh_summary[3,]
Acr_AlWajh_coeff_upper<-Acr_AlWajh_summary[1,]

#### Acr - Yanbu ####
Acr_Yanbu_pop <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="Yanbu",], fct = LL.3())
summary(Acr_Yanbu_pop)
plot(Acr_Yanbu_pop)

Acr_Yanbu <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="Yanbu",], curveid=Geno, fct = LL.3())
summary(Acr_Yanbu)
plot(Acr_Yanbu)

#extract coeffs by geno, then compute 95% CIs
Acr_Yanbu_genocoeffs_50<-data.frame(ED(Acr_Yanbu, c(50)))
Acr_Yanbu_coeff_mean<-mean(Acr_Yanbu_genocoeffs_50$Estimate)
Acr_Yanbu_coeff_mean

Acr_Yanbu_summary<-data.frame(CI(Acr_Yanbu_genocoeffs_50$Estimate, ci=0.95))
Acr_Yanbu_coeff_lower<-Acr_Yanbu_summary[3,]
Acr_Yanbu_coeff_upper<-Acr_Yanbu_summary[1,]

#### Acr - AlFahal ####

Acr_AlFahal_pop <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="AlFahal",], fct = LL.3())
summary(Acr_AlFahal_pop)
plot(Acr_AlFahal_pop)

Acr_AlFahal <- drm(Median ~ Temp, data = Acr_data[Acr_data$Site=="AlFahal",], curveid=Geno, fct = LL.3())
summary(Acr_AlFahal)
plot(Acr_AlFahal)

#extract coeffs by geno, then compute 95% CIs
Acr_AlFahal_genocoeffs_50<-data.frame(ED(Acr_AlFahal, c(50)))
Acr_AlFahal_coeff_mean<-mean(Acr_AlFahal_genocoeffs_50$Estimate)
Acr_AlFahal_coeff_mean

Acr_AlFahal_summary<-data.frame(CI(Acr_AlFahal_genocoeffs_50$Estimate, ci=0.95))
Acr_AlFahal_coeff_lower<-Acr_AlFahal_summary[3,]
Acr_AlFahal_coeff_upper<-Acr_AlFahal_summary[1,]

############################################################################################
#### Combine genotpye-ED50s into dataframe for statistical analysis ####
############################################################################################

Acr_Geno_ED50s <- data.frame(cbind(Acr_AlWajh_genocoeffs_50[,1],
                               Acr_Yanbu_genocoeffs_50[,1], Acr_AlFahal_genocoeffs_50[,1]))

Acr_Geno_ED50s<-Acr_Geno_ED50s %>% 
  dplyr::rename(AlWajh=X1,
                Yanbu=X2,
                AlFahal=X3)

Acr_Geno_ED50s$Geno<-as.factor(1:nrow(Acr_Geno_ED50s))
str(Acr_Geno_ED50s)

Acr_Geno_ED50s_long<-melt(Acr_Geno_ED50s, id="Geno")

Acr_Geno_ED50s_long<-Acr_Geno_ED50s_long %>% 
  dplyr::rename(Site= variable,
         ED50=value)

Acr_ED50_mod<-aov(ED50 ~ Site, Acr_Geno_ED50s_long)
summary(Acr_ED50_mod)
TukeyHSD(Acr_ED50_mod)

############################################################################################
#### Combine ED50 data plus predict curves from models for plotting ####
############################################################################################

Acr_coeff_means<-data.frame(Acr_AlWajh_coeff_mean, Acr_AlFahal_coeff_mean, Acr_Yanbu_coeff_mean)
Acr_coeff_lowers<-data.frame(Acr_AlWajh_coeff_lower, Acr_AlFahal_coeff_lower, Acr_Yanbu_coeff_lower)
Acr_coeff_uppers<-data.frame(Acr_AlWajh_coeff_upper, Acr_AlFahal_coeff_upper, Acr_Yanbu_coeff_upper)

Acr_AlWajh_preddata = data.frame(temp = seq(30,39, length.out = 100))
Acr_AlWajh_pred = as.data.frame(predict(Acr_AlWajh_pop, newdata = Acr_AlWajh_preddata, interval = 'confidence'))
Acr_AlWajh_preddata = data.frame(Acr_AlWajh_preddata, Median = Acr_AlWajh_pred$Prediction, Lower = Acr_AlWajh_pred$Lower, Upper = Acr_AlWajh_pred$Upper)

Acr_AlFahal_preddata = data.frame(temp = seq(30,39, length.out = 100))
Acr_AlFahal_pred = as.data.frame(predict(Acr_AlFahal_pop, newdata = Acr_AlFahal_preddata, interval = 'confidence'))
Acr_AlFahal_preddata = data.frame(Acr_AlFahal_preddata, Median = Acr_AlFahal_pred$Prediction, Lower = Acr_AlFahal_pred$Lower, Upper = Acr_AlFahal_pred$Upper)

Acr_Yanbu_preddata = data.frame(temp = seq(30,39, length.out = 100))
Acr_Yanbu_pred = as.data.frame(predict(Acr_Yanbu_pop, newdata = Acr_Yanbu_preddata, interval = 'confidence'))
Acr_Yanbu_preddata = data.frame(Acr_Yanbu_preddata, Median = Acr_Yanbu_pred$Prediction, Lower = Acr_Yanbu_pred$Lower, Upper = Acr_Yanbu_pred$Upper)

#### PLOT Acropora ####
Acr_data$Temp<-as.character(Acr_data$Temp)
Acr_data$Temp<-as.numeric(Acr_data$Temp)

levels(Acr_data$Site)
Acr_data$Site = factor(Acr_data$Site,levels(Acr_data$Site)[c(4,3,6,1)]) 

Acr_plot<- ggplot() +
  geom_jitter(data = Acr_data, aes(x = Temp, y = Median, color = Site), size = 1, width = 0.25) +
  scale_x_continuous(limits=c(29,40), breaks=c(30,32,34,36,38)) +
  scale_y_continuous(limits=c(-5, 125), breaks=c(0, 20, 40, 60, 80, 100)) +
  
  geom_line(data = Acr_AlWajh_preddata, aes(x = temp, y = Median), color = 'darkgoldenrod1', show.legend = FALSE) +
  geom_ribbon(data = Acr_AlWajh_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_coeff_means, aes(xintercept = Acr_AlWajh_coeff_mean), color = 'darkgoldenrod1', show.legend = FALSE) +
  annotate("rect", xmin=Acr_coeff_lowers$Acr_AlWajh_coeff_lower, xmax=Acr_coeff_uppers$Acr_AlWajh_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkgoldenrod1',  alpha = 0.1) +
  geom_text(data = Acr_coeff_means, aes(label=round(Acr_AlWajh_coeff_mean, digits = 2)), x = 31, y = 25, show.legend = FALSE, color = 'darkgoldenrod1') +
  
  geom_line(data = Acr_Yanbu_preddata, aes(x = temp, y = Median), color = 'darkorange1', show.legend = FALSE) +
  geom_ribbon(data = Acr_Yanbu_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange1', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_coeff_means, aes(xintercept = Acr_Yanbu_coeff_mean), color = 'darkorange1', show.legend = FALSE) +
  annotate("rect", xmin=Acr_coeff_lowers$Acr_Yanbu_coeff_lower, xmax=Acr_coeff_uppers$Acr_Yanbu_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange1',  alpha = 0.1) +
  geom_text(data = Acr_coeff_means, aes(label=round(Acr_Yanbu_coeff_mean, digits = 2)), x = 31, y = 20, show.legend = FALSE, color = 'darkorange1') +
  
  geom_line(data = Acr_AlFahal_preddata, aes(x = temp, y = Median), color = 'red3', show.legend = FALSE) +
  geom_ribbon(data = Acr_AlFahal_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'red3', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_coeff_means, aes(xintercept = Acr_AlFahal_coeff_mean), color = 'red3', show.legend = FALSE) +
  annotate("rect", xmin=Acr_coeff_lowers$Acr_AlFahal_coeff_lower, xmax=Acr_coeff_uppers$Acr_AlFahal_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'red3',  alpha = 0.1) +
  geom_text(data = Acr_coeff_means, aes(label=round(Acr_AlFahal_coeff_mean, digits = 2)), x = 31, y = 15, show.legend = FALSE, color = 'red3') +
  
  ggtitle("A. hemprichii") +
  scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3")) +
  ylab("Percent pigmentation") +
  xlab("Temperature (°C)") +
  theme_bw()

Acr_plot

######################################## 
#### Pocillopora DRCs ####
######################################## 

#### Poc - Eilat ####
Poc_Eilat_pop <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Eilat",], fct = LL.3())
summary(Poc_Eilat_pop)
plot(Poc_Eilat_pop)

Poc_Eilat <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Eilat",], curveid=Geno, fct = LL.3())
summary(Poc_Eilat)
plot(Poc_Eilat)

#extract coeffs by geno, then compute 95% CIs
Poc_Eilat_genocoeffs_50<-data.frame(ED(Poc_Eilat, c(50)))
Poc_Eilat_coeff_mean<-mean(Poc_Eilat_genocoeffs_50$Estimate)
Poc_Eilat_coeff_mean

Poc_Eilat_summary<-data.frame(CI(Poc_Eilat_genocoeffs_50$Estimate, ci=0.95))
Poc_Eilat_coeff_lower<-Poc_Eilat_summary[3,]
Poc_Eilat_coeff_upper<-Poc_Eilat_summary[1,]

#### Poc - AlWajh ####
Poc_AlWajh_pop <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="AlWajh",], fct = LL.3())
summary(Poc_AlWajh_pop)
plot(Poc_AlWajh_pop)

Poc_AlWajh <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="AlWajh",], curveid=Geno, fct = LL.3())
summary(Poc_AlWajh)
plot(Poc_AlWajh)

#extract coeffs by geno, then compute 95% CIs
Poc_AlWajh_genocoeffs_50<-data.frame(ED(Poc_AlWajh, c(50)))
Poc_AlWajh_coeff_mean<-mean(Poc_AlWajh_genocoeffs_50$Estimate)
Poc_AlWajh_coeff_mean

Poc_AlWajh_summary<-data.frame(CI(Poc_AlWajh_genocoeffs_50$Estimate, ci=0.95))
Poc_AlWajh_coeff_lower<-Poc_AlWajh_summary[3,]
Poc_AlWajh_coeff_upper<-Poc_AlWajh_summary[1,]

#### Poc - Yanbu ####
Poc_Yanbu_pop <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Yanbu",], fct = LL.3())
summary(Poc_Yanbu_pop)
plot(Poc_Yanbu_pop)

Poc_Yanbu <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Yanbu",], curveid=Geno, fct = LL.3())
summary(Poc_Yanbu)
plot(Poc_Yanbu)

#extract coeffs by geno, then compute 95% CIs
Poc_Yanbu_genocoeffs_50<-data.frame(ED(Poc_Yanbu, c(50)))
Poc_Yanbu_coeff_mean<-mean(Poc_Yanbu_genocoeffs_50$Estimate)
Poc_Yanbu_coeff_mean

Poc_Yanbu_summary<-data.frame(CI(Poc_Yanbu_genocoeffs_50$Estimate, ci=0.95))
Poc_Yanbu_coeff_lower<-Poc_Yanbu_summary[3,]
Poc_Yanbu_coeff_upper<-Poc_Yanbu_summary[1,]

#### Poc - AlFahal ####
Poc_AlFahal_pop <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="AlFahal",], fct = LL.3())
summary(Poc_AlFahal_pop)
plot(Poc_AlFahal_pop)

Poc_AlFahal <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="AlFahal",], curveid=Geno, fct = LL.3())
summary(Poc_AlFahal)
plot(Poc_AlFahal)

#extract coeffs by geno, then compute 95% CIs
Poc_AlFahal_genocoeffs_50<-data.frame(ED(Poc_AlFahal, c(50)))
Poc_AlFahal_coeff_mean<-mean(Poc_AlFahal_genocoeffs_50$Estimate)
Poc_AlFahal_coeff_mean

Poc_AlFahal_summary<-data.frame(CI(Poc_AlFahal_genocoeffs_50$Estimate, ci=0.95))
Poc_AlFahal_coeff_lower<-Poc_AlFahal_summary[3,]
Poc_AlFahal_coeff_upper<-Poc_AlFahal_summary[1,]

#### Poc - Obock ####
Poc_Obock_pop <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Obock",], fct = LL.3())
summary(Poc_Obock_pop)
plot(Poc_Obock_pop)

Poc_Obock <- drm(Median ~ Temp, data = Poc_data[Poc_data$Site=="Obock",], curveid=Geno, fct = LL.3())
summary(Poc_Obock)
plot(Poc_Obock)

#extract coeffs by geno, then compute 95% CIs
Poc_Obock_genocoeffs_50<-data.frame(ED(Poc_Obock, c(50)))
Poc_Obock_coeff_mean<-mean(Poc_Obock_genocoeffs_50$Estimate)
Poc_Obock_coeff_mean

Poc_Obock_summary<-data.frame(CI(Poc_Obock_genocoeffs_50$Estimate, ci=0.95))
Poc_Obock_coeff_lower<-Poc_Obock_summary[3,]
Poc_Obock_coeff_upper<-Poc_Obock_summary[1,]

############################################################################################
#### Combine genotpye-ED50s into dataframe for statistical analysis ####
############################################################################################

Poc_Geno_ED50s <- data.frame(cbind(Poc_Obock_genocoeffs_50[,1],Poc_Eilat_genocoeffs_50[,1],Poc_AlWajh_genocoeffs_50[,1],
                               Poc_Yanbu_genocoeffs_50[,1], Poc_AlFahal_genocoeffs_50[,1]))

Poc_Geno_ED50s<-merge(Poc_Obock_genocoeffs_50[,1],Poc_Eilat_genocoeffs_50[,1], by=0, all=TRUE)
Poc_Geno_ED50s<-Poc_Geno_ED50s %>% 
  dplyr::rename(Obock= x,
                Eilat=y)

Poc_Geno_ED50s<-merge(Poc_Geno_ED50s[,2:3],Poc_AlWajh_genocoeffs_50[,1], by=0, all=TRUE)
Poc_Geno_ED50s<-merge(Poc_Geno_ED50s[,2:4],Poc_Yanbu_genocoeffs_50[,1], by=0, all=TRUE)
Poc_Geno_ED50s<-merge(Poc_Geno_ED50s[,2:5],Poc_AlFahal_genocoeffs_50[,1], by=0, all=TRUE)

Poc_Geno_ED50s<-Poc_Geno_ED50s %>% 
  dplyr::rename(Obock= Obock,
                Eilat=Eilat,
                AlWajh=y.x,
                Yanbu=y.y,
                AlFahal=y)

Poc_Geno_ED50s<-Poc_Geno_ED50s[,2:6]

Poc_Geno_ED50s$Geno<-as.factor(1:nrow(Poc_Geno_ED50s))
str(Poc_Geno_ED50s)

Poc_Geno_ED50s_long<-melt(Poc_Geno_ED50s, id="Geno")

Poc_Geno_ED50s_long<-Poc_Geno_ED50s_long %>% 
  dplyr::rename(Site= variable,
         ED50=value)

Poc_ED50_mod<-aov(ED50 ~ Site, Poc_Geno_ED50s_long)
summary(Poc_ED50_mod)
TukeyHSD(Poc_ED50_mod)

############################################################################################
#### Combine ED50 data plus predict curves from models for plotting ####
############################################################################################

Poc_coeff_means<-data.frame(Poc_AlWajh_coeff_mean, Poc_Eilat_coeff_mean, Poc_AlFahal_coeff_mean, Poc_Obock_coeff_mean, Poc_Yanbu_coeff_mean)
Poc_coeff_lowers<-data.frame(Poc_AlWajh_coeff_lower, Poc_Eilat_coeff_lower, Poc_AlFahal_coeff_lower, Poc_Obock_coeff_lower, Poc_Yanbu_coeff_lower)
Poc_coeff_uppers<-data.frame(Poc_AlWajh_coeff_upper, Poc_Eilat_coeff_upper, Poc_AlFahal_coeff_upper, Poc_Obock_coeff_upper, Poc_Yanbu_coeff_upper)

Poc_AlWajh_preddata = data.frame(temp = seq(30,39, length.out = 100))
Poc_AlWajh_pred = as.data.frame(predict(Poc_AlWajh_pop, newdata = Poc_AlWajh_preddata, interval = 'confidence'))
Poc_AlWajh_preddata = data.frame(Poc_AlWajh_preddata, Median = Poc_AlWajh_pred$Prediction, Lower = Poc_AlWajh_pred$Lower, Upper = Poc_AlWajh_pred$Upper)

Poc_Eilat_preddata = data.frame(temp = seq(30,39, length.out = 100))
Poc_Eilat_pred = as.data.frame(predict(Poc_Eilat_pop, newdata = Poc_Eilat_preddata, interval = 'confidence'))
Poc_Eilat_preddata = data.frame(Poc_Eilat_preddata, Median = Poc_Eilat_pred$Prediction, Lower = Poc_Eilat_pred$Lower, Upper = Poc_Eilat_pred$Upper)

Poc_AlFahal_preddata = data.frame(temp = seq(30,39, length.out = 100))
Poc_AlFahal_pred = as.data.frame(predict(Poc_AlFahal_pop, newdata = Poc_AlFahal_preddata, interval = 'confidence'))
Poc_AlFahal_preddata = data.frame(Poc_AlFahal_preddata, Median = Poc_AlFahal_pred$Prediction, Lower = Poc_AlFahal_pred$Lower, Upper = Poc_AlFahal_pred$Upper)

Poc_Obock_preddata = data.frame(temp = seq(30,39, length.out = 100))
Poc_Obock_pred = as.data.frame(predict(Poc_Obock_pop, newdata = Poc_Obock_preddata, interval = 'confidence'))
Poc_Obock_preddata = data.frame(Poc_Obock_preddata, Median = Poc_Obock_pred$Prediction, Lower = Poc_Obock_pred$Lower, Upper = Poc_Obock_pred$Upper)

Poc_Yanbu_preddata = data.frame(temp = seq(30,39, length.out = 100))
Poc_Yanbu_pred = as.data.frame(predict(Poc_Yanbu_pop, newdata = Poc_Yanbu_preddata, interval = 'confidence'))
Poc_Yanbu_preddata = data.frame(Poc_Yanbu_preddata, Median = Poc_Yanbu_pred$Prediction, Lower = Poc_Yanbu_pred$Lower, Upper = Poc_Yanbu_pred$Upper)

#### PLOT Pocillopora ####
Poc_data$Temp<-as.character(Poc_data$Temp)
Poc_data$Temp<-as.numeric(Poc_data$Temp)

levels(Poc_data$Site)
Poc_data$Site = factor(Poc_data$Site,levels(Poc_data$Site)[c(4,3,6,1,5)])

Poc_plot<- ggplot() +
  geom_jitter(data = Poc_data, aes(x = Temp, y = Median, color = Site), size = 1, width = 0.25) +
  scale_x_continuous(limits=c(29,40), breaks=c(30,32,34,36,38)) +
  scale_y_continuous(limits=c(-60, 151), breaks=c(0, 20, 40, 60, 80, 100)) +
  
  geom_line(data = Poc_Eilat_preddata, aes(x = temp, y = Median), color = 'royalblue2', show.legend = FALSE) +
  geom_ribbon(data = Poc_Eilat_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue2', linetype=2, alpha = 0.2) +
  geom_vline(data = Poc_coeff_means, aes(xintercept = Poc_Eilat_coeff_mean), color = 'royalblue2', show.legend = FALSE) +
  annotate("rect", xmin=Poc_coeff_lowers$Poc_Eilat_coeff_lower, xmax=Poc_coeff_uppers$Poc_Eilat_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue2',  alpha = 0.1) +
  geom_text(data = Poc_coeff_means, aes(label=round(Poc_Eilat_coeff_mean, digits = 2)), x = 31, y = 50, show.legend = FALSE, color = 'royalblue2') +
  
  geom_line(data = Poc_AlWajh_preddata, aes(x = temp, y = Median), color = 'darkgoldenrod1', show.legend = FALSE) +
  geom_ribbon(data = Poc_AlWajh_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.2) +
  geom_vline(data = Poc_coeff_means, aes(xintercept = Poc_AlWajh_coeff_mean), color = 'darkgoldenrod1', show.legend = FALSE) +
  annotate("rect", xmin=Poc_coeff_lowers$Poc_AlWajh_coeff_lower, xmax=Poc_coeff_uppers$Poc_AlWajh_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkgoldenrod1',  alpha = 0.1) +
  geom_text(data = Poc_coeff_means, aes(label=round(Poc_AlWajh_coeff_mean, digits = 2)), x = 31, y = 40, show.legend = FALSE, color = 'darkgoldenrod1') +
  
  geom_line(data = Poc_Yanbu_preddata, aes(x = temp, y = Median), color = 'darkorange1', show.legend = FALSE) +
  geom_ribbon(data = Poc_Yanbu_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange1', linetype=2, alpha = 0.2) +
  geom_vline(data = Poc_coeff_means, aes(xintercept = Poc_Yanbu_coeff_mean), color = 'darkorange1', show.legend = FALSE) +
  annotate("rect", xmin=Poc_coeff_lowers$Poc_Yanbu_coeff_lower, xmax=Poc_coeff_uppers$Poc_Yanbu_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange1',  alpha = 0.1) +
  geom_text(data = Poc_coeff_means, aes(label=round(Poc_Yanbu_coeff_mean, digits = 2)), x = 31, y = 30, show.legend = FALSE, color = 'darkorange1') +
  
  geom_line(data = Poc_AlFahal_preddata, aes(x = temp, y = Median), color = 'red3', show.legend = FALSE) +
  geom_ribbon(data = Poc_AlFahal_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'red3', linetype=2, alpha = 0.2) +
  geom_vline(data = Poc_coeff_means, aes(xintercept = Poc_AlFahal_coeff_mean), color = 'red3', show.legend = FALSE) +
  annotate("rect", xmin=Poc_coeff_lowers$Poc_AlFahal_coeff_lower, xmax=Poc_coeff_uppers$Poc_AlFahal_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'red3',  alpha = 0.1) +
  geom_text(data = Poc_coeff_means, aes(label=round(Poc_AlFahal_coeff_mean, digits = 2)), x = 31, y = 20, show.legend = FALSE, color = 'red3') +
  
  geom_line(data = Poc_Obock_preddata, aes(x = temp, y = Median), color = 'springgreen1', show.legend = FALSE) +
  geom_ribbon(data = Poc_Obock_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen1', linetype=2, alpha = 0.2) +
  geom_vline(data = Poc_coeff_means, aes(xintercept = Poc_Obock_coeff_mean), color = 'springgreen1', show.legend = FALSE) +
  annotate("rect", xmin=Poc_coeff_lowers$Poc_Obock_coeff_lower, xmax=Poc_coeff_uppers$Poc_Obock_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen1',  alpha = 0.1) +
  geom_text(data = Poc_coeff_means, aes(label=round(Poc_Obock_coeff_mean, digits = 2)), x = 31, y = 10, show.legend = FALSE, color = 'springgreen1') +
  
  ggtitle("P. verrucosa") +
  scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "springgreen1")) +
  ylab("Percent pigmentation") +
  xlab("Temperature (°C)") +
  theme_bw()

Poc_plot


#END
