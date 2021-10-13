
#Code for Figure S6 of "Empirically derived thermal thresholds of four coral species along the Red Sea"
#Statistical results are also summarized in Table S4

#setwd to source file location
library(ggplot2)
library(ggpmisc)

ED50s<-read.csv("ED50s.csv")

ED50s$Site<- as.factor(ED50s$Site)

Stylophora<-subset(ED50s, Species == 'Stylophora')
Porites<-subset(ED50s, Species == 'Porites')
Acropora<-subset(ED50s, Species == 'Acropora')
Pocillopora<-subset(ED50s, Species == 'Pocillopora')

#### Correlations between ED50s thermal thresholds (x) and Site MMMs (y) for each species
Stylophora_Corr <- cor.test(x = Stylophora$ED50, y = Stylophora$MMM, method = "pearson")
Stylophora_Corr #p.value = < 0.001, Cor = 0.61

Porites_Corr <- cor.test(x = Porites$ED50, y = Porites$MMM, method = "pearson")
Porites_Corr #p.value = 0.10, Cor = -0.28

Acropora_Corr <- cor.test(x = Acropora$ED50, y = Acropora$MMM, method = "pearson")
Acropora_Corr #p.value = 0.09, Cor = 0.33

Pocillopora_Corr <- cor.test(x = Pocillopora$ED50, y = Pocillopora$MMM, method = "pearson")
Pocillopora_Corr #p.value = < 0.001, Cor = 0.57

# plots

#Stylophora
print(levels(Stylophora$Site))
Stylophora$Site = factor(Stylophora$Site,levels(Stylophora$Site)[c(4,3,6,1,2,5)])

ggplot(Stylophora,aes(x=ED50,y=MMM,color=Site)) +
  theme_classic() +
  stat_smooth(method = "lm",formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE, colour="grey40")+
  geom_point(size = 5, alpha=0.5) +
  theme(legend.position = 'bottom')+
  ggtitle("S. pistillata") + xlab("ED50s") + ylab("Site MMM")+
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "grey20"),
        axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="grey20", size=10, face="bold"),
        legend.text = element_text(colour="grey20", size=10, face="plain")) + scale_shape_manual(values=c(16, 16)) +
  scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkgreen","darkorchid4"))

#Pocillopora
print(levels(Pocillopora$Site))
Pocillopora$Site = factor(Pocillopora$Site,levels(Pocillopora$Site)[c(4,3,6,1,2,5)])

ggplot(Pocillopora,aes(x=ED50,y=MMM,color=Site)) +
  theme_classic() +
  stat_smooth(method = "lm",formula = y ~ x, se = TRUE, level = 0.95, na.rm = TRUE, colour="grey40")+
  geom_point(size = 5, alpha=0.5) +
  theme(legend.position = 'bottom')+
  ggtitle("P. verrucosa") + xlab("ED50s") + ylab("Site MMM")+
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "grey20"),
        axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="grey20", size=10, face="bold"),
        legend.text = element_text(colour="grey20", size=10, face="plain")) + scale_shape_manual(values=c(16, 16)) +
        scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkgreen","darkorchid4"))

#END