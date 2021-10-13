#Code for Figure 4 of "Empirically derived thermal thresholds of four coral species along the Red Sea"

#setwd to source file location

#Heatmap
library(pheatmap)
library(RColorBrewer)

heatmap_data<-read.csv("Coral_heatmap.csv") #Numbers indicate rank of coral species at each site (see Fig. 3b)

data_matrix <- data.matrix(heatmap_data[,2:7])

rownames(data_matrix) = sapply(heatmap_data$Coral.species,function(x) strsplit(as.character(x),split = "\\\\")[[1]][1])

coral_heatmap <- pheatmap(data_matrix,cluster_cols=FALSE,cluster_rows=FALSE,scale="none",
                          fontsize_row=12,fontsize_col=12,angle_col=45,border_color="grey87", 
                          color = brewer.pal(n = 4, name = "Purples"), cellheight = 50)

#END 