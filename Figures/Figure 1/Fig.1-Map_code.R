#Code for Figure 1 of "Empirically derived thermal thresholds of four coral species along the Red Sea"

#IMPORTANT#
#Before starting - read the info.txt file in this folder. 
#Users must download: #the 'gshhg-bin-2.3.7' folder and 'ct5km_climatology_v3.1.nc' file from NOAA


#setwd to source file location

library(vegan)
library(maptools)
library(dplyr)
library(ggplot2)
library(raster)
library(xts)
library(ncdf4)
library(lattice)
library(dplyr)
library(tidyr)

### Extract NOAA MMM by lat/lon to plot
install.packages('gpclib', type='source')

if (!rgeosStatus()) gpclibPermit()
gshhs.f.b <- './gshhg-bin-2.3.7/gshhs_f.b'
sf1 <- getRgshhsMap(gshhs.f.b, xlim = c(30, 46), ylim = c(8, 31)) %>%
  fortify()

sites <- read.csv('GPSCoordinates.csv')

####RedSeaSubsetting####
nc <- nc_open("ct5km_climatology_v3.1.nc") # Open NOAA netcdf file

sst_NOAA_clim_full <- ncvar_get( nc, "sst_clim_mmm") # Extract whole globe, can subset by lat/lon later
lon <- as.vector(ncvar_get(nc,"lon")) # get longitude for reference
lat <- as.vector(ncvar_get(nc,"lat")) # get latitude for reference

RedSeaMMM<-data.frame(sst_NOAA_clim_full[4201:4561,1180:1701])
row.names(RedSeaMMM)<-4201:4561
names(RedSeaMMM)<-lat[1180:1701]

####Pivot_longer way####
MMMToPlot<-cbind("Lon_ref"=rep(4201:4561, each=522),pivot_longer(RedSeaMMM, col=1:ncol(RedSeaMMM), names_to="Lat_ref", values_to="MMM"))
MMMToPlot$Lon <- rep(lon[4201:4561],each = 522)
MMMToPlot$Lat <- lat[1180:1701]

####Index lookup way####
MMMsToPlot<-data.frame(cbind("LonIdx"=rep(4201:4561, each=522),"LatIdx"=1180:1701,"TruLon"=lon[rep(4201:4561, each=522)], "TruLat"=lat[1180:1701]))
MMMsToPlot$LookupMMM<-sst_NOAA_clim_full[cbind(MMMsToPlot$LonIdx,MMMsToPlot$LatIdx)]

#plotting main map
RedSea_MMM_plot<-ggplot() + 
  geom_raster(data = MMMToPlot, aes(x=Lon, y=Lat, fill=MMM)) +
  #geom_tile(data = MMMToPlot, aes(x=Lon, y=Lat, fill=MMM, width=0.3, height=0.3))+ #messes up MMM grid relative to polygons
  geom_polygon(data = sf1, aes(x=long, y = lat, group = group), fill = 'grey50', color='black', lwd = 0.2) +
  geom_point(data=sites, aes(x=site_long, y=site_lat, shape = Site), size=2, pch = c(21,21,21,21,21,21), fill = c(c("royalblue2","darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1"))) +
  geom_text(data = sites, aes(label = Site, x = site_long-0.5, y = site_lat+0.5), show.legend = FALSE, color = 'black') +
  theme_bw() +
  xlab("Longitude (E)") +
  ylab("Latitude (N)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_sf(xlim = c(30, 46), ylim = c(8, 31), expand = FALSE) + #initial sf1 clipping has to match these x-y lims to avoid issues when importing into Illustrator
  #coord_fixed(ratio=1.1, xlim = c(30,48), ylim = c(8,31), expand = 0)+
  scale_fill_gradient2(low = 'dodgerblue', high = 'red2', mid = 'seagreen3', midpoint = 29) 
#midpoint = mean(MMMToPlot$MMM,na.rm=TRUE) doesn't produce appropriate colour scale
RedSea_MMM_plot

ggsave(RedSea_MMM_plot, height =  8, width = 8, filename = "gradient_map.pdf", useDingbats=FALSE)

#plotting Al Wajh/Yanbu close up
Close_up_plot<-ggplot() + 
  geom_raster(data = MMMToPlot, aes(x=Lon, y=Lat, fill=MMM)) +
  #geom_tile(data = MMMToPlot, aes(x=Lon, y=Lat, fill=MMM, width=0.3, height=0.3))+ #messes up MMM grid relative to polygons
  geom_polygon(data = sf1, aes(x=long, y = lat, group = group), fill = 'grey50', color='black', lwd = 0.2) +
  geom_point(data=sites, aes(x=site_long, y=site_lat, shape = Site), size=2, pch = c(21,21,21,21,21,21), fill = c(c("royalblue2","darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1"))) +
  geom_text(data = sites, aes(label = Site, x = site_long, y = site_lat), show.legend = FALSE, color = 'black') +
  theme_bw() +
  xlab("Longitude (E)") +
  ylab("Latitude (N)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_sf(xlim = c(36.4, 38.2), ylim = c(23.5, 26), expand = FALSE) + #initial sf1 clipping has to match these x-y lims to avoid issues when importing into Illustrator
  #coord_fixed(ratio=1.1, xlim = c(30,48), ylim = c(8,31), expand = 0)+
  scale_fill_gradient2(low = 'dodgerblue', high = 'red2', mid = 'seagreen3', midpoint = 29) 
#midpoint = mean(MMMToPlot$MMM,na.rm=TRUE) doesn't produce appropriate colour scale
Close_up_plot

ggsave(Close_up_plot, height =  8, width = 8, filename = "Close_up_map.pdf", useDingbats=FALSE)

