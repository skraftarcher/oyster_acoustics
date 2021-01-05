# this script will make a site map for the LUMCON-based sampling

# written by Stephanie K. Archer, 12/12/2020

# load packages and bring in data
if(!require(sf))install.packages("sf");library(sf)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(ggspatial)) install.packages('ggspatial');library(ggspatial)
if(!require(ggmap)) install.packages('ggmap');library(ggmap)
# base map
la<-read_sf("C:/Users/sarcher/Documents/shapefiles/Louisiana.shp")
la_close<-st_crop(la,xmin=-90.68,xmax=-90.65,ymin=29.24,ymax=29.27)
# site coordinates
(sites <- read_xlsx("02_odata/Site Coordinates.xlsx"))



ggplot()+
  geom_sf(data=la_close)+
  geom_point(data=sites,aes(x=Long,y=Lat,color=SiteID),size=3)+
  theme_bw()

# try with google base map

register_google(key="AIzaSyA3ghxCBCFWYKdXxRHwp81T4PjtRPmaNI0")

la.gg<-get_map(location=c(-90.66753,29.255),source="google",maptype="satellite",crop=FALSE,zoom=16)


ggmap(la.gg)+
  geom_point(data=sites[-6,],aes(x=Long,y=Lat),size=4,color="white",alpha=.5)+
  geom_point(data=sites[-6,],aes(x=Long,y=Lat),size=3,color="black")+
  geom_point(data=sites[6,],aes(x=Long,y=Lat),size=5,color="red")+
  theme_bw()+
  ylab("Latitude")+
  xlab("Longitude")
  
ggsave("04_figures/site_map_urop.jpg")

ggmap(la.gg)+
  geom_point(data=sites[-6,], aes(x= Long, y= Lat),color="white",size=2)+
  geom_text(aes(label=SiteID,x= Long, y= Lat),hjust=0, vjust=0,data=sites[-6,],color="white",size=6)

ggsave("04_figures/map_fiel_work.jpg")
