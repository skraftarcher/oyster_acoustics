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

#sites2<-data.frame(SiteID=c("Haley1","Haley2","Finella5"),
#                   Lat=c(29.25516849194847,29.254818131760082,29.256152633729453),
#                   Long=c(-90.6645846434736, -90.66337258056063, -90.67258161521897 ))

sites<-rbind(sites,sites2)

ggplot()+
  geom_sf(data=la_close)+
  geom_point(data=sites,aes(x=Long,y=Lat,color=SiteID),size=3)+
  theme_bw()

# try with google base map

register_google(key="AIzaSyB5-KC0pGAjEbFIShGzCwqL244OkqtXuBk")

la.gg<-get_map(location=c(lon=-90.66753,lat=29.255),source="google",maptype="satellite",crop=FALSE,zoom=16)


ggmap(la.gg)+
  geom_point(data=sites[-6,],aes(x=Long,y=Lat),size=4,color="white",alpha=.5)+
  geom_point(data=sites[-6,],aes(x=Long,y=Lat),size=3,color="black")+
  geom_point(data=sites[6,],aes(x=Long,y=Lat),size=5,color="red")+
  theme_bw()+
  ylab("Latitude")+
  xlab("Longitude")
  
ggsave("04_figures/site_map_REU.jpg")

sitemap<-ggmap(la.gg)+
  geom_point(data=sites[-6,], aes(x= Long, y= Lat),color="white",size=2)+
  geom_text(aes(label=SiteID,x= Long, y= Lat),nudge_x=.0015, 
            nudge_y=c(0,0,-.0001,.0001,0),data=sites[-6,],color="white",size=4)+
  theme(axis.title = element_blank())

ggsave("04_figures/map_field_work.jpg")

ov<-read_rds("03_wdata/oyster_biomass.rds")%>%
  mutate(Site=paste0("OA",Site),
         m.bio=m.bio/(0.25*0.25),
         sd.bio=sd.bio/(0.25*0.25))%>%
  filter(critterID=="oyster")%>%
  bind_rows(data.frame(Site="OA2",citterID="oyster",m.bio=0,sd.bio=0))
ylabel<-expression(paste("Oyster biomass (mg m"^"-2",")"))
(ovplot<-ggplot(ov)+
  geom_bar(aes(x=Site,y=m.bio),stat="identity")+
  geom_errorbar(aes(x=Site,ymin=m.bio-(sd.bio/sqrt(3)),ymax=m.bio+(sd.bio/sqrt(3))))+
  ylab(ylabel)+
  theme_bw()+
  theme(panel.grid = element_blank()))

library(patchwork)

sitemap / ovplot

ggsave("04_figures/oysterabundancebysite.jpg")
