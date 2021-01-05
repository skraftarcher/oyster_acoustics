# This script pulls data from google drive and makes figures comparing sites on oyster volume, size dist., and biomass

#Stephanie K. Archer 12/14/2020


if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

#assign the google sheet link to url
url<-"https://docs.google.com/spreadsheets/d/1GVtzCYM-0VjyJemBoDoKsW80YuFDXiWLo_izcpEaYko/edit?usp=sharing"

# now pull the sheet
volume<-read.csv(text=gsheet2text(url,format='csv'),stringsAsFactors = FALSE)  

size.url<-"https://docs.google.com/spreadsheets/d/1GVtzCYM-0VjyJemBoDoKsW80YuFDXiWLo_izcpEaYko/edit#gid=1464195423"
osize<-read.csv(text=gsheet2text(size.url,format='csv'),stringsAsFactors = FALSE) 

bio.url<-"https://docs.google.com/spreadsheets/d/1GVtzCYM-0VjyJemBoDoKsW80YuFDXiWLo_izcpEaYko/edit#gid=595301829"

biomass<-read.csv(text=gsheet2text(bio.url,format='csv'),stringsAsFactors = FALSE)

# organize and plot volume data
vol.sum<-volume%>%
  select(Site,Quadrat,total.oyster.volume,total.shell.volume,total.mussel.volume)%>%
  pivot_longer(total.oyster.volume:total.mussel.volume,names_to="Type",values_to="volume")%>%
  mutate(Type=case_when(
    Type=="total.oyster.volume"~"oyster",
    Type=="total.mussel.volume"~"mussel",
    Type=="total.shell.volume"~"shell"))%>%
  group_by(Site,Quadrat,Type)%>%
  summarize(volume=sum(volume))%>%
  group_by(Site,Type)%>%
  summarize(m.vol=mean(volume),sd.vol=sd(volume)/sqrt(n()))


ggplot(data=vol.sum)+
  geom_point(aes(x=Site,y=m.vol,color=Type),size=4,position=position_dodge(0.5))+
  geom_errorbar(aes(x=Site,ymin=m.vol-sd.vol,ymax=m.vol+sd.vol,color=Type),position=position_dodge(0.5))



# biomass data

bio.sum<-biomass%>%
  group_by(Site,Quadrat,critterID)%>%
  summarize(biomass=sum(dry.final))%>%
  group_by(Site,critterID)%>%
  summarize(m.bio=mean(biomass),sd.bio=sd(biomass)/sqrt(n()))


ggplot(data=bio.sum)+
  geom_point(aes(x=Site,y=m.bio,color=critterID),size=4,position=position_dodge(0.5))+
  geom_errorbar(aes(x=Site,ymin=m.bio-sd.bio,ymax=m.bio+sd.bio,color=critterID),position=position_dodge(0.5))
