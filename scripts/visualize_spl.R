# project to assess if soundscapes track biodiversity on oyster reefs

# Script to start examining data

# load packages 
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("readxl")
lp("lubridate")
lp("seewave")
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# bring in the data
cs_5674_bb<-read_rds("odata/calc_sum_22_5674_broadband_spl.rds")
cs_5678_bb<-read_rds("odata/calc_sum_22_5678_broadband_spl.rds")
cs_5680_bb<-read_rds("odata/calc_sum_22_5680_broadband_spl.rds")
l_5674_bb<-read_rds("odata/lum_sum_22_5674_broadband_spl.rds")
l_5678_bb<-read_rds("odata/lum_sum_22_5678_broadband_spl.rds")
l_5680_bb<-read_rds("odata/lum_sum_22_5680_broadband_spl.rds")


# visualize calcasieu site control (aka mud)
cs_5678_bb2<-as.data.frame(cs_5678_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt))

cs_5678_bbsum<-cs_5678_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


ggplot(data=cs_5678_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=sdspl))+
  scale_fill_viridis_c(option="B",end=.8)



# visualize lumcon OH4 
l_5678_bb2<-as.data.frame(l_5678_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt))

l_5678_bbsum<-l_5678_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


ggplot(data=l_5678_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=sdspl))+
  scale_fill_viridis_c(option="B",end=.8)

# visualize calcasieu site 17.3
cs_5674_bb2<-as.data.frame(cs_5674_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt))

cs_5674_bbsum<-cs_5674_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


ggplot(data=cs_5674_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=sdspl))+
  scale_fill_viridis_c(option="B",end=.8)
