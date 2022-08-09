# project to assess if soundscapes track biodiversity on oyster reefs

# Script to start examining broadband data

# load packages 
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("readxl")
lp("lubridate")
lp("seewave")
lp("patchwork")
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
         sec=second(dt),
         basin="Calcasieu",
         site="control",
         tray=3)

cs_5678_bbsum<-cs_5678_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(cc<-ggplot(data=cs_5678_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize calcasieu site 17.3
cs_5674_bb2<-as.data.frame(cs_5674_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Calcasieu",
         site="17",
         tray=3)

cs_5674_bbsum<-cs_5674_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(c17.3<-ggplot(data=cs_5674_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize calcasieu site 21.3
cs_5680_bb2<-as.data.frame(cs_5680_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Calcasieu",
         site="21",
         tray=3)

cs_5680_bbsum<-cs_5680_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(c21.3<-ggplot(data=cs_5680_bbsum)+
    # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
    # geom_line(aes(x=dt,y=spl))
    geom_tile(aes(x=d,y=hr,fill=rmsspl))+
    scale_fill_viridis_c(option="B",end=.8))


# Calcasieu
cc/c17.3/c21.3+plot_layout(guides="collect")

# visualize lumcon control
l_5674_bb2<-as.data.frame(l_5674_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         site="control",
         tray=4)

l_5674_bbsum<-l_5674_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(lc<-ggplot(data=l_5674_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize lumcon OH4 
l_5678_bb2<-as.data.frame(l_5678_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         site="OH4",
         tray=4)

l_5678_bbsum<-l_5678_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(loh4<-ggplot(data=l_5678_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize lumcon OH2 
l_5680_bb2<-as.data.frame(l_5680_bb[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         site="OH2",
         tray=4)

l_5680_bbsum<-l_5680_bb2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(loh2<-ggplot(data=l_5680_bbsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# all lumcon sites
lc/loh4/loh2
