# project to assess if soundscapes track biodiversity on oyster reefs

# Script to start examining low data

# load packages 
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("readxl")
lp("lubridate")
lp("seewave")
lp("patchwork")
theme_set(theme_bw()+theme(panel.grid = element_blank()))

# bring in the data
cs_5674_low<-read_rds("odata/calc_sum_22_5674_low_spl.rds")
cs_5678_low<-read_rds("odata/calc_sum_22_5678_low_spl.rds")
cs_5680_low<-read_rds("odata/calc_sum_22_5680_low_spl.rds")
l_5674_low<-read_rds("odata/lum_sum_22_5674_low_spl.rds")
l_5678_low<-read_rds("odata/lum_sum_22_5678_low_spl.rds")
l_5680_low<-read_rds("odata/lum_sum_22_5680_low_spl.rds")
l_5674_lowa<-read_rds("odata/lum_spr_22_5674_low_spl.rds")
l_5678_lowa<-read_rds("odata/lum_spr_22_5678_low_spl.rds")
l_5680_lowa<-read_rds("odata/lum_spr_22_5680_low_spl.rds")
l_5679_lowa<-read_rds("odata/lum_spr_22_5679_low_spl.rds")

# visualize calcasieu site control (aka mud)
cs_5678_low2<-as.data.frame(cs_5678_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Calcasieu",
         Project="TNC",
         Site="C",
         season="summer", tray=3)

cs_5678_lowsum<-cs_5678_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(cc<-ggplot(data=cs_5678_lowsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize calcasieu site 17.3
cs_5674_low2<-as.data.frame(cs_5674_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Calcasieu",
         Project="TNC",
         Site="17",
         season="summer", tray=3)

cs_5674_lowsum<-cs_5674_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(c17.3<-ggplot(data=cs_5674_lowsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize calcasieu site 21.3
cs_5680_low2<-as.data.frame(cs_5680_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Calcasieu",
         Project="TNC",
         Site="21",
         season="summer", tray=3)

cs_5680_lowsum<-cs_5680_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(c21.3<-ggplot(data=cs_5680_lowsum)+
    # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
    # geom_line(aes(x=dt,y=spl))
    geom_tile(aes(x=d,y=hr,fill=rmsspl))+
    scale_fill_viridis_c(option="B",end=.8))


# Calcasieu
cc/c17.3/c21.3+plot_layout(guides="collect")

# visualize lumcon control
l_5674_low2<-as.data.frame(l_5674_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="M",
         season="summer", tray=4)

l_5674_lowsum<-l_5674_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(lc<-ggplot(data=l_5674_lowsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize lumcon OH4 
l_5678_low2<-as.data.frame(l_5678_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="4",
         season="summer", tray=4)

l_5678_lowsum<-l_5678_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(loh4<-ggplot(data=l_5678_lowsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# visualize lumcon OH2 
l_5680_low2<-as.data.frame(l_5680_low[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="2",
         season="summer", tray=4)

l_5680_lowsum<-l_5680_low2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))


(loh2<-ggplot(data=l_5680_lowsum)+
  # geom_point(aes(x=dt,y=spl,size=spl),color="purple",alpha=.1)+
  # geom_line(aes(x=dt,y=spl))
  geom_tile(aes(x=d,y=hr,fill=rmsspl))+
  scale_fill_viridis_c(option="B",end=.8))

# all lumcon sites
lc/loh4/loh2


# lumcon spring data
# visualize lumcon OH1 spring 
l_5678_lowa2<-as.data.frame(l_5678_lowa[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="1",
         season = "spring",
         tray=4)

l_5678_lowasum<-l_5678_lowa2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))

# visualize lumcon OH2 spring 
l_5680_lowa2<-as.data.frame(l_5680_lowa[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="2",
         season = "spring",
         tray=4)

l_5680_lowasum<-l_5680_lowa2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))

# visualize lumlowcon OH4 spring 
l_5674_lowa2<-as.data.frame(l_5674_lowa[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="4",
         season = "spring",
         tray=4)

l_5674_lowasum<-l_5674_lowa2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))

# visualize lumcon OH1 spring 
l_5679_lowa2<-as.data.frame(l_5679_lowa[-1,])%>%
  rename(dt=V1,spl=V2)%>%
  mutate(dt=as.POSIXct(dt,origin = "1970-01-01",tz="America/Chicago"),
         d=day(dt),
         yr=year(dt),
         m=month(dt),
         hr=hour(dt),
         mins=minute(dt),
         sec=second(dt),
         basin="Terrebonne",
         Project="OH",
         Site="5",
         season = "spring",
         tray=4)

l_5679_lowasum<-l_5679_lowa2%>%
  group_by(d,hr)%>%
  summarize(mspl=mean(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl))
