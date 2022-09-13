## project to assess if soundscapes track biodiversity on oyster reefs

#biodiversuty and spl relationship

source("scripts/install_packages_function.R")

lp("tidyverse")

#bring in data
spl<-read_rds("wdata/site_summarized_spl.rds")
biod<-read.csv("wdata/summarized_biodiversity.csv")
aind<-read.csv("wdata/summarized_sitelevel_acousticindices.csv")

#join spl and biod
biospl<-left_join(biod, spl)%>%
  left_join(aind)%>%
  mutate(stype=case_when(
    Site%in% c("C","M")~"Control",
    !Site%in% c("C","M")~ "Reef"))
theme_set(theme_bw()+theme(panel.grid = element_blank()))

spllab<-expression( )
#plots
(splp<-ggplot(data=biospl)+
  geom_point(aes(y=spdiv,x=bbrmsspl, color=basin, shape=stype), size=5)+
  geom_smooth(aes(y=spdiv,x=bbrmsspl),method="lm")+
  ylab("Species Diversity")+
  xlab("Broadband Sound Pressure level (dB re 1 μPa)")+
  scale_color_viridis_d(end=.8,begin=.2,name="Control")+
  scale_shape_manual(name="Site type",values=c(17,19))#+
  # geom_point(aes(y=spdiv,x=lowrmsspl, color=basin, shape=stype), size=5)+
  # geom_smooth(aes(y=spdiv,x=lowrmsspl),method="lm")
  #geom_point(aes(y=spdiv,x=bbsdspl, color=basin, shape=stype), size=5)
  #geom_point(aes(y=spdiv,x=bbminspl, color=basin, shape=season), size=5)
  #geom_point(aes(y=spdiv,x=bbmaxspl, color=basin, shape=stype), size=5)
)

(splrp<-ggplot(data=biospl)+
  #geom_point(aes(x=sprich,y=lowrmsspl,color=basin,shape=season),size=5)+
  #geom_smooth(aes(x=sprich,y=lowrmsspl),method="lm")
   geom_point(aes(y=sprich,x=bbrmsspl,color=basin,shape=stype),size=5)+
   geom_smooth(aes(y=sprich,x=bbrmsspl),method="lm")+
    ylab("Species Richness")+
    xlab("Broadband Sound Pressure level (dB re 1 μPa)")+
    scale_color_viridis_d(end=.8,begin=.2,name="Control")+
    scale_shape_manual(name="Site type",values=c(17,19)))
  # geom_point(aes(y=sprich,x=bbmedspl,color=basin,shape=season),size=5)+
  # geom_smooth(aes(y=sprich,x=bbmedspl),method="lm")
  # geom_point(aes(x=sprich,y=lowmedspl,color=basin,shape=season),size=5)+
  # geom_smooth(aes(x=sprich,y=lowmedspl),method="lm")

ggplot(data=biospl)+
  # geom_point(aes(x=abund,y=lowrmsspl,color=basin,shape=season),size=5)+
  # geom_smooth(aes(x=abund,y=lowrmsspl),method="lm")
   # geom_point(aes(x=abund,y=bbrmsspl,color=basin,shape=season),size=5)+
   # geom_smooth(aes(x=abund,y=bbrmsspl),method="lm")
  # geom_point(aes(x=abund,y=bbmedspl,color=basin,shape=season),size=5)+
  # geom_smooth(aes(x=abund,y=bbmedspl),method="lm")
geom_point(aes(x=abund,y=lowmedspl,color=basin,shape=season),size=5)+
geom_smooth(aes(x=abund,y=lowmedspl),method="lm")


#Acoustic indices plots
ggplot(data=biospl)+
  #geom_point(aes(x=sprich,y=maci,color=basin,shape=stype),size=5)
  # geom_point(aes(x=sprich,y=mndsi,color=basin,shape=stype),size=5)
  #geom_point(aes(x=sprich,y=mbiophony,color=basin,shape=stype),size=5)
  #geom_point(aes(x=sprich,y=manthrophony,color=basin,shape=stype),size=5)
  #geom_point(aes(x=sprich,y=mH,color=basin,shape=stype),size=5)
  #geom_point(aes(x=spdiv,y=maci,color=basin,shape=stype),size=5)
  geom_point(aes(x=spdiv,y=mndsi,color=basin,shape=stype),size=5)
  #geom_point(aes(x=spdiv,y=mbiophony,color=basin,shape=stype),size=5)
  #geom_point(aes(x=spdiv,y=manthrophony,color=basin,shape=stype),size=5)
  #geom_point(aes(x=spdiv,y=mH,color=basin,shape=stype),size=5)
