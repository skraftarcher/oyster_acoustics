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

#plots
ggplot(data=biospl)+
  #geom_point(aes(x=spdiv,y=bbrmsspl, color=basin, shape=season), size=5)
  geom_point(aes(x=spdiv,y=lowrmsspl, color=basin, shape=stype), size=5)
  #geom_point(aes(x=spdiv,y=bbsdspl, color=basin, shape=stype), size=5)
  #geom_point(aes(x=spdiv,y=bbminspl, color=basin, shape=season), size=5)
  #geom_point(aes(x=spdiv,y=bbmaxspl, color=basin, shape=stype), size=5)

ggplot(data=biospl)+
  #geom_point(aes(x=sprich,y=lowrmsspl,color=basin,shape=season),size=5)+
  #geom_smooth(aes(x=sprich,y=lowrmsspl),method="lm")
  #  geom_point(aes(x=sprich,y=bbrmsspl,color=basin,shape=season),size=5)+
  #  geom_smooth(aes(x=sprich,y=bbrmsspl),method="lm")
  geom_point(aes(x=sprich,y=bbmedspl,color=basin,shape=season),size=5)+
  geom_smooth(aes(x=sprich,y=bbmedspl),method="lm")
  # geom_point(aes(x=sprich,y=lowmedspl,color=basin,shape=season),size=5)+
  # geom_smooth(aes(x=sprich,y=lowmedspl),method="lm")


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
