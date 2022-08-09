## project to assess if soundscapes track biodiversity on oyster reefs

#biodiversuty and spl relationship

source("scripts/install_packages_function.R")

lp("tidyverse")

#bring in data
spl<-read_rds("wdata/site_summarized_spl.rds")
biod<-read.csv("wdata/summarized_biodiversity.csv")

#join spl and biod
biospl<-left_join(biod, spl)%>%
  mutate(stype=case_when(
    Site%in% c("C","M")~"Control",
    !Site%in% c("C","M")~ "Reef"))

#plots
ggplot(data=biospl)+
  #geom_point(aes(x=spdiv,y=bbrmsspl, color=basin, shape=stype), size=5)
  #geom_point(aes(x=spdiv,y=lowrmsspl, color=basin, shape=stype), size=5)
  #geom_point(aes(x=spdiv,y=bbsdspl, color=basin, shape=stype), size=5)
  #geom_point(aes(x=spdiv,y=bbminspl, color=basin, shape=stype), size=5)
  geom_point(aes(x=spdiv,y=bbmaxspl, color=basin, shape=stype), size=5)
