# project to assess if soundscapes track biodiversity on oyster reefs

# Script to start examining link between spl and biodiversity

#bring in spl data
source("scripts/visualize_spl.R")
source("scripts/visualize_low_spl.R")

bbspl<-bind_rows(cs_5674_bb2,
                 cs_5678_bb2,
                 cs_5680_bb2,
                 l_5674_bb2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")),
                 l_5678_bb2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")),
                 l_5680_bb2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")))%>%
    rename(bbspl=spl)

# organize Site-level dataset
bbspl.Site<-bbspl%>%
  group_by(basin,Site,tray)%>%
  summarize(bbsplvar=var(bbspl),
            bbsdspl=sd(bbspl),
            bbrmsspl=rms(bbspl),
            bbmedspl=median(bbspl),
            bbmaxspl=max(bbspl),
            bbminspl=min(bbspl))

# real quick visualization of control Sites vs reef Sites

#ggplot(data=bbspl.Site,aes(x=Site,color=basin))+
  #geom_errorbar(aes(ymin=rmsspl-sdspl,ymax=rmsspl+sdspl),width=.1)+
  #geom_point(aes(y=rmsspl),size=5)
  #geom_point(aes(y=sdspl),size=5)
  #geom_point(aes(y=medspl),size=5)
  #geom_point(aes(y=maxspl),size=5,shape=1)+
  #geom_point(aes(y=minspl),size=5,shape=16)

#organizing low frequency spl
lowspl<-bind_rows(cs_5674_low2,
                 cs_5678_low2,
                 cs_5680_low2,
                 l_5674_low2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")),
                 l_5678_low2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")),
                 l_5680_low2%>%
                   filter(dt >= ymd_hms("2022-07-09 08:00:00",
                                        tz="America/Chicago")))%>%
  rename(lowspl=spl)

# organize Site-level dataset
lowspl.Site<-lowspl%>%
  group_by(basin,Site,tray)%>%
  summarize(lowsplvar=var(lowspl),
            lowsdspl=sd(lowspl),
            lowrmsspl=rms(lowspl),
            lowmedspl=median(lowspl),
            lowmaxspl=max(lowspl),
            lowminspl=min(lowspl))

# real quick visualization of control Sites vs reef Sites

#ggplot(data=lowspl.Site,aes(x=Site,color=basin))+
  #geom_errorbar(aes(ymin=rmsspl-sdspl,ymax=rmsspl+sdspl),width=.1)+
  #geom_point(aes(y=rmsspl),size=5)
  #geom_point(aes(y=sdspl),size=5)
  #geom_point(aes(y=medspl),size=5)
  #geom_point(aes(y=maxspl),size=5,shape=1)+
  #geom_point(aes(y=minspl),size=5,shape=16)

#join data
spl<-left_join(bbspl, lowspl)
Site<-left_join(bbspl.Site, lowspl.Site)

#saving data
write_rds(spl, "wdata/by_minute_spl.rds")
write_rds(Site, "wdata/Site_summarized_spl.rds")
