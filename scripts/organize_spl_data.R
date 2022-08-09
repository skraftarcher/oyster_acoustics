# project to assess if soundscapes track biodiversity on oyster reefs

# Script to start examining link between spl and biodiversity

#bring in spl data
source("scripts/visualize_spl.R")

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
                                        tz="America/Chicago")))

# organize site-level dataset
bbspl.site<-bbspl%>%
  group_by(basin,site,tray)%>%
  summarize(splvar=var(spl),
            sdspl=sd(spl),
            rmsspl=rms(spl),
            medspl=median(spl),
            maxspl=max(spl),
            minspl=min(spl))

# real quick visualization of control sites vs reef sites

ggplot(data=bbspl.site,aes(x=site,color=basin))+
  #geom_errorbar(aes(ymin=rmsspl-sdspl,ymax=rmsspl+sdspl),width=.1)+
  #geom_point(aes(y=rmsspl),size=5)
  #geom_point(aes(y=sdspl),size=5)
  #geom_point(aes(y=medspl),size=5)
  geom_point(aes(y=maxspl),size=5,shape=1)+
  geom_point(aes(y=minspl),size=5,shape=16)
