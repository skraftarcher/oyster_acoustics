# organize acoustic indices data

source("scripts/install_packages_function.R")

lp("tidyverse")

# bring in data
lsum5674<-read.csv("wdata/lum5674_indices.csv")%>%
  mutate(basin="Terrebonne",Site = "M",season="summer")
lsum5678<-read.csv("wdata/lum5678_indices.csv")%>%
  mutate(basin="Terrebonne",Site = "4",season="summer")
# lsum5680<-read.csv("wdata/lum5680_indices.csv")%>%
#   mutate(basin="Terrebonne",Site = "2",season="summer") didn't finish running yet
csum5678<-read.csv("wdata/calc5678_indices.csv")%>%
  mutate(basin="Calcasieu",Site = "C",season="summer")
csum5674<-read.csv("wdata/calc5674_indices.csv")%>%
  mutate(basin="Calcasieu",Site = "17",season="summer")
csum5680<-read.csv("wdata/calc5680_indices.csv")%>%
  mutate(basin="Calcasieu",Site = "21",season="summer")

# join and organize data
ai.sum<-bind_rows(lsum5674,
                  lsum5678,
                  #lsum5680,
                  csum5674,
                  csum5678,
                  csum5680)%>%
  group_by(basin,season,Site)%>%
  summarize(maci=mean(aci),
            sdaci=sd(aci),
            mndsi=mean(ndsi),
            sdndsi=sd(ndsi),
            mbiophony=mean(biophony),
            sdbiophony=sd(biophony),
            manthrophony=mean(anthrophony),
            sdanthrophony=sd(anthrophony),
            mH=mean(H),
            sdH=sd(H))


write.csv(ai.sum,"wdata/summarized_sitelevel_acousticindices.csv",row.names=FALSE)
