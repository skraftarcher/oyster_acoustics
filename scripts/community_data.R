## project to assess if soundscapes track biodiversity on oyster reefs

#source to organize community data and calculate diversity indices

source("scripts/install_packages_function.R")
source("scripts/download_community-EX.R")

lp("tidyverse")
lp("vegan")

# organize data

ls22b<-lumcoms22%>%
  filter(Method=="Tray")%>%
  filter(Site %in% c("4","2","M"))%>%
  mutate(dwbio=as.numeric(DryWeight.g)-WeighBoat.g,
         dwbio=ifelse(dwbio<0,0.01,dwbio))%>%
  group_by(Project,Site,TaxaID)%>%
  summarize(dwbio=sum(dwbio,na.rm =TRUE))%>%
  pivot_wider(names_from = TaxaID,values_from = dwbio,values_fill = 0)


ls22env<-ls22b[,1:2]  

ls22com<-ls22b[,-1:-2]

# calculate species richness
ls22env$sprich<-specnumber(ls22com)

# calculate diversity
ls22env$spdiv<-diversity(ls22com)


# double check things
lumcoms22[lumcoms22$Site=="2"& lumcoms22$BagID==7,]

# pull out snapping shrimp and toadfish abundances

ls22sound<-lumcoms22%>%
  filter(Method=="Tray")%>%
  filter(TaxaID %in% c("shmp-2","fsh-1"))%>%
  filter(Site %in% c("4","2","M"))%>%
  mutate(dwbio=as.numeric(DryWeight.g)-WeighBoat.g,
         dwbio=ifelse(dwbio<0,0.01,dwbio),
         TaxaID=case_when(
           TaxaID=="shmp-2"~"SnappingShrimp",
           TaxaID=="fsh-1"~"OysToadfish"))%>%
  group_by(Project,Site,TaxaID)%>%
  summarize(abund=sum(Abundance),
            dwbio=sum(dwbio))%>%
  pivot_wider(names_from = TaxaID,values_from=c(abund,dwbio),values_fill = 0)

# join the abundance of sound producing taxa back on to summary data for lumcon
ls22env<-left_join(ls22env,ls22sound)%>%
  mutate(abund_OysToadfish=ifelse(is.na(abund_OysToadfish),0,abund_OysToadfish),
         dwbio_OysToadfish=ifelse(is.na(dwbio_OysToadfish),0,dwbio_OysToadfish))

