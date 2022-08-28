## project to assess if soundscapes track biodiversity on oyster reefs

#source to organize community data and calculate diversity indices

source("scripts/install_packages_function.R")
source("scripts/download_community-EX.R")

lp("tidyverse")
lp("vegan")
lp("readxl")

# organize data
# there are some organisms that were not sampled in Calcasieu so when comparing
# across basins those should be removed.
# these are amphipods, isopods, and polychaetes

trm<-unique(c(lumcomspr22$Taxa.ID[grep(lumcomspr22$Taxa.ID,pattern="poly*")],
       lumcoms22$TaxaID[grep(lumcoms22$TaxaID,pattern="poly*")],
       lumcomspr22$Taxa.ID[grep(lumcomspr22$Taxa.ID,pattern="iso*")],
       lumcoms22$TaxaID[grep(lumcoms22$TaxaID,pattern="iso*")],
       lumcomspr22$Taxa.ID[grep(lumcomspr22$Taxa.ID,pattern="amp*")],
       lumcoms22$TaxaID[grep(lumcoms22$TaxaID,pattern="amp*")]))

#spring 2022
lspr22b<-lumcomspr22%>%
  filter(!Taxa.ID %in% trm)%>%
  filter(Site %in% c(1,2,4,5))%>%
  mutate(dwbio=as.numeric(Dry.Weight..g.)-Weigh.Boat..g.,
         dwbio=ifelse(dwbio<0,0.01,dwbio))%>%
  group_by(Project,Site,Taxa.ID)%>%
  summarize(dwbio=sum(dwbio,na.rm =TRUE))%>%
  pivot_wider(names_from = Taxa.ID,values_from = dwbio,values_fill = 0)


lspr22env<-lspr22b[,1:2]  

lspr22com<-lspr22b[,-1:-2]

# add season
lspr22env$season<-"spring"
# calculate species richness
lspr22env$sprich<-specnumber(lspr22com)

# calculate diversity
lspr22env$spdiv<-diversity(lspr22com)

# animal abundance
lspr22env$abund<-rowSums(lspr22com)


# pull out snapping shrimp and toadfish abundances

lspr22sound<-lumcomspr22%>%
  filter(Taxa.ID %in% c("shmp-2","fsh-1"))%>%
  filter(Site %in% c(1,2,4,5))
# there are no snapping shrimp or oyster toad fish

# join the abundance of sound producing taxa back on to summary data for lumcon
lspr22env<-lspr22env%>%
  mutate(abund_OysToadfish=0,
         dwbio_OysToadfish=0)


#summer 2022
ls22b<-lumcoms22%>%
  filter(!TaxaID %in% trm)%>%
  filter(Method=="Tray")%>%
  filter(Site %in% c("4","2","M"))%>%
  mutate(dwbio=as.numeric(Dryw)-WeighBoat,
         dwbio=ifelse(dwbio<0,0.01,dwbio))%>%
  group_by(Project,Site,TaxaID)%>%
  summarize(dwbio=sum(dwbio,na.rm =TRUE))%>%
  pivot_wider(names_from = TaxaID,values_from = dwbio,values_fill = 0)


ls22env<-ls22b[,1:2]  

ls22com<-ls22b[,-1:-2]

# add season
ls22env$season<-"summer"
# calculate species richness
ls22env$sprich<-specnumber(ls22com)

# calculate diversity
ls22env$spdiv<-diversity(ls22com)

# animal abundance
ls22env$abund<-rowSums(ls22com)


# double check things
lumcoms22[lumcoms22$Site=="2"& lumcoms22$BagID==7,]

# pull out snapping shrimp and toadfish abundances

ls22sound<-lumcoms22%>%
  filter(Method=="Tray")%>%
  filter(TaxaID %in% c("shmp-2","fsh-1"))%>%
  filter(Site %in% c("4","2","M"))%>%
  mutate(dwbio=as.numeric(Dryw)-WeighBoat,
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

#bring in calc data

cls22<-read_xlsx("odata/TNC_CL_tray_databse.xlsx",sheet="raw_data")%>%
  filter(collection.date>"2022-06-01")%>%
  filter(!is.na(combined.weight.g))%>%
  filter(species.code!="NA")%>%
  select(Site=site, plot, species.code, combined.weight.g)%>%
  distinct()%>%
  mutate(combined.weight.g=as.numeric(combined.weight.g), 
         Project="TNC")%>%
  group_by(Project, Site, species.code)%>%
  summarize(totwt=sum(combined.weight.g))%>%
  pivot_wider(names_from=species.code, values_from=totwt, values_fill=0)


cls22env<-cls22[,1:2]  

cls22com<-cls22[,-1:-2]

# add season
cls22env$season<-"summer"
# calculate species richness
cls22env$sprich<-specnumber(cls22com)

# calculate diversity
cls22env$spdiv<-diversity(cls22com)

# animal abundance
cls22env$abund<-rowSums(cls22com)

cls22env$abund_OysToadfish<-0
cls22env$dwbio_OysToadfish<-0

#create biodiversity dataset
lspr22env$Site<-as.character(lspr22env$Site)
bio22<-bind_rows(lspr22env,ls22env, cls22env)

#saving file
write.csv(bio22, "wdata/summarized_biodiversity.csv", row.names = FALSE)
