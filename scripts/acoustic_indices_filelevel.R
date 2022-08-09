# script to calculate wav level indices

# load packages
source("scripts/install_packages_function.R")
lp("soundecology")
lp("tidyverse")
lp("lubridate")
lp("tuneR")
lp("seewave")

# now write a function to have it go through a folder and calcualte 
# multiple indices for each wave function

get.indices<-function(folder){
  t1<-data.frame(sname=folder,files=list.files(folder,pattern="*.wav"),
                 aci=NA,ndsi=NA,biophony=NA,anthrophony=NA,
                 H=NA)%>%
    separate(sname,into=c("d","o","sname"),sep="/")%>%
    select(-d,-o)
  for(i in 1:nrow(t1)){
    .d<-readWave(paste0(folder,"/",t1$files[i]))
    t1$aci[i]<-sum(acoustic_complexity(.d,min_freq = 20)$aci_fl_left_vals,na.rm = TRUE)
    t2<-ndsi(.d)
    t1$ndsi[i]<-t2$ndsi_left
    t1$biophony[i]<-t2$biophony_left
    t1$anthrophony[i]<-t2$anthrophony_left
    t1$H[i]<-H(.d)
  }
return(t1)
  }

# set folders to run function on
# change this for each computer as appropriate
f1<-"D:/Calc_Sum_2022/Calc_Sum_22_5674"
f2<-"D:/Calc_Sum_2022/Calc_Sum_22_5678"
f3<-"D:/Calc_Sum_2022/Calc_Sum_22_5680"
f4<-"D:/LUM_Sum_2022/LUM_Sum_22_5674"
f5<-"D:/LUM_Sum_2022/LUM_Sum_22_5678"
f6<-"D:/LUM_Sum_2022/LUM_Sum_22_5680"

calc5674<-get.indices(folder=f1)
calc5678<-get.indices(folder=f2)
calc5680<-get.indices(folder=f3)
lum5674<-get.indices(folder=f4)
lum5678<-get.indices(folder=f5)
lum5680<-get.indices(folder=f6)

