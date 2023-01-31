library(tidyverse)
mean_s_dat<-read.csv("~/SmokeProject/Data/final_means2.csv")
Visdat<-read.csv("~/SmokeProject/Data/VisitationData.csv")
mean_s_dat<-tidyr::gather(mean_s_dat,key="Date",value="Smoke",X19800131:X20191231)

mean_s_dat$Year<-as.integer(substr(mean_s_dat$Date,2,5))
mean_s_dat$Month<-as.integer(substr(mean_s_dat$Date,6,7))
mean_s_dat$UnitCode<-mean_s_dat$park
mean_s_dat<-mean_s_dat[,3:6]
dat<-merge(Visdat,mean_s_dat,by=c("Year","Month","UnitCode"))
dat$RecreationVisits<-gsub(",","",dat$RecreationVisits)
dat$RecreationVisits<-as.numeric(dat$RecreationVisits)
dat$stdsmoke<-as.vector(scale(dat$Smoke))

#add max smoke
max_s_dat<-read.csv("~/SmokeProject/Data/final_max.csv")
max_s_dat<-tidyr::gather(max_s_dat,key="Date",value="Smoke",X19800131:X20191231)
max_s_dat$Year<-as.integer(substr(max_s_dat$Date,2,5))
max_s_dat$Month<-as.integer(substr(max_s_dat$Date,6,7))
max_s_dat$UnitCode<-max_s_dat$park
max_s_dat$MAXSmoke<-max_s_dat$Smoke
max_s_dat<-max_s_dat[,4:7]
dat<-merge(dat,max_s_dat,by=c("Year","Month","UnitCode"))
dat$stdsmokeMAX<-as.vector(scale(dat$MAXSmoke))

#add median smoke
med_s_dat<-read.csv("~/SmokeProject/Data/final_medians.csv")
med_s_dat<-tidyr::gather(med_s_dat,key="Date",value="Smoke",X19800131:X20191231)
med_s_dat$Year<-as.integer(substr(med_s_dat$Date,2,5))
med_s_dat$Month<-as.integer(substr(med_s_dat$Date,6,7))
med_s_dat$UnitCode<-med_s_dat$park
med_s_dat$medSmoke<-med_s_dat$Smoke
med_s_dat<-med_s_dat[,4:7]
dat<-merge(dat,med_s_dat,by=c("Year","Month","UnitCode"))
dat$stdsmokemed<-as.vector(scale(dat$medSmoke))

#Create a season variable
dat$Season<-ifelse(dat$Month %in% c(3:5),"Spring",
                    ifelse(dat$Month %in% c(6:8),"Summer",
                           ifelse(dat$Month %in% c(9:11),"Fall","Winter")))

#make combined variables for park/season and park/month
dat$CatColS<-paste0(dat$UnitCode,dat$Season)

dat$CatColM<-paste0(dat$UnitCode,dat$Month)


#add vis season
x<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==max(mv))%>%mutate(cc=paste0(UnitCode,Season))

x2<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==min(mv))%>%mutate(cc=paste0(UnitCode,Season))

dat$SeasType<-ifelse(dat$CatColS %in% x$cc,
                      "High",ifelse(dat$CatColS %in% x2$cc,"Low","Shoulder"))



#create the csv
write.csv(dat, file="Data/MergedDataCompleteFINAL2.csv")
