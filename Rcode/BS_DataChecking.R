library(tidyverse)
column_dat<-reBS_dat_clean<-function(data){
data<-tidyr::gather(data,key="Date",value="Smoke",m_198001:m_202009)
  
data$Year<-as.integer(substr(data$Date,3,6))
data$Month<-as.integer(substr(data$Date,7,8))
data$UnitCode<-mean_s_dat$park
data<-data[,3:6]
data<-data%>%dplyr::filter(Year%in%1980:2019)
data<-data[-9959,] #remove a row we don't havedata on for some reason
return(data)
}ad.csv("~/SmokeProject/Data/BS_Data/BS_column_mean.csv")
surface_dat<-read.csv("~/SmokeProject/Data/BS_Data/BS_surface_mean.csv")
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]


column_dat<-BS_dat_clean(column_dat)
surface_dat<-BS_dat_clean(surface_dat)


vec1<-paste0(dat$Year,dat$Month,dat$UnitCode)
vec2<-paste0(surface_dat$Year,surface_dat$Month,surface_dat$UnitCode)

which(vec2 %in% vec1 ==FALSE)

surface_dat$SurfaceBS<-surface_dat$Smoke
surface_dat$ColumnBS<-column_dat$Smoke
surface_dat<-surface_dat[,-1]
dat<-merge(dat,surface_dat,by=c("Year","Month","UnitCode"))

plot(dat$ColumnBS,dat$SurfaceBS)
plot(dat$Smoke,dat$SurfaceBS)
plot(dat$Smoke,dat$ColumnBS)
plot(dat$medSmoke,dat$SurfaceBS)
plot(dat$medSmoke,dat$ColumnBS)

#plot
ggplot(dat,aes(x=SurfaceBS,y=RecreationVisits))+geom_point()+
  geom_smooth(se=F,method="loess")+facet_wrap(~UnitCode,scales="free")






