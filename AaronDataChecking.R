library(tidyverse)
aaron_BC<-read.csv("~/SmokeProject/Aaron/ASCII/means_BC.csv")
aaron_PM<-read.csv("~/SmokeProject/Aaron/ASCII/means_PM.csv")
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]


aaron_dat_clean<-function(data){
  data<-tidyr::gather(data,key="Date",value="Smoke",X2000.01.01:X2017.12.01)
  
  data$Year<-as.integer(substr(data$Date,2,5))  #stopped here
  data$Month<-as.integer(substr(data$Date,7,8))
  data$UnitCode<-data$park
  data<-data[,3:6]
  data<-data[-2279,]#remove a row we don't have data on for some reason
  return(data)
}


 

dat<-dat%>%filter(Year %in% 2000:2017)

aaron_BC<-aaron_dat_clean(aaron_BC)
aaron_PM<-aaron_dat_clean(aaron_PM) ###Doesn't work, PM has more smokeobs!!


#vec1<-paste0(dat$Year,dat$Month,dat$UnitCode)
#vec2<-paste0(aaron_BC$Year,aaron_BC$Month,aaron_BC$UnitCode)

#which(vec2 %in% vec1 ==FALSE)
