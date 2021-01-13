library(tidyverse)
library(magrittr)
smoke<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
aqi<-read_csv("Data/AQI_Final_Data.csv")[,-1]

library(rjson)
result <- fromJSON(file = "Data/Tetons_AQI/te2007.json")
result <- fromJSON(file = "Data/Tetons_AQI/te2020.json")
json_data_frame <- as.data.frame(result)

result$Data[[252]]$date_local
result$Data[[252]]$aqi

x<-data.frame(date=rep(NA,252),aqi=rep(NA,252))

for(i in 1:252){
  x[i,]$date<-result$Data[[i]]$date_local
  x[i,]$aqi<-result$Data[[i]]$aqi
}
x$Year<-substr(x$date,1,4)
x$Month<-substr(x$date,6,7)
x$Day<-substr(x$date,9,10)

x<-x%>%group_by(Year,Month)%>%summarise(medaqi=median(aqi))
y<-smoke%>%filter(UnitCode == "GRTE")%>%filter(Year ==2007)%>%filter(Month %in% c(6:12))

plot(x$medaqi,y$medSmoke)

#start below
result <- fromJSON(file = "Data/Tetons_AQI/te2007.json")
datt<-data.frame(date=rep(NA,252),aqi=rep(NA,252))

for(i in 1:252){
  datt[i,]$date<-result$Data[[i]]$date_local
  datt[i,]$aqi<-result$Data[[i]]$aqi
}
files<-list.files("Data/Tetons_AQI/")


for(j in files[-1]){
  result<-fromJSON(file = paste0("Data/Tetons_AQI/",j))
  len<-length(result$Data)
  x<-data.frame(date=rep(NA,len),aqi=rep(NA,len))
  for(i in 1:len){
    x[i,]$date<-result$Data[[i]]$date_local
    x[i,]$aqi<-result$Data[[i]]$aqi
  }
  datt<-rbind(datt,x)
  
}
datt%<>%group_by(date)%>%summarise(aqi=median(aqi))
datt$Year<-substr(datt$date,1,4)
datt$Month<-as.numeric(substr(datt$date,6,7))
datt$Day<-substr(datt$date,9,10)

datt%<>%group_by(Year,Month)%>%summarise(medaqi=median(aqi))

y<-smoke%>%filter(UnitCode == "GRTE")%>%filter(Year %in% 2007:2019)%>%select(medSmoke,Year,Month)
y<-na.omit(merge(y,datt,by=c("Month","Year"),all.y=T))
plot(y$medaqi,y$medSmoke)
fit<-lm(medaqi~medSmoke,data=y)
summary(fit)


hist(smoke$medSmoke)
smoke$aqi<-predict(fit, smoke)
quantile(predict(fit, smoke))
smoke[smoke$aqi>100,]

head(aqi)
unique(smoke$UnitCode) %in%unique(aqi$UNIT_CODE)
aqi%<>%filter(UNIT_CODE %in%smoke$UnitCode)
aqi$UnitCode<-aqi$UNIT_CODE
x<-merge(smoke,aqi,by=c("UnitCode","Year","Month"),all.x = TRUE)

naas<-x[is.na(x$Monthly_Med),]
write.csv(naas,file="AQI_NAs.csv")
View(naas)
mean(x[!is.na(x$Monthly_Med),]$stdsmokemed)
fit<-MASS::glm.nb(RecreationVisits~Monthly_Med,data=x)
ggplot(data=x,aes(x=Monthly_Med,y=RecreationVisits))+facet_wrap(~UnitCode,scales="free")+
  geom_point()+geom_smooth(method="REML",se=F)

x[x$Monthly_Med==NA,]
View(x)

cor(x$Smoke,x$Monthly_Med,use="complete.obs")
plot(x$Smoke,x$Monthly_Med)


y<-filter(x,stdsmokemed>1.5)
plot(y$Smoke,y$Monthly_Med)
