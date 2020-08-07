library(rstan)
library(rstanarm)
library(tidyverse)
Data<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
Data$Date<-zoo::as.yearmon(paste(Data$Year, Data$Month), "%Y %m")
Data<-Data[order(Data$UnitCode, Data$Date),]
Data<-Data%>%filter( SeasType =="High")
fits <- lme4::lmList(RecreationVisits ~ Date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$Date)
Data$VisDiff<-as.vector(Data$RecreationVisits-Data$trendvis)
load("~/SmokeProject/ModelObjects/Residuals_bkpoint_HS.rda")

Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=Date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendvis),color="green")+ggtitle("Yosemite NP")

#print estimates
print( fit_HS , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

#table of park numbers
formattable::formattable(data.frame(number=c(1:32),park=as.factor(unique(dat$UnitCode )))) 


slope1s<-vector()
for(i in 1:24){slope1s[i]<-paste0("slope1","[",i,"]")}

slope1plot<-rstan::plot(fit_HS, pars = slope1s,
                        prob = 0.5, prob_outer = 0.9)


slope2s<-vector()
for(i in 1:24){slope2s[i]<-paste0("slope2","[",i,"]")}

slope2plot<-rstan::plot(fit_HS, pars = slope2s,
                        prob = 0.5, prob_outer = 0.9)


bkpts<-vector()
for(i in 1:24){bkpts[i]<-paste0("bkpoint","[",i,"]")}

bkpointplot<-rstan::plot(fit_HS, pars = bkpts,
                         prob = 0.5, prob_outer = 0.9)


Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
high_smoke_parks<-Data%>%group_by(UnitCode)%>%
  summarise(max_smoke=max(stdsmoke))%>%
  filter(max_smoke>=0.5)

high_smoke_parks<-dat%>%filter(UnitCode %in% high_smoke_parks$UnitCode)
x<-as.data.frame(fit_HS)
x<-apply(x,2,mean)
x<-as.data.frame(x)
x<-data.frame(UnitCode= unique(high_smoke_parks$UnitCode), slope1=x[2:25,1],slope2=x[26:49,1],ran_int = x[51:74,1],bkpt=x[76:99,1],intercept=rep(x[1,1],24))




dat%>%filter(UnitCode == "REDW")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[19,]$intercept-x[19,]$ran_int), slope =x[19,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[19,]$bkpt)+
  geom_abline(data=x, intercept = (x[19,]$intercept-x[19,]$ran_int)+
                (x[19,]$slope1*x[19,]$bkpt-(x[19,]$bkpt*x[19,]$slope2)), slope = x[19,]$slope2, color="red", 
              linetype="dashed", size=1 )



dat%>%filter(UnitCode == "YOSE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[23,]$intercept-x[23,]$ran_int), slope =x[23,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[23,]$bkpt)+
  geom_abline(data=x, intercept = (x[23,]$intercept-x[23,]$ran_int)+
                (x[23,]$slope1*x[23,]$bkpt-(x[23,]$bkpt*x[23,]$slope2)), slope = x[23,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "CARE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[4,]$intercept-x[4,]$ran_int), slope =x[4,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[4,]$bkpt)+
  geom_abline(data=x, intercept = (x[4,]$intercept-x[4,]$ran_int)+
                (x[4,]$slope1*x[4,]$bkpt-(x[4,]$bkpt*x[4,]$slope2)), slope = x[4,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "ROMO")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[20,]$intercept-x[20,]$ran_int), slope =x[20,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[20,]$bkpt)+
  geom_abline(data=x, intercept = (x[20,]$intercept-x[20,]$ran_int)+
                (x[20,]$slope1*x[20,]$bkpt-(x[20,]$bkpt*x[20,]$slope2)), slope = x[20,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "CRLA")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[7,]$intercept-x[7,]$ran_int), slope =x[7,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[7,]$bkpt)+
  geom_abline(data=x, intercept = (x[7,]$intercept-x[7,]$ran_int)+
                (x[7,]$slope1*x[7,]$bkpt-(x[7,]$bkpt*x[7,]$slope2)), slope = x[7,]$slope2, color="red", 
              linetype="dashed", size=1 )
