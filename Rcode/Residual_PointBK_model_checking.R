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
load("~/SmokeProject/ModelObjects/Residuals_bkpoint.rda")

Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=Date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendvis),color="green")+ggtitle("Yosemite NP")

#print estimates
print( zz , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

#table of park numbers
formattable::formattable(data.frame(number=c(1:32),park=as.factor(unique(dat$UnitCode )))) 


slope1s<-vector()
 for(i in 1:32){slope1s[i]<-paste0("slope1","[",i,"]")}

slope1plot<-rstan::plot(zz, pars = slope1s,
           prob = 0.5, prob_outer = 0.9)


slope2s<-vector()
for(i in 1:32){slope2s[i]<-paste0("slope2","[",i,"]")}

slope2plot<-rstan::plot(zz, pars = slope2s,
            prob = 0.5, prob_outer = 0.9)


bkpts<-vector()
for(i in 1:32){bkpts[i]<-paste0("bkpoint","[",i,"]")}

bkpointplot<-rstan::plot(zz, pars = bkpts,
                        prob = 0.5, prob_outer = 0.9)



Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
x<-as.data.frame(zz)
x<-apply(x,2,mean)
x<-as.data.frame(x)
x<-data.frame(UnitCode= unique(dat$UnitCode), slope1=x[2:33,1],slope2=x[34:65,1],ran_int = x[67:98,1],bkpt=x[100:131,1],intercept=rep(x[1,1],32))




dat%>%filter(UnitCode == "REDW")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[26,]$intercept-x[26,]$ran_int), slope =x[26,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[26,]$bkpt)+
   geom_abline(data=x, intercept = (x[26,]$intercept-x[26,]$ran_int)+
                (x[26,]$slope1*x[26,]$bkpt-(x[26,]$bkpt*x[26,]$slope2)), slope = x[26,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "CAVE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[7,]$intercept-x[7,]$ran_int), slope =x[7,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[7,]$bkpt)+
  geom_abline(data=x, intercept = (x[7,]$intercept-x[7,]$ran_int)+
                (x[7,]$slope1*x[7,]$bkpt-(x[7,]$bkpt*x[7,]$slope2)), slope = x[7,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "YOSE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[31,]$intercept-x[31,]$ran_int), slope =x[31,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[31,]$bkpt)+
  geom_abline(data=x, intercept = (x[31,]$intercept-x[31,]$ran_int)+
                (x[31,]$slope1*x[31,]$bkpt-(x[31,]$bkpt*x[31,]$slope2)), slope = x[31,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "OLYM")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[23,]$intercept-x[23,]$ran_int), slope =x[23,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[23,]$bkpt)+
  geom_abline(data=x, intercept = (x[23,]$intercept-x[23,]$ran_int)+
                (x[23,]$slope1*x[23,]$bkpt-(x[23,]$bkpt*x[23,]$slope2)), slope = x[23,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "GRBA")%>%   #i think this will be bad
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[12,]$intercept-x[12,]$ran_int), slope =x[12,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[12,]$bkpt)+
  geom_abline(data=x, intercept = (x[12,]$intercept-x[12,]$ran_int)+
                (x[12,]$slope1*x[12,]$bkpt-(x[12,]$bkpt*x[12,]$slope2)), slope = x[12,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "CARE")%>% #i think this will be good
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[6,]$intercept-x[6,]$ran_int), slope =x[6,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[6,]$bkpt)+
  geom_abline(data=x, intercept = (x[6,]$intercept-x[6,]$ran_int)+
                (x[6,]$slope1*x[6,]$bkpt-(x[6,]$bkpt*x[6,]$slope2)), slope = x[6,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "ROMO")%>% #i think this will be good
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[27,]$intercept-x[27,]$ran_int), slope =x[27,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[27,]$bkpt)+
  geom_abline(data=x, intercept = (x[27,]$intercept-x[27,]$ran_int)+
                (x[27,]$slope1*x[27,]$bkpt-(x[27,]$bkpt*x[27,]$slope2)), slope = x[27,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "LAVO")%>% 
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[19,]$intercept-x[19,]$ran_int), slope =x[19,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[19,]$bkpt)+
  geom_abline(data=x, intercept = (x[19,]$intercept-x[19,]$ran_int)+
                (x[19,]$slope1*x[19,]$bkpt-(x[19,]$bkpt*x[19,]$slope2)), slope = x[19,]$slope2, color="red", 
              linetype="dashed", size=1 )

dat%>%filter(UnitCode == "PINN")%>% 
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[25,]$intercept-x[25,]$ran_int), slope =x[25,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = x[25,]$bkpt)+
  geom_abline(data=x, intercept = (x[25,]$intercept-x[25,]$ran_int)+
                (x[25,]$slope1*x[25,]$bkpt-(x[25,]$bkpt*x[25,]$slope2)), slope = x[25,]$slope2, color="red", 
              linetype="dashed", size=1 )



dat%>%filter(UnitCode %in% c("CRLA","LAVO"))%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+geom_smooth(se=F)+facet_wrap(~UnitCode,scales="free")


dat%>%filter(UnitCode %in% c("CRLA","LAVO"))%>%
  ggplot(., aes(x=Date,y=stdsmoke))+geom_point()+geom_smooth(se=F)+facet_wrap(~UnitCode,scales="free")


