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




dat%>%filter(UnitCode == "REDW")%>%
ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+geom_smooth(se=F)




dat%>%filter(UnitCode == "REDW")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
geom_abline(intercept = (-5.65-415.10), slope = 0.29, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
  
  geom_abline(intercept = (-5.65-415.10)+(0.29*2.94-(2.94*-405.09)), slope = -405.09, color="red", 
              linetype="dashed", size=1 )

Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
x<-as.data.frame(zz)
x<-apply(x,2,mean)
x<-as.data.frame(x)
x<-data.frame(UnitCode= unique(dat$UnitCode), slope1=x[2:33,1],slope2=x[34:65,1],ran_int = x[67:98,1],bkpt=x[100:131,1],intercept=rep(x[1,1],32))

dat%>%filter(UnitCode == "REDW")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(intercept = (-5.65-415.10), slope = 0.29, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
  
  geom_abline(intercept = (-5.65-415.10)+(0.29*2.94-(2.94*-405.09)), slope = -405.09, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "REDW")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[26,]$intercept-x[26,]$ran_int), slope =x[26,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
   geom_abline(data=x, intercept = (x[26,]$intercept-x[26,]$ran_int)+
                (x[26,]$slope1*x[26,]$bkpt-(x[26,]$bkpt*x[26,]$slope2)), slope = x[26,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "CAVE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[7,]$intercept-x[7,]$ran_int), slope =x[7,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
  geom_abline(data=x, intercept = (x[7,]$intercept-x[7,]$ran_int)+
                (x[7,]$slope1*x[7,]$bkpt-(x[7,]$bkpt*x[7,]$slope2)), slope = x[7,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "YOSE")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[31,]$intercept-x[31,]$ran_int), slope =x[31,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
  geom_abline(data=x, intercept = (x[31,]$intercept-x[31,]$ran_int)+
                (x[31,]$slope1*x[31,]$bkpt-(x[31,]$bkpt*x[31,]$slope2)), slope = x[31,]$slope2, color="red", 
              linetype="dashed", size=1 )


dat%>%filter(UnitCode == "OLYM")%>%
  ggplot(., aes(x=stdsmoke,y=VisDiff))+geom_point()+
  geom_abline(data=x,intercept = (x[23,]$intercept-x[23,]$ran_int), slope =x[23,]$slope1, color="blue", 
              linetype="dashed", size=1)+ geom_vline(xintercept = 2.94)+
  geom_abline(data=x, intercept = (x[23,]$intercept-x[23,]$ran_int)+
                (x[23,]$slope1*x[23,]$bkpt-(x[23,]$bkpt*x[23,]$slope2)), slope = x[23,]$slope2, color="red", 
              linetype="dashed", size=1 )
