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
