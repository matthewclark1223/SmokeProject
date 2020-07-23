library(tidyverse)
library(rstan)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
dat$Date<-zoo::as.yearmon(paste(dat$Year, dat$Month), "%Y %m")
dat<-dat[order(dat$UnitCode, dat$Date),]
dat<-dat%>%filter( SeasType =="High")
fits <- lmList(RecreationVisits ~ Date | UnitCode, data=dat) 
dat$trendvis<-predict(fits,date=dat$Date)
dat$VisDiff<-as.vector(dat$RecreationVisits-dat$trendvis)
dat%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=Date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendvis),color="green")+ggtitle("Yosemite NP")





data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  vis = dat$VisDiff,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=8)


l<-stan( file="Residuals_Break_Point.stan" , data=data_list,chains=8,iter=5000,control=list(adapt_delta=0.99) ,warmup = 800 )

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

options(mc.cores=3)

zz<-stan( file="~/SmokeProject/StanCode/Residuals_FIXED_BP.stan" , data=data_list,chains=3 )







