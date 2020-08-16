library(tidyverse)
library(rstan)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
dat$Date<-zoo::as.yearmon(paste(dat$Year, dat$Month), "%Y %m")
dat<-dat[order(dat$UnitCode, dat$Date),]
dat<-dat%>%filter( SeasType =="High")
fits <- lme4::lmList(RecreationVisits ~ Date | UnitCode, data=dat) 
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

rstan::rstan_options(autowrite=TRUE)


#this converged
zz<-stan( file="~/SmokeProject/StanCode/Residuals_Break_Point.stan" , data=data_list,chains=8,iter=8000,control=list(adapt_delta=0.99,max_treedepth = 12) ,warmup = 4000 )


print( zz , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


save(zz,file="~/SmokeProject/ModelObjects/Residuals_bkpoint.rda")





#subset by parks that had smoke >0.5sd
high_smoke_parks<-dat%>%group_by(UnitCode)%>%
  summarise(max_smoke=max(stdsmoke))%>%
  filter(max_smoke>=0.5)

high_smoke_parks<-dat%>%filter(UnitCode %in% high_smoke_parks$UnitCode)

data_list_high_smoke <- list(
  N = nrow(high_smoke_parks),
  Nprk = length(unique(high_smoke_parks$UnitCode)),
  vis = high_smoke_parks$VisDiff,
  smoke = high_smoke_parks$stdsmoke,
  pcode = as.numeric(as.factor(high_smoke_parks$UnitCode )))

options(mc.cores=8)
rstan::rstan_options(autowrite=TRUE)


#this converged
fit_HS<-stan( file="~/SmokeProject/StanCode/Residuals_Break_Point_HS.stan" , data=data_list_high_smoke,chains=8,iter=2000,control=list(adapt_delta=0.99,max_treedepth = 10) ,warmup = 1000 )
print( fit_HS , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


save(fit_HS,file="~/SmokeProject/ModelObjects/Residuals_bkpoint_HS.rda")




