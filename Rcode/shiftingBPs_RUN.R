library(tidyverse)
library(rstan)
library(ggridges)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
head(dat)
range(dat$Year)
dat$halfDec<-ifelse(dat$Year %in% 1980:1984,"1980_1984",
                    ifelse(dat$Year %in% 1985:1989,"1985_1989",
                           ifelse(dat$Year %in% 1990:1994,"1990_1994",
                                  ifelse(dat$Year %in% 1995:1999,"1995_1999",
                                         ifelse(dat$Year %in% 2000:2004,"2000_2004",
                                                ifelse(dat$Year %in% 2005:2009,"2005_2010",
                                                       ifelse(dat$Year %in% 2010:2014,"2010_2014",
                                                              ifelse(dat$Year %in% 2015:2019,"2015_2019","XXX"))))))))


ggplot(dat, aes(x = Smoke, y = halfDec)) + geom_density_ridges()+scale_x_sqrt()


ggplot(dat, aes(x = Smoke, y = as.character(Year))) + geom_density_ridges()+scale_x_log10()

dat%>%filter(halfDec =="2015_2019")%>%
  ggplot(.,aes(x=medSmoke,y=RecreationVisits))+
  geom_point()+geom_smooth(se=F)+facet_wrap(~UnitCode,scales = "free")

dat%>%filter(halfDec =="1995_1999")%>%
  ggplot(.,aes(x=medSmoke,y=RecreationVisits))+
  geom_point()+geom_smooth(se=F)+facet_wrap(~UnitCode,scales = "free")

ggplot(dat,aes(x=medSmoke,y=RecreationVisits,color=UnitCode))+
  geom_point()+geom_smooth(se=F,method="lm")+facet_wrap(~halfDec,scales = "free")


data<-dat

dat<-data%>%filter(halfDec =="1995_1999")


data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$medSmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=3)

rstan::rstan_options(autowrite=TRUE)


#run the moddy boi
moddy<-rstan::stan( file="~/SmokeProject/StanCode/ShiftingBPs.stan" , 
                    data=data_list,chains=3,iter=5000,
                    control=list(adapt_delta=0.95,max_treedepth = 12) ,warmup = 2500 )


print( moddy , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

#loop through each 5 year period
timez<-unique(data$halfDec)

for(i in timez){
  #make dataframes for each 5 year period
  
  x<-filter(data,halfDec == i)
  
  #make data lists for each 5 year period
  data_list <- list(
    N = nrow(x),
    Nprk = length(unique(x$UnitCode)),
    count = x$RecreationVisits,
    smoke = x$medSmoke,
    pcode = as.numeric(as.factor(x$UnitCode )))
  #make models for each 5 year period
  assign(paste0("mod",i),
         rstan::stan( file="~/SmokeProject/StanCode/ShiftingBPs.stan" , 
                                      data=data_list,chains=3,iter=50,
                                      control=list(adapt_delta=0.95,max_treedepth = 12) ,
                      warmup = 25 ))
  
  
}





