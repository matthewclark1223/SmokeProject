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



data<-dat

#dat<-data%>%filter(halfDec =="2015_2019")

dat$AR_Vis<-lag(dat$RecreationVisits,n=384)
dat[385:1920,]$AR_Vis==dat[1:1536,]$RecreationVisits

dat<-na.omit(dat)

data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  arVis = dat$AR_Vis,
  pcode = as.numeric(as.factor(dat$UnitCode )),
  smoke = dat$medSmoke)


options(mc.cores=8)

rstan::rstan_options(autowrite=TRUE)

#run the moddy boi
mod<-rstan::stan( file="~/SmokeProject/StanCode/Heierarchical_AR_Model.stan" , 
                           data=data_list,chains=8,iter=5000 ,warmup = 2500 )


print( mod , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


shinystan::launch_shinystan(mod)
