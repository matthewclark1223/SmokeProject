library(tidyverse)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]



#add smoke season
x<-Data%>%group_by(UnitCode,Season)%>%
  summarise(ms=mean(Smoke))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(ms==max(ms))%>%mutate(cc=paste0(UnitCode,Season))



Data$SmokeSeas<-ifelse(Data$CatColS %in% x$cc,
                      "High","NotHigh")



#Data<-Data%>%filter( SmokeSeas =="High")
Data<-Data%>%filter( SeasType =="High")

Data<-Data%>%
  filter(Region %in% c("Intermountain","Pacific West"))
##

dat<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )

dat<-na.omit(dat)

dat$stdARV<-as.vector(scale(dat$ARVis))


data_list2 <- list(
  N = nrow(dat),
  previs = dat$stdARV,
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)

A3<- stan( file="~/SmokeProject/StanCode/SmokeNegBinomAutoregressive.stan" , 
           data=data_list2,chains=3 ,warmup=1000,iter = 8000, control = list(max_treedepth = 25))



print( A3 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(A3, file="stdsmokeNbinomAutoregressiveSubset.rda")

x<-as.data.frame(summary(A3))
zz<-row.names(x[2:34,])
stan_plot(A3,pars = c(zz) ,fill_color = "purple", )

## no AR term
dat<-Data

data_list3 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)

M1<- stan( file="~/SmokeProject/StanCode/SmokeNegBinom.stan" , 
           data=data_list3,chains=3 ,warmup=1000,iter = 8000, control = list(max_treedepth = 25))



print( M1 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(M1, file="smokeRandomEffectsSubset.rda")

x<-as.data.frame(summary(M1))
zz<-row.names(x[2:34,])
stan_plot(M1,pars = c(zz) ,fill_color = "purple", )



#try just southeast?

Data<-read_csv("Data/MergedDataComplete.csv")[,-1]


Data<-Data%>%
  filter(Region %in% c("Southeast"))

x<-Data%>%group_by(UnitCode,Season)%>%
  summarise(ms=mean(Smoke))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(ms==max(ms))%>%mutate(cc=paste0(UnitCode,Season))



Data$SmokeSeas<-ifelse(Data$CatColS %in% x$cc,
                       "High","NotHigh")



Data<-Data%>%filter( SmokeSeas =="High")
##

dat<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )

dat<-na.omit(dat)

dat$stdARV<-as.vector(scale(dat$ARVis))


data_list4 <- list(
  N = nrow(dat),
  previs = dat$stdARV,
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)

A4<- stan( file="~/SmokeProject/StanCode/SmokeNegBinomAutoregressive.stan" , 
           data=data_list4,chains=3 ,warmup=1000,iter = 8000, control = list(max_treedepth = 25))

print( A4 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(A4, file="smokeRandomEffectsSoutheast.rda")

x<-as.data.frame(summary(A4))
zz<-row.names(x[2:8,])
stan_plot(A4,pars = c(zz) ,fill_color = "purple", )
