library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
Data$Date<-zoo::as.yearmon(paste(Data$Year, Data$Month), "%Y %m")
Data<-Data[order(Data$UnitCode, Data$Date),]
#Data<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
#Data$VisChange<-(Data$RecreationVisits-Data$ARVis)/Data$ARVis
#Data<-na.omit(Data)
#add smoke season
x<-Data%>%group_by(UnitCode,Season)%>%
  summarise(ms=mean(Smoke))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(ms==max(ms))%>%mutate(cc=paste0(UnitCode,Season))


Data$SmokeSeas<-ifelse(Data$CatColS %in% x$cc,
                       "High","NotHigh")

Data<-Data%>%filter( SmokeSeas =="High")
#filter by high season
Data<-Data%>%filter( SeasType =="High")

#filter by smoke affected regions
#Data<-Data%>%
 # filter(Region %in% c("Intermountain","Pacific West"))


#get vis and smoke trends
fits <- lmList(Smoke ~ Date | UnitCode, data=Data) 
Data$trendsmoke<-predict(fits,date=Data$Date)

fits <- lmList(RecreationVisits ~ Date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$Date)

Data$SmokeDiff<-scale(Data$Smoke-Data$trendsmoke)
Data$VisDiff<-Data$RecreationVisits-Data$trendvis
dat<-Data
#plot it to make sure it worked
Data%>%filter(UnitCode=="BIBE")%>%
ggplot(.,aes(x=Date,y=Smoke))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendsmoke),color="green")


Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=Date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendvis),color="green")+ggtitle("Yosemite NP")






Data%>%
  ggplot(.,aes(x=Smoke,y=VisDiff))+
  geom_point()+
  geom_smooth(se=F,method="lm")+theme_classic()+facet_wrap(~UnitCode,scales="free")+
  ggtitle("mean smoke")

Data%>%
  ggplot(.,aes(x=MAXSmoke,y=VisDiff))+
  geom_point()+
  geom_smooth(se=F,method="lm")+theme_classic()+facet_wrap(~UnitCode,scales="free")+
  ggtitle("max smoke")

Data%>%
  ggplot(.,aes(x=Smoke,y=VisDiff))+
  geom_point()+
  geom_smooth(se=F)+theme_classic()+facet_wrap(~UnitCode,scales="free")+
  ggtitle("mean smoke")

Data%>%
  ggplot(.,aes(x=MAXSmoke,y=VisDiff))+
  geom_point()+
  geom_smooth(se=F)+theme_classic()+facet_wrap(~UnitCode,scales="free")+
  ggtitle("max smoke")

Data%>%
  ggplot(.,aes(x=Smoke,y=RecreationVisits))+
  geom_point()+
  geom_smooth(se=F,method="lm")+theme_classic()+facet_wrap(~UnitCode,scales="free")


Data%>%
  ggplot(.,aes(x=SmokeDiff,y=RecreationVisits))+
  geom_point()+
  geom_smooth(se=F,method="lm")+theme_classic()+facet_wrap(~UnitCode,scales="free")

options(mc.cores=3)
fit1<-rstanarm::stan_glmer(VisDiff~SmokeDiff|UnitCode,data=Data,chains=3)
plot(fit1, regex_pars = "SmokeDiff UnitCode",
     prob = 0.5, prob_outer = 0.9)
plot(fit1, regex_pars = "stdsmoke UnitCode",
          prob = 0.5, prob_outer = 0.9)

bayesplot::color_scheme_set("viridisC")
(trace <- plot(fit, "trace", pars = c("b[stdsmoke UnitCode:ZION]","b[stdsmoke UnitCode:YELL]","b[stdsmoke UnitCode:SEQU]","(Intercept)")))








data_list4 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  vis = as.vector(dat$VisDiff),
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)
rstan::rstan_options(auto_write=T)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
M5<- rstan::stan( file="~/SmokeProject/StanCode/Residuals.stan" , 
                  data=data_list4,chains=1 ,warmup=80,iter = 600, control = list(max_treedepth = 25))


print( M5 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )
x<-as.data.frame(summary(M5))
zz<-row.names(x[2:4,])
stan_plot(M5,pars = c(zz) ,fill_color = "purple", )


###############################

#poisson growth rate model
library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
Data<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
Data$VisChange<-(Data$RecreationVisits-Data$ARVis)/Data$ARVis
Data<-na.omit(Data)

#filter by high season
Data<-Data%>%filter( SeasType =="High")

#filter by smoke affected regions
Data<-Data%>%
  filter(Region %in% c("Intermountain","Pacific West"))
dat<-Data
dat<-dat[1:342,]


data_list2 <- list(
  N = nrow(dat),
  ARVis = dat$ARVis,
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)
rstan::rstan_options(auto_write=T)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
M5<- rstan::stan( file="~/SmokeProject/StanCode/Poisson_growth_rate.stan" , 
                  data=data_list2,chains=1 ,warmup=100,iter = 200, control = list(max_treedepth = 25))







