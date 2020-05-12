library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
dat<-dat%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
dat<-na.omit(dat)
#add smoke season
#x<-Data%>%group_by(UnitCode,Season)%>%
 # summarise(ms=mean(Smoke))%>%ungroup()%>%
  #group_by(UnitCode)%>%filter(ms==max(ms))%>%mutate(cc=paste0(UnitCode,Season))
#Data<-Data%>%filter( SmokeSeas =="High")


#Data$SmokeSeas<-ifelse(Data$CatColS %in% x$cc,
 #                      "High","NotHigh")

#filter by high season
Data<-Data%>%filter( SeasType =="High")

#filter by smoke affected regions
Data<-Data%>%
  filter(Region %in% c("Intermountain","Pacific West"))


#get vis and smoke trends
fits <- lmList(Smoke ~ date | UnitCode, data=Data) 
Data$trendsmoke<-predict(fits,date=allData$date)

fits <- lmList(RecreationVisits ~ date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$date)


#plot it to make sure it worked
Data%>%filter(UnitCode=="YELL")%>%
ggplot(.,aes(x=date,y=Smoke))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=date,y=trendsmoke),color="green")


Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=date,y=trendvis),color="green")+ggtitle("Yosemite NP")


Data$SmokeDiff<-scale(Data$Smoke-Data$trendsmoke)
Data$VisDiff<-Data$RecreationVisits-Data$trendvis

Data%>%
  ggplot(.,aes(x=SmokeDiff,y=VisDiff))+
  geom_point(aes(color=UnitCode))+
  geom_smooth(aes(color=UnitCode),se=F,method = "lm")+theme_classic()

Data%>%
  ggplot(.,aes(x=SmokeDiff,y=VisDiff))+
  geom_point()+
  geom_smooth(se=F,method = "lm")+theme_classic()+facet_wrap(~Region)

#Model
dat<-Data

data_list3 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  vis = dat$VisDiff,
  smoke = dat$SmokeDiff,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)
rstan_options(auto_write=T)

M1<- stan( file="~/SmokeProject/StanCode/Residuals.stan" , 
           data=data_list3,chains=3 ,warmup=50,iter = 200, control = list(max_treedepth = 25))

############################
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
Data<-Data%>%filter( SeasType =="High")
fits <- lmList(Smoke ~ date | UnitCode, data=Data) 
Data$trendsmoke<-predict(fits,date=allData$date)

fits <- lmList(RecreationVisits ~ date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$date)
Data$SmokeDiff<-scale(Data$Smoke-Data$trendsmoke)
Data$VisDiff<-Data$RecreationVisits-Data$trendvis
dat<-Data

data_list3 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$Region)),
  vis = as.vector(dat$VisDiff),
  smoke = as.vector(dat$SmokeDiff),
  pcode = as.numeric(as.factor(dat$Region )))

options(mc.cores=parallel::detectCores()-1)
rstan::rstan_options(auto_write=T)

M1<- rstan::stan( file="~/SmokeProject/StanCode/Residuals.stan" , 
           data=data_list3,chains=3 ,warmup=50,iter = 200, control = list(max_treedepth = 25))




###############################


M1<-stan_glmer(VisDiff~SmokeDiff+(SmokeDiff|UnitCode),data=dat)
M2<-lme4::lmer(VisDiff~SmokeDiff+(SmokeDiff|UnitCode),data=dat)

print( M1 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(M1, file="smokeRandomEffectsSubset.rda")

x<-as.data.frame(summary(M1))
zz<-row.names(x[2:34,])
stan_plot(M1,pars = c(zz) ,fill_color = "purple", )





