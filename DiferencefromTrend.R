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

Data$SmokeDiff<-scale(Data$Smoke-Data$trendsmoke)
Data$VisDiff<-Data$RecreationVisits-Data$trendvis
dat<-Data
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



############################






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
                  data=data_list4,chains=3 ,warmup=800,iter = 6000, control = list(max_treedepth = 25))


print( M5 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )
x<-as.data.frame(summary(M5))
zz<-row.names(x[2:4,])
stan_plot(M5,pars = c(zz) ,fill_color = "purple", )


###############################









