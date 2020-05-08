library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
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
fits <- lmList(stdsmoke ~ date | UnitCode, data=Data) 
Data$trendsmoke<-predict(fits,date=allData$date)

fits <- lmList(RecreationVisits ~ date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$date)


#plot it to make sure it worked
Data%>%filter(UnitCode=="YELL")%>%
ggplot(.,aes(x=date,y=stdsmoke))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=date,y=trendsmoke),color="green")


Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=date,y=trendvis),color="green")+ggtitle("Yosemite NP")


Data$SmokeDiff<-Data$stdsmoke-Data$trendsmoke
Data$VisDiff<-as.integer(Data$RecreationVisits-round(Data$trendvis,digits=0))


#Model
dat<-Data

data_list3 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$VisDiff,
  smoke = dat$SmokeDiff,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=parallel::detectCores()-1)
rstan_options(auto_write=T)

M1<- stan( file="~/SmokeProject/StanCode/SmokeNegBinom.stan" , 
           data=data_list3,chains=3 ,warmup=100,iter = 800, control = list(max_treedepth = 25))



print( M1 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(M1, file="smokeRandomEffectsSubset.rda")

x<-as.data.frame(summary(M1))
zz<-row.names(x[2:34,])
stan_plot(M1,pars = c(zz) ,fill_color = "purple", )





