library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
Data<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
Data$VisChange<-(Data$RecreationVisits-Data$ARVis)/Data$ARVis
Data<-na.omit(Data)
#add smoke season
#x<-Data%>%group_by(UnitCode,Season)%>%
# summarise(ms=mean(Smoke))%>%ungroup()%>%
#group_by(UnitCode)%>%filter(ms==max(ms))%>%mutate(cc=paste0(UnitCode,Season))


#Data$SmokeSeas<-ifelse(Data$CatColS %in% x$cc,
#                      "High","NotHigh")

#Data<-Data%>%filter( SmokeSeas =="High")
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

library(rstanarm)
options(mc.cores=parallel::detectCores())
fit<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=dat,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
save(fit,file="residuals.rda")


bayesplot::color_scheme_set("viridisC")
(trace <- plot(fit, "trace", pars = c("b[stdsmoke UnitCode:ZION]","b[stdsmoke UnitCode:YELL]","b[stdsmoke UnitCode:SEQU]","(Intercept)")))
