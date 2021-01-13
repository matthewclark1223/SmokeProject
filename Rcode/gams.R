library(tidyverse)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
head(dat)
library(mgcv)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
dat$halfDec<-ifelse(dat$Year %in% 1980:1984,"1980_1984",
                    ifelse(dat$Year %in% 1985:1989,"1985_1989",
                           ifelse(dat$Year %in% 1990:1994,"1990_1994",
                                  ifelse(dat$Year %in% 1995:1999,"1995_1999",
                                         ifelse(dat$Year %in% 2000:2004,"2000_2004",
                                                ifelse(dat$Year %in% 2005:2009,"2005_2010",
                                                       ifelse(dat$Year %in% 2010:2014,"2010_2014",
                                                              ifelse(dat$Year %in% 2015:2019,"2015_2019","XXX"))))))))
dat$AR_Vis<-lag(dat$RecreationVisits,n=384)
dat[385:1920,]$AR_Vis==dat[1:1536,]$RecreationVisits

dat<-na.omit(dat)
data<-dat%>%filter(halfDec=="2015_2019")
data$UnitCode<-as.factor(data$UnitCode)

mod<-gam(RecreationVisits~s(Smoke,by=UnitCode)+s(AR_Vis,by=UnitCode)+UnitCode,
         data=data,method="REML")
plot(mod,residuals=TRUE,pch=1,cex=1,shade=T,
     shade.col="lightblue")

mod2<-gam(RecreationVisits~s(Smoke,by=UnitCode)+s(AR_Vis,UnitCode, bs="fs"),
         data=data,method="REML")

vis.gam(mod, view = c("Smoke", "UnitCode"), plot.type = "persp")
vis.gam(mod2, view = c("Smoke", "UnitCode"), plot.type = "persp")


mod<-gam(RecreationVisits~s(Smoke)+UnitCode+s(AR_Vis),
         data=data,method="REML",family=nb())
plot(mod,residuals=TRUE,seWithMean=TRUE, 
     shift = coef(mod)[1],pch=1,cex=1,shade=T,
     shade.col="lightblue")

mod<-gam(RecreationVisits~s(Smoke,by=UnitCode)+UnitCode+s(AR_Vis,by=UnitCode),
         data=data,method="REML",family=nb())
plot(mod,pages=1)
summary(mod)





stdize<-function(x){
  (x-mean(x))/(2*sd(x))}

