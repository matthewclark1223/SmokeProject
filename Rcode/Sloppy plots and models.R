library(tidyverse)
library(lme4)
library(rstanarm)
library(MASS)
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")

fit2<-glmer.nb(RecreationVisits~stdsmoke+(Season|UnitCode),data=dat)

fit3<-glmer.nb(RecreationVisits~ScaledSmoke+(1|CatColM),data=dat)
fit4<-glm(RecreationVisits~Smoke,family="poisson",data=dat)

newdata <- data.frame(
  ScaledSmoke = seq(from = min(dat[dat$CatColM=="YOSE07",]$Smoke), to = max(dat[dat$CatColM=="YOSE07",]$Smoke), length.out = 100),
  CatColM = rep("YOSE07",100))
  
newdata2 <- cbind(newdata, predict(fit3, newdata))

nd<-data.frame(Smoke=0.00316, CatColM="ARCH06")
predict(fit2, nd)



ggplot(data=dat[1:6959,],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
facet_wrap(~ParkName, scales="free")
  
ggplot(data=dat[6960:13955,],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
  facet_wrap(~ParkName,, scales="free")

ggplot(data=dat[13956:20891,],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
  facet_wrap(~ParkName,, scales="free")

ggplot(data=dat[20892:nrow(dat),],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
  facet_wrap(~ParkName, scales="free")
  
ggplot(data=dat[dat$UnitCode %in% c("YOSE", "YELL","ACAD","GRSM","JOTR","SEQU"),],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
  geom_point(aes(color=Month))+facet_wrap(~ParkName, scales="free")
  
ggplot(data=dat[dat$UnitCode %in% c("YOSE", "YELL","ACAD","GRSM","JOTR","SEQU"),],aes(x=Year,y=RecreationVisits))+
  geom_point()+facet_wrap(~ParkName, scales="free")+
  geom_smooth(method="lm", se=F, color="red", size=2)

ggplot(data=dat[dat$UnitCode %in% c("YOSE", "YELL","ACAD","GRSM","JOTR","SEQU"),],aes(x=Year,y=Smoke))+
  geom_point()+facet_wrap(~ParkName, scales="free")+
  geom_smooth(method="lm", se=F, color="red", size=2)

ggplot(data=dat[dat$UnitCode %in% c("YOSE", "YELL","ACAD","GRSM","JOTR","SEQU"),],aes(x=Smoke,y=RecreationVisits))+
  geom_smooth(aes(color=Month),method="lm", se=F)+
  geom_point(aes(color=Month))+facet_wrap(~ParkName, scales="free")


stdize(head(dat$Smoke))
stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}
2*sd(head(dat$Smoke))

head(dat$Smoke)-mean(head(dat$Smoke))/(2*sd(head(dat$Smoke)))



