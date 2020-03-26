library(tidyverse)
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")


ggplot(data=dat,aes(x=stdsmoke,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm",color="red",se=FALSE)+
  theme_classic()+ggtitle("max")

ggplot(data=dat,aes(x=stdsmoke,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(color="red",se=FALSE,alpha=0.1)+
  theme_classic()+ggtitle("max")




ggplot(data=dat,aes(x=Year,y=stdsmoke,by=UnitCode))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm",color="red",se=FALSE,alpha=0.2)+
  theme_classic()

ggplot(data=dat,aes(x=Year,y=RecreationVisits,by=UnitCode))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm",color="red",se=FALSE,alpha=0.2)+
  theme_classic()



dat$LE<- ifelse(datmax$stdsmoke >=2,"True","False")

ggplot(data=dat,aes(x=LE,y=RecreationVisits))+
  geom_boxplot()+
  theme_classic()


stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}

dat<-dat%>%group_by(UnitCode)%>%
  mutate(stdsmokepark=stdize(stdsmoke))

ggplot(data=datmax,aes(x=stdsmokepark,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm",color="red",se=FALSE,alpha=0.2)+
  theme_classic()+ggtitle("max")

ggplot(data=datmax,aes(x=stdsmokepark,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(color="red",se=FALSE,alpha=0.2)+
  theme_classic()+ggtitle("max")


#stop here

library(zoo)
dat$Date<-zoo::as.yearmon(paste(as.numeric(dat$Year),
                      dat$Month,sep="-"))



ggplot(dat,aes(x=Date,y=stdsmoke))+geom_point()+geom_smooth(method="lm")

#split by just summer 
dat%>%filter(Season=="Summer")%>%
ggplot(data=.,aes(x=stdsmoke,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm",color="red",se=FALSE)+
  theme_classic()+ggtitle("max")
