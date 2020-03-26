library(tidyverse)
library(rstan)
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
dat<-dat%>% filter(UnitCode !=c("GRSM"))
x<-dat%>%group_by(UnitCode)%>%summarize(max=max(RecreationVisits))%>%arrange(desc(max))
x<-x[1:8,]$UnitCode
dat<-dat%>% filter(UnitCode !=c("GRCA"))
dat<-dat%>% filter(UnitCode !=c("JEFF"))
dat<-dat%>% filter(UnitCode !=c("ACAD"))
dat<-dat%>% filter(UnitCode !=c("OLYM"))
dat<-dat%>% filter(UnitCode !=c("GLAC"))
dat<-dat%>%filter(UnitCode %in% x)

ggplot(data=dat,aes(x=stdsmoke,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(color="red",se=FALSE,alpha=0.1)+
  theme_classic()+ggtitle("Standardized Overall")

ggplot(data=dat,aes(x=stdsmokepark,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(color="red",se=FALSE,alpha=0.1)+
  theme_classic()+ggtitle("Standardized by Park")


data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmokepark,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=3)

l<- stan( file="SmokePointBreak.stan" , data=data_list,chains=3 )

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )