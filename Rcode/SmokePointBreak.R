library(tidyverse)
library(rstan)
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
dat<-dat%>% filter(UnitCode !=c("GRSM"))
x<-dat%>%group_by(UnitCode)%>%summarize(max=max(RecreationVisits))%>%arrange(desc(max))
x<-x[1:6,]$UnitCode
dat<-dat%>%filter(UnitCode %in% x)

dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
x<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=max(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==max(mv))%>%mutate(cc=paste0(UnitCode,Season))

x2<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=min(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==min(mv))%>%mutate(cc=paste0(UnitCode,Season))

dat$SeasType<-ifelse(dat$CatColS %in% x$cc,
                     "High",ifelse(dat$CatColS %in% x2$cc,"Low","Shoulder"))

dat<-dat%>%filter(SeasType =="High")

data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmokepark,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=3)

l<- stan( file="SmokePointBreak.stan" , data=data_list,chains=3 )

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )



data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmokepark,
  pcode = as.numeric(as.factor(dat$UnitCode )),
  NSPint = length(unique(dat$CatColS)),
  SPint = as.numeric(as.factor(dat$CatColS ))
  )

options(mc.cores=3)
l<- stan( file="SmokePointBreak.stan" , data=data_list,chains=1 )

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )
summary(l)





#now do with full ds
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")




dat$id<-as.numeric(as.factor(dat$UnitCode ))
data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmokepark,
  pcode = dat$id)

l<- stan( file="IndividualPointBreak.stan" , data=data_list,chains=3 )

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")



ggplot(data=dat,aes(x=stdsmokepark,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(color="red",se=FALSE,alpha=0.1)+
  theme_classic()+ggtitle("max")


dat%>%filter(RecreationVisits < 50000)%>%
  ggplot(.,aes(x=stdsmokepark,y=RecreationVisits,by=UnitCode))+
  geom_point(alpha=0.2)+
  geom_smooth(aes(color=UnitCode),se=FALSE,alpha=0.1)+
  theme_classic()+ggtitle("max")


z<-dat[dat$UnitCode=="GRCA",]$Smoke
zz<-z-mean(z)
zz/(2*sd(z))


stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}

sdd1<-function(x) {return(x-mean(x))}
sdd2<-function(x) {return(x/(2*sd(x)))}
plot(sdd2(sdd1(z)),z)
stdize(z)
