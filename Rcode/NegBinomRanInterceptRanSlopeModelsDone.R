library(tidyverse)
library(rstan)
library(shinystan)
dat<-read_csv("~/SmokeProject/Data/MergedDataComplete.csv")
x<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==max(mv))%>%mutate(cc=paste0(UnitCode,Season))

x2<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
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

l<- stan( file="~/SmokeProject/StanCode/SmokeNegBinom.stan" , data=data_list,chains=3 ,warmup = 1000 ,iter = 10000)

print( l , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(l, file="stdsmokeparkNbinom.rda")

data_list2 <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

l2<- stan( file="~/SmokeProject/StanCode/SmokeNegBinom.stan" , data=data_list2,chains=3 ,warmup=1000 ,iter = 6000)
print( l2 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(l2, file="stdsmokeNbinom.rda")


#Plot outputs
load("stdsmokeparkNbinom.rda")
load("stdsmokeNbinom.rda")
x<-as.data.frame(summary(l))
zz<-row.names(x[2:61,])
stan_plot(l,pars = c(zz) ,fill_color = "purple", )
stan_plot(l2,pars = c(zz) ,fill_color = "purple", )
########Compare outputs to overall smokiness
datAll<-read_csv("~/SmokeProject/Data/MergedDataComplete.csv")
MeanSlopes<-data.frame(Park=unique(dat$UnitCode),slopeSmokePark=(as.data.frame(summary(l))[2:61,1]),
                       slopeSmokeOverall=(as.data.frame(summary(l2))[2:61,1]),
                       MedSmoke= (summarise(group_by(dat,UnitCode),MedSmoke=median(stdsmoke)))$MedSmoke,
                       MedSmokeAll= (summarise(group_by(datAll,UnitCode),MedSmoke=median(stdsmoke)))$MedSmoke) 

write.csv(MeanSlopes,file="ParameterOutputs.csv")

cor(MeanSlopes$MedSmokeAll,MeanSlopes$slopeSmokeOverall)
ggplot(MeanSlopes,aes(x=MedSmokeAll,y=slopeSmokeOverall))+
  geom_point()+ggtitle("Median Year round smokiness by slope of smoke in high season\nCor = 0.3")

cor(MeanSlopes$MedSmoke,MeanSlopes$slopeSmokeOverall)
ggplot(MeanSlopes,aes(x=MedSmoke,y=slopeSmokeOverall))+
  geom_point()+ggtitle("Median high season smokiness by slope of smoke in high season\nCor = 0.37")
