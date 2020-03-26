dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
x<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==max(mv))%>%mutate(cc=paste0(UnitCode,Season))

x2<-dat%>%group_by(UnitCode,Season)%>%
  summarise(mv=mean(RecreationVisits))%>%ungroup()%>%
  group_by(UnitCode)%>%filter(mv==min(mv))%>%mutate(cc=paste0(UnitCode,Season))

dat$SeasType<-ifelse(dat$CatColS %in% x$cc,
                     "High",ifelse(dat$CatColS %in% x2$cc,"Low","Shoulder"))

stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}

dat<-dat%>%group_by(as.character(Year))%>%mutate(stdsmoketime=stdize(Smoke*10e8))


dat<-dat%>%filter(SeasType =="High")

data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoketime,
  pcode = as.numeric(as.factor(dat$UnitCode )))

options(mc.cores=3)

l3<- stan( file="SmokeNegBinom.stan" , data=data_list,chains=3 ,warmup = 1000 ,iter = 10000)

print( l3 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(l3, file="stdsmokeTimeNbinom.rda")
stan_plot(l3,pars = c(zz) ,fill_color = "purple", )

mean(as.data.frame(summary(l3))[2:61,1])



