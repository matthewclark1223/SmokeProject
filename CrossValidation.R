library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")

#filter by high season
Data<-Data%>%filter( SeasType =="High")

#filter by smoke affected regions
Data<-Data%>%
  filter(Region %in% c("Intermountain","Pacific West"))


#get vis trends

fits <- lmList(RecreationVisits ~ date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$date)
Data$VisDiff<-Data$RecreationVisits-Data$trendvis
dat<-Data

#pull out last 5 years
train<-dat%>%filter(!Year %in% c("2018","2017","2016","2015","2014"))
test<-dat%>%filter(Year %in% c("2018","2017","2016","2015","2014"))

library(rstanarm)
options(mc.cores=parallel::detectCores())

#fitTrain<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
save(fitTrain,file="CVResidualMod.rds")
preds<-apply(posterior_predict(fitTrain,test),2,median)
reals<-test$VisDiff

d<-as.data.frame(cbind(as.vector(preds),reals))
names(d)<-c("preds","reals")
mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))
d%>%
  ggplot(.,aes(x=reals,y=preds))+
  geom_jitter(size=2, alpha=0.3)+
  geom_smooth(method="lm", se=FALSE, colour="red")+
  geom_text(aes(x=-1e+05,y=1e+05),size=10,label=paste("cor =",round(cor(d$preds,d$reals),digits=2)))+
  ggtitle("CV Results")+theme_classic()+mytheme+
  theme(plot.title = element_text(hjust = 0.5))









