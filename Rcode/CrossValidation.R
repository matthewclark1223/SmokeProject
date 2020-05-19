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
train1<-dat%>%filter(!Year %in% c("2018","2017","2016","2015","2014"))
test1<-dat%>%filter(Year %in% c("2018","2017","2016","2015","2014"))

library(rstanarm)
options(mc.cores=parallel::detectCores())

#fitTrain<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train1,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain,file="CVResidualMod.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod.rds")

preds1<-apply(posterior_predict(fitTrain,test1),2,median)
reals1<-test1$VisDiff


## 2013:2009

train2<-dat%>%filter(!Year %in% c("2013","2012","2011","2010","2009"))
test2<-dat%>%filter(Year %in% c("2013","2012","2011","2010","2009"))
#fitTrain2<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train2,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain2,file="CVResidualMod2.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod2.rds")

preds2<-apply(posterior_predict(fitTrain2,test2),2,median)
reals2<-test2$VisDiff

# 2008:2004
train3<-dat%>%filter(!Year %in% c("2008","2007","2006","2005","2004"))
test3<-dat%>%filter(Year %in% c("2008","2007","2006","2005","2004"))
#fitTrain3<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train3,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain3,file="CVResidualMod3.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod3.rds")

preds3<-apply(posterior_predict(fitTrain3,test3),2,median)
reals3<-test3$VisDiff

# 2003:1999
train4<-dat%>%filter(!Year %in% c("2003","2002","2001","2000","1999"))
test4<-dat%>%filter(Year %in% c("2003","2002","2001","2000","1999"))
#fitTrain4<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train4,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain4,file="CVResidualMod4.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod4.rds")

preds4<-apply(posterior_predict(fitTrain4,test4),2,median)
reals4<-test4$VisDiff

# 1998:1994
train5<-dat%>%filter(!Year %in% c("1998","1997","1996","1995","1994"))
test5<-dat%>%filter(Year %in% c("1998","1997","1996","1995","1994"))
#fitTrain5<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train5,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain5,file="CVResidualMod5.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod5.rds")

preds5<-apply(posterior_predict(fitTrain5,test5),2,median)
reals5<-test5$VisDiff

# 1993:1989
train6<-dat%>%filter(!Year %in% c("1993","1992","1991","1990","1989"))
test6<-dat%>%filter(Year %in% c("1993","1992","1991","1990","1989"))
#fitTrain6<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train6,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain6,file="CVResidualMod6.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod6.rds")

preds6<-apply(posterior_predict(fitTrain6,test6),2,median)
reals6<-test6$VisDiff

# 1988:1980
train7<-dat%>%filter(!Year %in% c("1988","1987","1986","1985","1984","1983","1982","1981","1980"))
test7<-dat%>%filter(Year %in% c("1988","1987","1986","1985","1984","1983","1982","1981","1980"))
#fitTrain7<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=train7,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fitTrain7,file="CVResidualMod7.rds")
load("~/SmokeProject/ModelObjects/CVResidualMod7.rds")

preds7<-apply(posterior_predict(fitTrain7,test7),2,median)
reals7<-test7$VisDiff

#combine them all
preds<-c(preds1,preds2,preds3,preds4,preds5,preds6,preds7)
reals<-c(reals1,reals2,reals3,reals4,reals5,reals6,reals7)


cor(as.vector(preds1),reals1)
cor(as.vector(preds2),reals2)
cor(as.vector(preds3),reals3)
cor(as.vector(preds4),reals4)
cor(as.vector(preds5),reals5)
cor(as.vector(preds6),reals6)
cor(as.vector(preds7),reals7)

x<-data.frame(Model=c(1:7), 
              "TimePeriod"=c("1980 - 1988",
                                         "1989 - 1993",
                                         "1994 - 1998",
                                         "1999 - 2003",
                           "2004 - 2008",
                           "2009 - 2013",
                           "2014 - 2018"), 
              Correlation=c(cor(as.vector(preds1),reals1),
                                                   cor(as.vector(preds2),reals2),
                                                   cor(as.vector(preds3),reals3),
                                                   cor(as.vector(preds4),reals4),
                                                   cor(as.vector(preds5),reals5),
                                                   cor(as.vector(preds6),reals6),
                                                   cor(as.vector(preds7),reals7)))

x$r2<-round(x$Correlation^2,digits=2)
x$Correlation<-round(x$Correlation,digits=2)
names(x)<-c("Model","Time Period", "Correlation", "R^2")
formattable::formattable(x)



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









