library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
Data<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
Data$VisChange<-(Data$RecreationVisits-Data$ARVis)/Data$ARVis
Data<-na.omit(Data)

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

library(rstanarm)
options(mc.cores=parallel::detectCores())
#fit<-stan_lmer(VisDiff~stdsmoke|UnitCode,data=dat,adapt_delta = 0.99,chains=8,warmup=2000,iter=8000)
#save(fit,file="residuals.rda")
load("~/SmokeProject/ModelObjects/residuals.rda")

bayesplot::color_scheme_set("viridisC")
(trace <- plot(fit, "trace", pars = c("b[stdsmoke UnitCode:ZION]","b[stdsmoke UnitCode:YELL]","b[stdsmoke UnitCode:SEQU]","(Intercept)")))


#look at the predictions

dat<-dat%>%group_by(UnitCode)%>%mutate(MinSmoke=min(stdsmoke))

minsmokedata<-data.frame(UnitCode=dat$UnitCode,stdsmoke=dat$MinSmoke)


dat$PredNoSmoke50CI<-apply(posterior_predict(fit,minsmokedata),2,median)
dat$PredNoSmoke95CI<-apply(posterior_predict(fit,minsmokedata),2,function(x){quantile(x,0.95)})
dat$PredNoSmoke5CI<-apply(posterior_predict(fit,minsmokedata),2,function(x){quantile(x,0.05)})
dat$PredNoSmoke75CI<-apply(posterior_predict(fit,minsmokedata),2,function(x){quantile(x,0.75)})
dat$PredNoSmoke25CI<-apply(posterior_predict(fit,minsmokedata),2,function(x){quantile(x,0.25)})


dat$PredNoSmoke50CI<-dat$trendvis+dat$PredNoSmoke50CI
dat$PredNoSmoke95CI<-dat$trendvis+dat$PredNoSmoke95CI
dat$PredNoSmoke5CI<-dat$trendvis+dat$PredNoSmoke5CI
dat$PredNoSmoke75CI<-dat$trendvis+dat$PredNoSmoke75CI
dat$PredNoSmoke25CI<-dat$trendvis+dat$PredNoSmoke25CI


write.csv(dat,file="Data/FullDataWithMinSmokePredictions.csv")

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))

dat%>%filter(UnitCode =="ROMO")%>%
ggplot(.,aes(x=date))+
  geom_ribbon(aes(ymin=PredNoSmoke5CI, ymax=PredNoSmoke95CI),fill="#1f78b4",alpha=0.2)+
  geom_ribbon(aes(ymin=PredNoSmoke25CI, ymax=PredNoSmoke75CI),fill="#1f78b4",alpha=0.5)+
  geom_line(aes(y=RecreationVisits,color="black"),size=2)+
  geom_line(aes(y=PredNoSmoke50CI,color="#1f78b4"),linetype="longdash",size=2)+theme_classic()+
  mytheme + scale_y_continuous(name="Recreational Visits", labels = scales::comma)+xlab("Date")+
  scale_color_identity(name = "",
                       breaks = c("black", "#1f78b4"),
                       labels = c("Observed", "Median Estimate"),
                       guide = "legend")+ggtitle("Rocky Mountain National Park")



  ggplot(dat,aes(x=date))+
  geom_line(aes(y=RecreationVisits),color="green")+
  geom_line(aes(y=PredNoSmoke),color="blue")+geom_smooth(aes(y=RecreationVisits),se=F,method="lm",color="red")+facet_wrap(~UnitCode,scales="free")

  p <- plot(fit, regex_pars = "stdsmoke UnitCode",
            prob = 0.5, prob_outer = 0.9)


  
head(dat)  
fitint<-as.data.frame(posterior_interval(fit))
names(fitint)<-c("CI5","CI95")
fitint$Par<-row.names(fitint)
fitsig<-fitint[grep("stdsmoke UnitCode",row.names(fitint)),]%>% 
  filter(CI5<0 & CI95<0) 

fitsig$Par<-substr(fitsig$Par,21,24)

#predicted vis fees 
FEdat<-read.csv("~/SmokeProject/Data/ParkFees.csv")

#get the visitation observed and predicted yearly totals (same scale as fee data)
VEstDat<-dat%>%group_by(UnitCode,Year)%>%
  summarise(RecVisits=sum(RecreationVisits),PredVisits = sum(PredNoSmoke))%>%
  filter(UnitCode %in% fitsig$Par)

#only include significant parks and years we predicted for
FEdat<-FEdat%>%filter(UnitCode%in%fitsig$Par)

#merge the fee and visitation data
financialData<-merge(FEdat, VEstDat, by = c("UnitCode","Year"))

#multiply the fees and visitation (predicted and observed)
financialData<-financialData%>%mutate(incomeObserved= round(SingleVisitorFee*RecVisits,digits = 2),incomePred= round(SingleVisitorFee*PredVisits,digits=2))

#get the dollar estimates for smoke loss for each park)
summaryFinancialLoss<-financialData %>% group_by(UnitCode)%>%summarise(totrec=sum(incomeObserved),totpred=sum(incomePred))%>%
  mutate(IncomeDif=totrec-totpred,IncomePrecDiff=round((1-(totrec/totpred))*100,digits=2))


summaryFinancialLoss$ParkName<-unique(dat[dat$UnitCode %in% summaryFinancialLoss$UnitCode,]$ParkName)

summaryFinancialLoss<-summaryFinancialLoss[,c(6,4,5)]
summaryFinancialLoss$IncomeDif<-currency(summaryFinancialLoss$IncomeDif)
names(summaryFinancialLoss)<-(c("National Park","Income Loss Estimate (1980-2018)", "Percent Income Loss (1980-2018)"))
summaryFinancialLoss$`Percent Income Loss (1980-2018)`<-paste0(summaryFinancialLoss$`Percent Income Loss (1980-2018)`,'%')
formattable::formattable(summaryFinancialLoss)



sum(dat$RecreationVisits)/sum(dat$PredNoSmoke50CI)
sum(summaryFinancialLoss$IncomeDif)  
  
