library(tidyverse)
library(lme4)
library(rstan)
Data<-read_csv("Data/MergedDataComplete.csv")[,-1]
Data$date<-zoo::as.yearmon(paste0(Data$Month,Data$Year),"%m%Y")
#Data<-Data%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
#Data$VisChange<-(Data$RecreationVisits-Data$ARVis)/Data$ARVis
#Data<-na.omit(Data)

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


#write.csv(dat,file="Data/FullDataWithMinSmokePredictions.csv")
dat<-read.csv("Data/FullDataWithMinSmokePredictions.csv")
dat$date<-zoo::as.yearmon(dat$date)
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
  geom_line(aes(y=PredNoSmoke50CI),color="blue")+geom_smooth(aes(y=RecreationVisits),se=F,method="lm",color="red")+facet_wrap(~UnitCode,scales="free")

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
  summarise(RecVisits=sum(RecreationVisits),PredVisits = sum(PredNoSmoke50CI))%>%
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

#summary stats
sum(dat$RecreationVisits)/sum(dat$PredNoSmoke50CI)
sum(summaryFinancialLoss$IncomeDif)
##

summaryFinancialLoss$IncomeDif<-formattable::currency(summaryFinancialLoss$IncomeDif)
names(summaryFinancialLoss)<-(c("National Park","Income Loss Estimate (1980-2018)", "Percent Income Loss (1980-2018)"))
summaryFinancialLoss$`Percent Income Loss (1980-2018)`<-paste0(summaryFinancialLoss$`Percent Income Loss (1980-2018)`,'%')
formattable::formattable(summaryFinancialLoss)



  
  


# create dumbbell plots
#Visitation
dat%>%filter(UnitCode %in% fitsig$Par)%>%group_by(ParkName)%>%
  summarise(totrec=sum(RecreationVisits),totpred=sum(PredNoSmoke50CI))%>%ungroup()%>%
  mutate(diff=totpred-totrec)%>%
  ggplot(., 
         aes(y = reorder(ParkName,diff),
             x = totrec,
             xend = totpred)) +  
  ggalt::geom_dumbbell(size = 1.2,
                       size_x = 5, 
                       size_xend = 5,
                       colour = "darkgrey", 
                       colour_x = "black", 
                       colour_xend = "#1f78b4",dot_guide = T,dot_guide_size = NULL,dot_guide_colour = "lightgrey") +
  geom_rect( aes(xmin=92000000, xmax=98000000, ymin=-Inf, ymax=Inf), fill="grey") +
  geom_text( aes(label=paste0("-",round((diff/totrec)*100,digits=1), "%"), y=ParkName, x=95000000), fontface=2, size=4)+
  annotate("text", x = 95000000, y = "Glacier NP", label = "Difference",vjust=-1.5,fontface=2)+
  annotate("text", x = 60000000, y = "Yosemite NP", label = "Observed visitation",vjust=-1,hjust=1,fontface=2)+
  annotate("text", x = 73000000, y = "Yosemite NP", label = "Predicted visitation no smoke",vjust=-1,color="#1f78b4",fontface=2)+
  scale_x_continuous( breaks=seq(10000000,90000000,20000000),labels = scales::comma) + 
  labs(title = "Total Lost Visitation",
       subtitle = "1980 to 2018",
       x = "Total Visits",
       y = "")+theme_classic()+mytheme+theme(plot.subtitle = element_text(face = "bold"))


#Money

financialData%>%filter(UnitCode %in% fitsig$Par)%>%group_by(Park)%>%
  summarise(sumincome=sum(incomeObserved),sumincomePred=sum(incomePred))%>%ungroup()%>%
  mutate(diff=sumincomePred-sumincome)%>%
  ggplot(., 
         aes(y = reorder(Park,diff),
             x = sumincome,
             xend = sumincomePred)) +  
  ggalt::geom_dumbbell(size = 1.2,
                       size_x = 5, 
                       size_xend = 5,
                       colour = "darkgrey", 
                       colour_x = "black", 
                       colour_xend = "#b2df8a",dot_guide = T,dot_guide_size = NULL,dot_guide_colour = "lightgrey")+
  geom_rect( aes(xmin=1020000000, xmax=1080000000, ymin=-Inf, ymax=Inf), fill="grey") +
  geom_text( aes(label=paste0("-",round((diff/sumincome)*100,digits=1), "%"), y=Park, x=1050000000), fontface=2, size=4)+
  annotate("text", x = 1050000000, y = "Glacier NP", label = "Difference",vjust=-1.5,fontface=2)+
  scale_x_continuous( labels = scales::dollar_format(prefix = "$")) +
  annotate("text", x = 620000000, y = "Yosemite NP", label = "Observed fee revenue",vjust=-1,hjust=1,fontface=2)+
  annotate("text", x = 750000000, y = "Yosemite NP", label = "Predicted fee revenue no smoke",vjust=-1,color="#b2df8a",fontface=2)+
  labs(title = "Total Lost Revenue",
       subtitle = "1980 to 2018",
       x = "Total Visitor Revenue 1980 to 2018",
       y = "")+theme_classic()+mytheme+theme(plot.subtitle = element_text(face = "bold"))





PNames<-unique(dat$ParkName)

pp<-as.data.frame(posterior_interval(fit))
limits<-row.names(pp[grep("stdsmoke UnitCode",row.names(pp)),])

pars<-plot(fit, regex_pars = "stdsmoke UnitCode",
           prob = 0.5, prob_outer = 0.9)

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))



pars+ggplot2::ggtitle("Posterior medians with 50% and 90% intervals")+
  ggplot2::scale_y_discrete(name ="Park",labels=rev(PNames),limits=rev(limits))+
  mytheme+scale_x_continuous(name="Parameter Value", labels = scales::comma)+
  theme(panel.grid.major.y = element_line(color = "lightgrey", linetype="dotted")) 

                                                                                      

