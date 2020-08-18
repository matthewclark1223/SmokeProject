library(rstan)
library(rstanarm)
library(tidyverse)
Data<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
Data$Date<-zoo::as.yearmon(paste(Data$Year, Data$Month), "%Y %m")
Data<-Data[order(Data$UnitCode, Data$Date),]
Data<-Data%>%filter( SeasType =="High")
fits <- lme4::lmList(RecreationVisits ~ Date | UnitCode, data=Data) 
Data$trendvis<-predict(fits,date=Data$Date)
Data$VisDiff<-as.vector(Data$RecreationVisits-Data$trendvis)
load("~/SmokeProject/ModelObjects/Residuals_bkpoint.rda")

Data%>%filter(UnitCode=="YOSE")%>%
  ggplot(.,aes(x=Date,y=RecreationVisits))+geom_point()+geom_smooth(se=F,method = "lm")+
  geom_point(aes(x=Date,y=trendvis),color="green")+ggtitle("Yosemite NP")

#print estimates
print( zz , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

#table of park numbers
formattable::formattable(data.frame(number=c(1:32),park=as.factor(unique(dat$UnitCode )))) 


slope1s<-vector()
for(i in 1:32){slope1s[i]<-paste0("slope1","[",i,"]")}

slope1plot<-rstan::plot(zz, pars = slope1s,
                        prob = 0.5, prob_outer = 0.9)


slope2s<-vector()
for(i in 1:32){slope2s[i]<-paste0("slope2","[",i,"]")}

slope2plot<-rstan::plot(zz, pars = slope2s,
                        prob = 0.5, prob_outer = 0.9)


bkpts<-vector()
for(i in 1:32){bkpts[i]<-paste0("bkpoint","[",i,"]")}

bkpointplot<-rstan::plot(zz, pars = bkpts,
                         prob = 0.5, prob_outer = 0.9)



Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
x<-as.data.frame(zz)

x<-apply(x,2,quantile,probs=c(0.25,0.5,0.75))

x50<-as.data.frame(apply(x,2,quantile,probs=0.5))
x05<-as.data.frame(apply(x,2,quantile,probs=0.05))
x95<-as.data.frame(apply(x,2,quantile,probs=0.95))

x<-apply(x,2,median)
x<-as.data.frame(x)
x<-data.frame(UnitCode= unique(dat$UnitCode), 
              slope1_0.5=x50[2:33,1],slope1_0.05=x05[2:33,1],slope1_0.75=x95[2:33,1],
              slope2_0.5=x50[34:65,1],slope2_0.05=x05[34:65,1],slope2_0.95=x95[34:65,1],
              ran_int_0.5 = x50[67:98,1],ran_int_0.05 = x05[67:98,1],ran_int_0.95 = x95[67:98,1],
              bkpt_0.5=x50[100:131,1],bkpt_0.05=x05[100:131,1],bkpt_0.95=x95[100:131,1],
              intercept=rep(x50[1,1],32))


bpplot<-function(code,number){
  
  Intercept1<-(x[number,]$intercept-x[number,]$ran_int_0.5)
  Slope1<-x[number,]$slope1_0.5
  Intercept2<-(x[number,]$intercept-x[number,]$ran_int_0.5)+(x[number,]$slope1_0.5*x[number,]$bkpt_0.5-(x[number,]$bkpt_0.5*x[number,]$slope2_0.5))
  Slope2<-x[number,]$slope2_0.5
  smoke1<-(Data%>%filter(UnitCode == code)%>%filter(stdsmoke<=x[number,]$bkpt_0.5))$stdsmoke
  smoke2<-(Data%>%filter(UnitCode == code)%>%filter(stdsmoke>x[number,]$bkpt_0.5))$stdsmoke
  line1<-data.frame(smoke=c(x[number,]$bkpt_0.5,smoke1))
  line1$vis<-Intercept1+line1$smoke*Slope1
  line2<-data.frame(smoke=c(x[number,]$bkpt_0.5,smoke2))
  line2$vis<-Intercept2+line2$smoke*Slope2
  
  reldat<-dat%>%filter(UnitCode == code)
  reldat$bp<-ifelse(reldat$stdsmoke<x[number,]$bkpt_0.05,"below","over")
  ggplot(reldat, aes(x=stdsmoke,y=VisDiff))+
    geom_point(aes(color=bp,fill=bp),shape=21,size=4)+
    scale_colour_manual(values = c("black", "#0570b0"))+
    scale_fill_manual(values = c("black", "#a6bddb"))+
    geom_line(data=line1,aes(x=smoke,y=vis),color="black",size=1.25)+
    geom_line(data=line2,aes(x=smoke,y=vis),color="#0570b0",size=1.25)+
    geom_vline(xintercept = x[number,]$bkpt_0.5,color="grey",linetype="dashed",size=1.25)+theme_classic()+
    coord_cartesian(
      xlim = NULL,
      ylim = c(min(reldat$VisDiff),max(reldat$VisDiff)),
      expand = TRUE)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text( size=18, color="black",face="bold"),
          axis.title.x = element_text( size=18),
          axis.title.y = element_text( size=18),
          axis.text=(element_text(color="black", size=14)),
          legend.title = element_text(colour="black", size=18),
          legend.text = element_text( size = 14),
          legend.position = "none")+
    xlab("Smoke (standardized)")+ylab("Residual Visitation")+ggtitle(code)
  
}

bpplot("YELL",30)
Data%>%filter(UnitCode =="CRLA")
  
perks<-data.frame(code=unique(Data$UnitCode),number=1:32)
for(i in 1:32){
  assign(paste0("bp",i),bpplot(perks[i,]$code,perks[i,]$number))
}
gridExtra::grid.arrange(bp1,bp2,bp3,bp4,bp5,bp6,bp7,bp8,nrow=4)
gridExtra::grid.arrange(bp9,bp10,bp11,bp12,bp13,bp14,bp15,bp16,nrow=4)
gridExtra::grid.arrange(bp17,bp18,bp19,bp20,bp21,bp22,bp23,bp24,nrow=4)
gridExtra::grid.arrange(bp25,bp26,bp27,bp28,bp29,bp30,bp31,bp32,nrow=4)




