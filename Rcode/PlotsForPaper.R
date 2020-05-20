library(tidyverse)
allData<-read_csv("Data/MergedDataComplete.csv")[,-1]
predDat<-read_csv("Data/FullDataWithMinSmokePredictions.csv")[,-1]
predDat$date<-zoo::as.yearmon(predDat$date)
allData$date<-zoo::as.yearmon(paste0(allData$Month,allData$Year),"%m%Y")
allData<-allData%>% filter(SeasType =="High")
 #all sig parks
predDat.interp <- predDat %>% 
   split(.$UnitCode) %>% 
   map_df(~data.frame(UnitCode = approx(.x$date, .x$PredNoSmoke50CI, n = 80), 
                      nat = approx(.x$date, .x$RecreationVisits, n = 80), 
                      UnitCode = .x$UnitCode[1]))

cols <- c("Observed greater" = "#b2df8a", "Predicted greater" = "#1f78b4")
ggplot(predDat.interp, aes(nat.x, nat.y)) +  facet_wrap(~UnitCode,scales="free") +
  geom_ribbon(aes(ymin = nat.y, ymax = pmin(UnitCode.y, nat.y), fill = "Observed greater"),alpha=0.9) +
  geom_ribbon(aes(ymin = UnitCode.y, ymax = pmin(UnitCode.y, nat.y), fill = "Predicted greater"),alpha=0.9) +
  geom_line(aes(linetype = "Observed Visitation"),size=1.15) +
  geom_line(aes(nat.x, UnitCode.y, linetype = "Predicted Visitation No Smoke"),size=1.15) +
  scale_fill_manual(values=cols)+theme_classic()+xlab("Time")+ylab("Recreational Visits")

ggplot(predDat.interp, aes(nat.x, nat.y)) +  facet_wrap(~UnitCode,scales="free") +
  geom_ribbon(aes(ymin = nat.y, ymax = pmin(UnitCode.y, nat.y), fill = "Observed greater"),alpha=0.5) +
  geom_ribbon(aes(ymin = UnitCode.y, ymax = pmin(UnitCode.y, nat.y), fill = "Predicted greater"),alpha=0.5) +
  geom_line(color="#b2df8a",size=1.15) +
  geom_line(aes(nat.x, UnitCode.y), color="#1f78b4",size=1.15) +
  scale_fill_manual(values=cols)+theme_classic()+xlab("Time")+ylab("Recreational Visits")


#individual
cols <- c("Observed greater" = "#b2df8a", "Predicted greater" = "#1f78b4")

p1<-predDat.interp%>%filter(UnitCode == "GLAC")%>%
ggplot(., aes(nat.x, nat.y)) +  
  geom_ribbon(aes(ymin = nat.y, ymax = pmin(UnitCode.y, nat.y), fill = "Observed greater"),alpha=0.9) +
  geom_ribbon(aes(ymin = UnitCode.y, ymax = pmin(UnitCode.y, nat.y), fill = "Predicted greater"),alpha=0.9) +
  geom_line(aes(linetype = "Observed Visitation"),size=1.15) +
  geom_line(aes(nat.x, UnitCode.y, linetype = "Predicted Visitation No Smoke"),size=1.15) +
  scale_fill_manual(values=cols)+theme_classic()+xlab("Time")+ylab("Recreational Visits")


p1<-predDat.interp%>%filter(UnitCode == "GLAC")%>%
  ggplot(., aes(nat.x, nat.y)) +  
  geom_ribbon(aes(ymin = nat.y, ymax = pmin(UnitCode.y, nat.y), fill = "Observed greater"),alpha=0.5) +
  geom_ribbon(aes(ymin = UnitCode.y, ymax = pmin(UnitCode.y, nat.y), fill = "Predicted greater"),alpha=0.5) +
  geom_line(color="#b2df8a",size=1.15) +
  geom_line(aes(nat.x, UnitCode.y), color="#1f78b4",size=1.15) +
  scale_fill_manual(values=cols)+theme_classic()+xlab("Time")+ylab("Recreational Visits")


#smoke
glacdat=filter(allData,UnitCode == "GLAC")
fit<-lm(stdsmoke~date,data=glacdat)
glacdat$predsmoke<-predict(fit,date=glacdat$date)


p2<-ggplot(glacdat,aes(x=date,y=stdsmoke))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
    geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    #guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
   
gridExtra::grid.arrange(p1,p2,nrow=2)
                            
                            
library(lme4)                            
fits <- lmList(stdsmoke ~ date | UnitCode, data=allData) 
allData$predsmoke<-predict(fits,date=allData$date)
allData<-allData%>%filter(UnitCode %in% unique(predDat$UnitCode))


  p1<-allData%>%filter(UnitCode == "BISC")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  
  p2<-allData%>%filter(UnitCode == "GLAC")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  p3<-allData%>%filter(UnitCode == "HAVO")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  p4<-allData%>%filter(UnitCode == "KATM")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  p5<-allData%>%filter(UnitCode == "KOVA")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  p6<-allData%>%filter(UnitCode == "LACL")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  p7<-allData%>%filter(UnitCode == "SAGU")%>%
    ggplot(.,aes(x=date,y=stdsmoke,by=UnitCode))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
  geom_segment(aes(xend = date, yend = predsmoke,alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=1.5) +
    geom_point(aes(alpha = abs(stdsmoke-predsmoke),color =(stdsmoke-predsmoke)),size=5)+
    scale_color_gradient2(low ="#08306b" , mid = "white", high = "#7f0000",name= "Difference from trend",breaks=c(0.2,-0.1),
                          labels=c("Above","Below")) + 
    #guides(color = FALSE) +
    guides(alpha = FALSE) + 
    geom_point(aes(y = predsmoke), shape = 1,color="lightgrey")+theme_classic()
  
  gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=3)

  
  ##HAVO Good Example
  cols <- c("Observed visitation" = "#33a02c", "Predicted visitation no smoke" = "#1f78b4")
  
  mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  plot.title = element_text( size=18, color="black",face="bold"),
                  axis.title.x = element_text( size=18),
                  axis.title.y = element_text( size=18),
                  axis.text=(element_text(color="black", size=14)),
                  legend.title = element_text(colour="black", size=18),
                  legend.text = element_text( size = 14))
  
  p1<-predDat.interp%>%filter(UnitCode == "HAVO")%>%
    ggplot(., aes(nat.x, nat.y)) +  
    geom_ribbon(aes(ymin = nat.y, ymax = pmin(UnitCode.y, nat.y), fill = "Observed visitation"),alpha=0.3) +
    geom_ribbon(aes(ymin = UnitCode.y, ymax = pmin(UnitCode.y, nat.y), fill = "Predicted visitation no smoke"),alpha=0.3) +
    geom_line(color="#33a02c",size=1.15) +
    geom_line(aes(nat.x, UnitCode.y), color="#1f78b4",size=1.15) +
    scale_fill_manual(values=cols,name=NULL)+theme_classic()+xlab("Time")+ylab("Recreational Visits")+
    ggtitle("Smoke Effects on Visitation in Hawaii Volcanos National Park")+mytheme
  
  
  p2<-allData%>%filter(UnitCode == "HAVO")%>%group_by(Year)%>%
    summarise(meansmoke=mean(stdsmoke),meanpredsmoke=mean(predsmoke))%>%
    ggplot(.,aes(x=Year,y=meansmoke))+geom_smooth(se=F,method="lm", color = "lightgrey",size=2)+
    geom_point(aes(y = meanpredsmoke), shape = 1,color="lightgrey")+
    geom_segment(aes(xend = Year, yend = meanpredsmoke,color =(meansmoke-meanpredsmoke)),size=1.5) +
    geom_point(aes(color =(meansmoke-meanpredsmoke)),size=5)+
    scale_color_gradient2(low ="#2b83ba" , mid = "#ffffbf", high = "#d7191c",name= "Difference from trend",breaks=c(0.050,-0.035),
                          labels=c("Above","Below")
    ) + 
    #guides(color = FALSE) +
    guides(alpha = FALSE) + 
    theme_classic()+xlab("Month")+
    ylab("Smoke (standardized)")+mytheme
   
  gridExtra::grid.arrange(p1,p2,nrow=2)
  
  
  
  
  
  
  
  
  ##Lolipops  ...
  predDat%>%group_by(UnitCode)%>%
    summarise(totrec=sum(RecreationVisits),totpred=sum(No_Smoke_Mean_est))%>%
  ggplot(.) +
    geom_segment( aes(x=UnitCode, xend=UnitCode, y=totrec, yend=totpred), color="grey") +
    geom_point( aes(x=UnitCode, y=totrec), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
    geom_point( aes(x=UnitCode, y=totpred), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
    coord_flip()+scale_y_log10()+
    theme_classic() +
    theme(
      legend.position = "none",
    ) +
    xlab("") +
    ylab("Value of Y")
  
  
  
  
  
  
  ## 
  
  p1<-predDat%>%filter(UnitCode == "OLYM")%>%
    ggplot(.,aes(x=date))+
    geom_ribbon(aes(ymin=PredNoSmoke5CI, ymax=PredNoSmoke95CI),fill="#1f78b4",alpha=0.2)+
    geom_ribbon(aes(ymin=PredNoSmoke25CI, ymax=PredNoSmoke75CI),fill="#1f78b4",alpha=0.5)+
    geom_line(aes(y=RecreationVisits,color="grey"),size=1.25)+
    geom_point(aes(y=RecreationVisits,color="black"),size=2)+
    geom_line(aes(y=PredNoSmoke50CI,color="#1f78b4"),linetype="longdash",size=1.25)+theme_classic()+
    mytheme + scale_y_continuous(name="Recreational Visits", labels = scales::comma)+
    scale_x_continuous(name="Date",breaks=c(1980,1990,2000,2010,2018))+
    scale_color_identity(name = "",
                         breaks = c("black", "#1f78b4"),
                         labels = c("Observed", "Median Estimate Minimum Smoke"),
                         guide = "legend")+ggtitle("Glacier National Park")+theme(legend.position="top")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line.x = element_blank())


  
  
p2<-predDat%>%filter(UnitCode == "OLYM")%>%group_by(Year)%>%
    summarise(meansmoke=mean(Smoke))%>%
    ggplot(.,aes(x=Year,y=meansmoke))+
    geom_segment(aes(xend = Year, yend = 1e-10,color =(meansmoke)),size=1.5) +
    geom_point(aes(color =(meansmoke)),size=5)+
    scale_color_gradient2(low ="#2b83ba" , mid = "#ffffbf", high = "#d7191c",name= "Difference from trend",breaks=c(0.050,-0.035),
                          labels=c("Above","Below")
    ) + 
    #guides(color = FALSE) +
    guides(alpha = FALSE) + 
    theme_classic()+  scale_x_continuous(name="Date",breaks=c(1980,1990,2000,2010,2018))+
   scale_y_continuous(name="Smoke (PM 2.5)",expand = c(0, 0), limits = c(1e-10, 9e-10))+mytheme
  

gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
