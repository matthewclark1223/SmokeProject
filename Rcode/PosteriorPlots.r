library(tidyverse)
library(rstan)
library(ggthemes)
library(ggridges)
load("~/SmokeProject/ModelObjects/modSmokeSet0_5.rda")#modSmoke2
load("~/SmokeProject/ModelObjects/modSmokeSet1.rda")#modSmoke3
load("~/SmokeProject/ModelObjects/modSmokeSet1_5.rda")#modSmoke4

#0.5
z<-extract(modSmoke2)
sl20.5<-as.data.frame(z$slope1)
names(sl20.5)<-unique(as.factor(dat$UnitCode ))
sl20.5$Model<-rep("bp0.5",nrow(sl20.5))
sl20.5<-gather(sl20.5,key="UnitCode",value="Estimate",ARCH:ZION)

#1.0
z<-extract(modSmoke3)
sl21.0<-as.data.frame(z$slope1)
names(sl21.0)<-unique(as.factor(dat$UnitCode ))
sl21.0$Model<-rep("bp1.0",nrow(sl21.0))
sl21.0<-gather(sl21.0,key="UnitCode",value="Estimate",ARCH:ZION)

#1.5
z<-extract(modSmoke4)
sl21.5<-as.data.frame(z$slope1)
names(sl21.5)<-unique(as.factor(dat$UnitCode ))
sl21.5$Model<-rep("bp1.5",nrow(sl21.5))
sl21.5<-gather(sl21.5,key="UnitCode",value="Estimate",ARCH:ZION)

slpz<-rbind(sl20.5,sl21.0,sl21.5)

slpz$Model<-factor(slpz$Model,levels = c("bp1.5","bp1.0","bp0.5"))

cols2<-c("bp0.5" = "#37a7abff", 
         "bp1.0" = "#36679eff",
         "bp1.5" = "#3d3369ff")

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))

#slope 1
slp1plot<-slpz%>%
  mutate(UnitCode=as.factor(UnitCode))%>%
  group_by(UnitCode,Model)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  mutate(half=ifelse(UnitCode %in% 
                       c("JOTR","KICA","LAVO","MEVE","MORA","NOCA","OLYM","PEFO","PINN","REDW","ROMO","SAGU","SEQU","YELL","YOSE","ZION"),"2nd","1st"))%>%
  mutate(UnitCodeRev=fct_rev(UnitCode))%>%
  ggplot(., aes(y = mid ,x=UnitCodeRev,ymin=lower,ymax=upper,color=Model))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=UnitCodeRev, ymin=bottom, ymax=top,color=Model), size=0.25,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=1,alpha=0.8)+xlab("NPS Unit Code")+
  scale_color_manual(name="Breakpoint",labels=c("0.5","1.0","1.5") ,values=cols2)+facet_wrap(~half,scales="free")+ylab("Standardized Effect Size")+
  coord_flip()+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                             strip.text.x = element_blank())+
  theme(legend.position = "bottom") 




z<-extract(modSmoke2)
sl20.5<-as.data.frame(z$slope2)
names(sl20.5)<-unique(as.factor(dat$UnitCode ))
sl20.5$Model<-rep("bp0.5",nrow(sl20.5))
sl20.5<-gather(sl20.5,key="UnitCode",value="Estimate",ARCH:ZION)

#1.0
z<-extract(modSmoke3)
sl21.0<-as.data.frame(z$slope2)
names(sl21.0)<-unique(as.factor(dat$UnitCode ))
sl21.0$Model<-rep("bp1.0",nrow(sl21.0))
sl21.0<-gather(sl21.0,key="UnitCode",value="Estimate",ARCH:ZION)

#1.5
z<-extract(modSmoke4)
sl21.5<-as.data.frame(z$slope2)
names(sl21.5)<-unique(as.factor(dat$UnitCode ))
sl21.5$Model<-rep("bp1.5",nrow(sl21.5))
sl21.5<-gather(sl21.5,key="UnitCode",value="Estimate",ARCH:ZION)

slpz2<-rbind(sl20.5,sl21.0,sl21.5)

slpz2$Model<-factor(slpz2$Model,levels = c("bp1.5","bp1.0","bp0.5"))

#slope2
slp2plot<-slpz2%>%
  mutate(UnitCode=as.factor(UnitCode))%>%
  group_by(UnitCode,Model)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  mutate(half=ifelse(UnitCode %in% 
                       c("JOTR","KICA","LAVO","MEVE","MORA","NOCA","OLYM","PEFO","PINN","REDW","ROMO","SAGU","SEQU","YELL","YOSE","ZION"),"2nd","1st"))%>%
  mutate(UnitCodeRev=fct_rev(UnitCode))%>%
  ggplot(., aes(y = mid ,x=UnitCodeRev,ymin=lower,ymax=upper,color=Model))+ggtitle("")+
  geom_linerange( mapping=aes(x=UnitCodeRev, ymin=bottom, ymax=top,color=Model), size=0.25,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=1,alpha=0.8)+xlab("NPS Unit Code")+
  scale_color_manual(name="Breakpoint",labels=c("0.5","1.0","1.5") ,values=cols2)+facet_wrap(~half,scales="free")+ylab("Standardized Effect Size")+
  coord_flip()+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())+
   theme(legend.position="bottom")

combplt<-gridExtra::grid.arrange(slp1plot,slp2plot)
