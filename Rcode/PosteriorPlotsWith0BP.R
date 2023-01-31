library(tidyverse)
library(rstan)
library(ggthemes)
#library(ggridges)
load("~/SmokeProject/ModelObjects/modSmokeSet0_52.rda")#modSmoke2
load("~/SmokeProject/ModelObjects/modSmokeSet12.rda")#modSmoke3
load("~/SmokeProject/ModelObjects/modSmokeSet02.rda") #modSmoke5
load("~/SmokeProject/ModelObjects/SmokeAR_NoBp2.rda") #fit

dat<-read_csv("Data/MergedDataCompleteFINAL2.csv")[,-1]
dat<-dat%>%mutate(NameShrt=str_remove(.$ParkName,c( " NP")))%>%
  mutate(NameShrt=str_remove(.$NameShrt,c( " & PRES")))
dat$UnitCode<-dat$NameShrt
#0.5
z<-rstan::extract(modSmoke2)
sl20.5<-as.data.frame(z$slope1)
names(sl20.5)<-unique(as.factor(dat$UnitCode ))
sl20.5$Model<-rep("bp0.5",nrow(sl20.5))
sl20.5<-gather(sl20.5,key="UnitCode",value="Estimate",Arches:Zion)

#1.0
z<-rstan::extract(modSmoke3)
sl21.0<-as.data.frame(z$slope1)
names(sl21.0)<-unique(as.factor(dat$UnitCode ))
sl21.0$Model<-rep("bp1.0",nrow(sl21.0))
sl21.0<-gather(sl21.0,key="UnitCode",value="Estimate",Arches:Zion)

#0.0
z<-rstan::extract(modSmoke5)
sl20.0<-as.data.frame(z$slope1)
names(sl20.0)<-unique(as.factor(dat$UnitCode ))
sl20.0$Model<-rep("bp0.0",nrow(sl20.0))
sl20.0<-gather(sl20.0,key="UnitCode",value="Estimate",Arches:Zion)

slpz<-rbind(sl20.5,sl21.0,sl20.0)

slpz$Model<-factor(slpz$Model,levels = c("bp0.0","bp1.0","bp0.5"))

cols2<-c("bp0.0" = "#37a7abff", 
         "bp0.5" = "#36679eff",
         "bp1.0" = "#3d3369ff")

mytheme<- theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=14),
                axis.title.y = element_text( size=14),
                axis.text=(element_text(color="black", size=10)),
                legend.title = element_text(colour="black", size=14),
                legend.text = element_text( size = 12))


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
                       unique(dat$UnitCode)[17:32],"2nd","1st"))%>%
  mutate(UnitCodeRev=fct_rev(UnitCode))%>%
  arrange(Model )%>%
  ggplot(., aes(y = mid ,x=UnitCodeRev,ymin=lower,ymax=upper,color=Model))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=UnitCodeRev, ymin=bottom, ymax=top,color=Model), size=0.25,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=.75,alpha=0.8)+xlab("Park")+
  scale_color_manual(name="Breakpoint",labels=c("0.0","0.5","1.0") ,values=cols2)+facet_wrap(~half,scales="free_y" )+ylab("Pre-breakpoint Standardized Effect of Smoke on Visitation")+
  coord_flip(clip = "off")+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())+ 
  guides(color = guide_legend(reverse = F))+
  theme(legend.position = "bottom")


#Start slope 2

z<-rstan::extract(modSmoke2)
sl20.5<-as.data.frame(z$slope2)
names(sl20.5)<-unique(as.factor(dat$UnitCode ))
sl20.5$Model<-rep("bp0.5",nrow(sl20.5))
sl20.5<-gather(sl20.5,key="UnitCode",value="Estimate",Arches:Zion)

#1.0
z<-rstan::extract(modSmoke3)
sl21.0<-as.data.frame(z$slope2)
names(sl21.0)<-unique(as.factor(dat$UnitCode ))
sl21.0$Model<-rep("bp1.0",nrow(sl21.0))
sl21.0<-gather(sl21.0,key="UnitCode",value="Estimate",Arches:Zion)

#0.0
z<-rstan::extract(modSmoke5)
sl20.0<-as.data.frame(z$slope2)
names(sl20.0)<-unique(as.factor(dat$UnitCode ))
sl20.0$Model<-rep("bp0.0",nrow(sl20.0))
sl20.0<-gather(sl20.0,key="UnitCode",value="Estimate",Arches:Zion)

slpz2<-rbind(sl20.5,sl21.0,sl20.0)

slpz2$Model<-factor(slpz2$Model,levels = c("bp0.0","bp1.0","bp0.5"))

#slope2
slp2plot<-slpz2%>%
  filter(UnitCode %in% c("Crater Lake","Redwood","Glacier","Lassen Volcanic","Kings Canyon",
                         "Sequoia","Yellowstone","Yosemite","North Cascades", "Grand Teton","Mount Rainier"))%>%
  mutate(UnitCode=as.factor(UnitCode))%>%
  group_by(UnitCode,Model)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  mutate(half=ifelse(UnitCode %in% 
                       unique(dat$UnitCode)[17:32],"2nd","1st"))%>%
  mutate(UnitCodeRev=fct_rev(UnitCode))%>%
  arrange(Model )%>%
  ggplot(., aes(y = mid ,x=UnitCodeRev,ymin=lower,ymax=upper,color=Model))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=UnitCodeRev, ymin=bottom, ymax=top,color=Model), size=0.25,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=0.75,alpha=0.8)+xlab("Park")+
  scale_color_manual(name="Breakpoint",labels=c("0.0","0.5","1.0") ,values=cols2)+facet_wrap(~half,scales="free_y" )+ylab("Post-breakpoint Standardized Effect of Smoke on Visitation")+
  coord_flip(clip = "off")+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())+ 
  guides(color = guide_legend(reverse = F))+
  theme(legend.position = "bottom")


#Posterior plots of no bp smoke model
#0.0
z<-as.matrix(fit, regex_pars = "smoke")
sl20.0<-as.data.frame(z)
sl20.0<-sl20.0[,1:32]
names(sl20.0)<-unique(as.factor(dat$UnitCode ))
#sl20.0$Model<-rep("bp0.0",nrow(sl20.0))
sl20.0<-gather(sl20.0,key="UnitCode",value="Estimate",Arches:Zion)


#slope2
plot<-sl20.0%>%
  mutate(UnitCode=as.factor(UnitCode))%>%
  group_by(UnitCode)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  mutate(half=ifelse(UnitCode %in% 
                       unique(dat$UnitCode)[17:32],"2nd","1st"))%>%
  mutate(UnitCodeRev=fct_rev(UnitCode))%>%
  ggplot(., aes(y = mid ,x=UnitCodeRev,ymin=lower,ymax=upper))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=UnitCodeRev, ymin=bottom, ymax=top), size=0.25,position = position_dodge(width = 0.5),alpha=0.99) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=0.5,alpha=0.99)+xlab("Park")+
  facet_wrap(~half,scales="free_y" )+ylab("Standardized Overall Effect of Smoke on Visitation")+
  coord_flip(clip = "off")+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())
