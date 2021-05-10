library(tidyverse)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
#filter to only high season data
#dat<-dat%>%filter(SeasType =="High")

#stdize function as per Gelman reccomendation
stdize<-function(x){
  (x-mean(x))/(2*sd(x))}
dat$Date<-zoo::as.yearmon(paste(dat$Year, dat$Month), "%Y %m")
dat$stdsmokemed<-stdize(dat$medSmoke)

cols <- c("0.5" = "#c33e56ff", 
          "1.0" = "#832b5aff",
          "1.5" = "#402141ff")

cols2<-c("0.5" = "#37a7abff", 
         "1.0" = "#36679eff",
         "1.5" = "#3d3369ff")

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))






#Plot of smoke change over time
ggplot(data=dat,aes(x=Date,y=medSmoke,by=UnitCode))+
  geom_smooth(method="gam",se=F,color="black")+
  geom_text(x=2012,y=6e-09,label="Olympic NP",color="black",fontface="bold",size=6)+
  geom_text(x=2017,y=5e-10,label="Kings Canyon NP",color="black",fontface="bold",size=6)+
  annotate("segment", x = 2012, xend = 2014, y = 5.8e-09, yend = 5.3e-09, colour = "black", size=1.5, arrow=arrow())+
  annotate("segment", x = 2017, xend = 2018, y = 6e-10, yend = 9e-10, colour = "black", size=1.5, arrow=arrow())+
  theme_classic()+mytheme+ylab("Median Smoke Value")
  

#show std scale
fit<-lm(formula = stdsmokemed ~ medSmoke, data = dat)

stdd<-function(x){
  predict.lm(fit,newdata=data.frame(medSmoke=x))
}

HS<-dat%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode!="LAVO")%>%filter(UnitCode!="KICA")
HSL<-dat%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode=="LAVO")
HSK<-dat%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode=="KICA")

smokePlt<-dat%>%filter(SeasType=="High")%>%
ggplot(.,aes(x=Date,y=medSmoke,by=UnitCode))+
  geom_point(alpha=0.4,size=3)+
    geom_text(data=HS,aes(x=Date-2.1,y=medSmoke,label=paste(ParkName,paste0(Month,"/",Year))),size=3,fontface="bold")+
    geom_text(data=HSK,aes(x=Date-2.7,y=medSmoke,label=paste(ParkName,paste0(Month,"/",Year))),size=3,fontface="bold")+
    geom_text(data=HSL,aes(x=Date-2.9,y=medSmoke,label=paste(ParkName,paste0(Month,"/",Year))),size=3,fontface="bold")+
    theme_classic()+mytheme+ylab("Median Smoke Value")+
scale_y_continuous(sec.axis =sec_axis(trans = stdd ,name="Standardized"))

#make bp example data
ind<-seq(-1,3,0.01)
slp<- -0.1
bp<-0.5
dep0.5<-ifelse(ind<=bp,-0.1,
            (-0.1-bp*slp)+slp*ind) 

bp<-1
dep1<-ifelse(ind<=bp,0,
               (0-bp*slp)+slp*ind)

bp<-1.5
dep1.5<-ifelse(ind<=bp,0.1,
               (0.1-bp*slp)+slp*ind)

bpdat<-data.frame(ind,dep0.5,dep1,dep1.5)

bpdat<-gather(bpdat,key="bp",value="dep",dep0.5,dep1,dep1.5)
  bpdat$bp<-recode(bpdat$bp, dep0.5 = "0.5",dep1 = "1.0",dep1.5 = "1.5")

  #reds version
#ggplot(data=bpdat,aes(x=ind,y=dep,color=bp))+geom_line(size=3)+
 # scale_color_manual(values=cols)+
#  scale_y_continuous(breaks=c(0),labels="Intercept")+
 # scale_x_continuous(breaks=c(0,0.5, 1.0, 1.5))+
#  labs(color = "Breakpoint")+xlab("Smoke (standardized)")+ylab("Visitation")+
#  geom_segment(x = 0.5,xend=0.5,y=-5.25,yend=-0.25,color="#c33e56ff",linetype="dashed",size=1)+
#  geom_segment(x = 1,xend=1,y=-5.25,yend=0,color="#832b5aff",linetype="dashed",size=1)+
 # geom_segment(x = 1.5,xend=1.5,y=-5.25,yend=0.25,color="#402141ff",linetype="dashed",size=1)+
  #theme_classic()+mytheme


  bpPlot<-ggplot(data=bpdat,aes(x=ind,y=dep,color=bp))+geom_line(size=3)+
    scale_color_manual(values=cols2)+
    scale_y_continuous(breaks=c(0),labels="Intercept", limits = c(-0.5,0.3))+
    scale_x_continuous(breaks=c(0,0.5, 1.0, 1.5))+
    geom_text(x=0,y=0.2,label="Slope 1",color="black",fontface="bold",size=6)+
    geom_text(x=2.5,y=0.1,label="Slope 2",color="black",fontface="bold",size=6)+
    annotate("segment", x = 0, xend = 0, y = 0.175, yend = 0.12, colour = "black", size=1.5, arrow=arrow())+
    annotate("segment", x = 2.5, xend = 2.4, y = 0.075, yend = 0.02, colour = "black", size=1.5, arrow=arrow())+
    labs(color = "Breakpoint")+xlab("Smoke (standardized)")+ylab("Visitation")+
    geom_segment(x = 0.5,xend=0.5,y=-5.25,yend=-0.1,color="#37a7abff",linetype="dashed",size=1)+
    geom_segment(x = 1,xend=1,y=-5.25,yend=0,color="#36679eff",linetype="dashed",size=1)+
    geom_segment(x = 1.5,xend=1.5,y=-5.25,yend=0.1,color="#3d3369ff",linetype="dashed",size=1)+
    theme_classic()+mytheme+theme(legend.position = "none") 
  
  
  

  
  