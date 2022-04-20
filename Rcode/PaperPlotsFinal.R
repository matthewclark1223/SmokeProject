library(tidyverse)
dat<-read_csv("~/SmokeProject/Data/MergedDataCompleteFINAL.csv")[,-1]
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

HS<-dat%>%mutate(ParkName= gsub(" NP","",dat$ParkName))%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode!="LAVO")%>%filter(UnitCode!="KICA")
HSL<-dat%>%mutate(ParkName= gsub(" NP","",dat$ParkName))%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode=="LAVO")
HSK<-dat%>%mutate(ParkName= gsub(" NP","",dat$ParkName))%>%filter(SeasType=="High")%>%filter(stdsmokemed>5)%>%filter(UnitCode=="KICA")


smokePlt<-dat%>%
  filter(SeasType=="High")%>%
ggplot(.,aes(x=Date,y=medSmoke,by=UnitCode))+
  geom_point(alpha=0.4,size=2.5)+
    geom_text(data=HS,aes(x=Date-2.4,y=medSmoke+1e-9,label=paste(ParkName,paste0(Month,"/",Year))),size=2.8,fontface="bold")+
    geom_text(data=HSK,aes(x=Date-3,y=medSmoke,label=paste(ParkName,paste0(Month,"/",Year))),size=2.8,fontface="bold")+
    geom_text(data=HSL,aes(x=Date-3.2,y=medSmoke,label=paste(ParkName,paste0(Month,"/",Year))),size=2.8,fontface="bold")+
    theme_classic()+mytheme+#ylab("Monthly Median Black Carbon Density ")+
    labs(y = bquote('Median Black Carbon'~(Kg/M^-2)), x = "Month")+
scale_y_continuous(sec.axis =sec_axis(trans = stdd ,name="Standardized (2 SD)"))

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
    geom_text(x=0,y=0.2,label="Slope~(beta)~1",color="black",fontface="bold",size=6,parse=TRUE)+
    geom_text(x=2.5,y=0.1,label="Slope~(beta)~2",color="black",fontface="bold",size=6,parse=TRUE)+
    annotate("segment", x = 0, xend = 0, y = 0.175, yend = 0.12, colour = "black", size=1.5, arrow=arrow())+
    annotate("segment", x = 2.5, xend = 2.4, y = 0.075, yend = 0.02, colour = "black", size=1.5, arrow=arrow())+
    labs(color = "Breakpoint")+xlab("Smoke (standardized)")+ylab("Visitation")+
    geom_segment(x = 0.5,xend=0.5,y=-5.25,yend=-0.1,color="#37a7abff",linetype="dashed",size=1)+
    geom_segment(x = 1,xend=1,y=-5.25,yend=0,color="#36679eff",linetype="dashed",size=1)+
    geom_segment(x = 1.5,xend=1.5,y=-5.25,yend=0.1,color="#3d3369ff",linetype="dashed",size=1)+
    theme_classic()+mytheme+theme(legend.position = "none") 
  
  
  ###make data list used for modeling
  #create a 1 year AR lag
  dat$AR_Vis<-lag(dat$RecreationVisits,n=384)
  x<-dat[385:1920,]$AR_Vis==dat[1:1536,]$RecreationVisits
  which(x != TRUE)
  
  #create a 2 year AR lag
  #dat$AR_Vis2<-lag(dat$RecreationVisits,n=768)
  #x<-dat[769:1920,]$AR_Vis2==dat[1:1152,]$RecreationVisits
  #which(x != TRUE)
  
  #create a 3 year AR lag
  #dat$AR_Vis3<-lag(dat$RecreationVisits,n=1152)
  #x<-dat[1153:1920,]$AR_Vis3==dat[1:768,]$RecreationVisits
  #which(x != TRUE)
  
 
  
  #get ridof the 1st 3 years of data
  dat<-na.omit(dat)
  
  #filter to only high season data
  dat<-dat%>%filter(SeasType =="High")
  
  #stdize function as per Gelman reccomendation
  stdize<-function(x){
    (x-mean(x))/(2*sd(x))}
  
  #data for modeling
  data_list <- list(
    N = nrow(dat),
    Nprk = length(unique(dat$UnitCode)),
    count = dat$RecreationVisits,
    #smoke = stdize(dat$medSmoke),
    arVis = stdize(dat$AR_Vis),
   # arVis2 = stdize(dat$AR_Vis2),
    #arVis3 = stdize(dat$AR_Vis3),
    pcode = as.numeric(as.factor(dat$UnitCode ))
  )
  
  
  
  #validation plot
  load("~/SmokeProject/ModelObjects/modAROnly.rda")
  y<-data_list$count #for pp checking in shinystan
  #shinystan::launch_shinystan(mod)
  z<-rstan::extract(mod)
  preds<-apply(z$count_pred,2,median)
  plot(y,preds)
  cor(y,preds)
  
  dat$PredVis<-preds
  
  dat$Date <- as.Date(paste(dat$Year, dat$Month, sep="-"), "%Y-%M")
  
  dat%>%filter(UnitCode =="GLAC")%>%
    ggplot(.,aes(x=Date))+geom_point(aes(y=RecreationVisits),color="blue")+geom_point(aes(y=PredVis),color="red")

  ggplot(dat,aes(x=RecreationVisits,y=PredVis))+
    geom_abline(intercept = 0, slope = 1,linetype="dashed",color="#525252",size=1.5 )+
    geom_point(alpha=0.5)+theme_classic()+mytheme+
    scale_x_continuous(labels = scales::comma)+scale_y_continuous(labels = scales::comma)+
    ylab("Predicted Visitation")+xlab("Actual Visitation")+
    geom_text(x=5000,y=20000,color="black",label=paste0("R^2 == ", round(cor(y,preds)^2,digits = 2)),parse=TRUE)
     
  
  
  
  y<-data_list$count #for pp checking in shinystan
  #shinystan::launch_shinystan(mod)
  z<-rstan::extract(modSmoke2)
  preds<-apply(z$count_pred,2,median)
  plot(y,preds)
  cor(y,preds)
  
  dat$PredVis<-preds
  
  dat$Date <- as.Date(paste(dat$Year, dat$Month, sep="-"), "%Y-%M")
  
  dat%>%filter(UnitCode =="GLAC")%>%
    ggplot(.,aes(x=Date))+geom_point(aes(y=RecreationVisits),color="blue")+geom_point(aes(y=PredVis),color="red")
  
  ggplot(dat,aes(x=RecreationVisits,y=PredVis))+
    geom_abline(intercept = 0, slope = 1,linetype="dashed",color="#525252",size=1.5 )+
    geom_point(alpha=0.5)+theme_classic()+mytheme+
    ylab("Predicted Visitation")+xlab("Actual Visitation")+
    geom_text(x=5000,y=20000,color="black",label=paste0("R^2 == ", round(cor(y,preds)^2,digits = 2)),parse=TRUE)
  
  
  #####
  #Plot actual model lines
  #Just start from begnning
  dat<-read_csv("~/SmokeProject/Data/MergedDataCompleteFINAL.csv")[,-1]
  #filter to only high season data
  #dat<-dat%>%filter(SeasType =="High")
  
  #stdize function as per Gelman reccomendation
  stdize<-function(x){
    (x-mean(x))/(2*sd(x))}
  dat$Date<-zoo::as.yearmon(paste(dat$Year, dat$Month), "%Y %m")
  dat$stdsmokemed<-stdize(dat$medSmoke)
  dat$AR_Vis<-lag(dat$RecreationVisits,n=384)
  x<-dat[385:1920,]$AR_Vis==dat[1:1536,]$RecreationVisits
  which(x != TRUE)
  
  #create a 2 year AR lag
  #dat$AR_Vis2<-lag(dat$RecreationVisits,n=768)
  #x<-dat[769:1920,]$AR_Vis2==dat[1:1152,]$RecreationVisits
  #which(x != TRUE)
  
  #create a 3 year AR lag
  #dat$AR_Vis3<-lag(dat$RecreationVisits,n=1152)
  #x<-dat[1153:1920,]$AR_Vis3==dat[1:768,]$RecreationVisits
  #which(x != TRUE)
  
  
  
  #get ridof the 1st 3 years of data
  dat<-na.omit(dat)
  #filter to only high season data
  dat<-dat%>%filter(SeasType =="High")
  
  load("~/SmokeProject/ModelObjects/modSmokeSet0_5.rda")
  z<-rstan::extract(modSmoke2)
  slp1s<-apply(z$slope1,2,median)
  slp2s<-apply(z$slope2,2,median)
  int<-rep(median(z$Intercept),32)
  rints<-apply(z$ran_intercept,2,median)
  dt<-data.frame(UnitCode=sort(unique(dat$UnitCode)),slp1s=slp1s,slp2s=slp2s,
                 int=int,rints=rints)
  
  
 xx<-dat%>%group_by(UnitCode)%>%
   summarize(minsmoke=min(stdsmokemed),maxsmoke=max(stdsmokemed)) 
 
smokeranges<-data.frame(UnitCode=NA,Value=NA)
 for(i in 1:32){
 l<-seq(xx[[i,2]],xx[[i,3]],by=0.01)
   l<-data.frame(UnitCode=rep(xx[[i,1]],length(l)),Value=l)
   smokeranges<-rbind(smokeranges,l)
 }
 smokeranges<-smokeranges[-1,] 
 smokeranges<-smokeranges[rep(seq_len(nrow(smokeranges)), each = 100), ]
row.names(smokeranges)<-NULL

smokeranges$draw<-rep(1:100)
smokeranges$prknum<-as.integer(as.factor(smokeranges$UnitCode))
###ADD Prediction from posterior here!!!!!

smkeq<-function(prknum, value,draw){
  if(value<0.5){
    pred = exp(z$Intercept[draw]+ z$slope1[draw,prknum]*value+z$ran_intercept[draw,prknum])
  }
  if(value>=0.5){
    pred = exp(z$Intercept[draw]+ z$slope1[draw,prknum]*0.5+(value-0.5)*z$slope2[draw,prknum]+
                 z$ran_intercept[draw,prknum])
  }
  #pred=rnbinom(1,mu=pred,size=z$phi[draw])
  return(pred)
}
 
smkeq(32,-0.07159349,5757)
set.seed(1)
smokeranges$pred<-rep(NA,nrow(smokeranges))
samps<-rep(sample(x=1:length(z$Intercept),size=100),9205)
for(i in 1:nrow(smokeranges)){
  smokeranges[i,]$pred<-smkeq(prknum=smokeranges[i,]$prknum,value= smokeranges[i,]$Value,draw=samps[i]  )
  #print(i) #nrow ~900,000
}

#write.csv(smokeranges,file="MarginalEffectsSmoke.csv")
smokeranges<-read.csv("MarginalEffectsSmoke.csv")

dat%>%group_by(UnitCode)%>%
  summarise(x=median(stdsmokemed))%>%arrange(desc(x) )

dat$NAME<-str_remove(dat$ParkName, " NP")
dat$NAME<-str_remove(dat$NAME, " & PRES")

 dtsub<-dt%>%filter(UnitCode %in%c("GRTE","REDW","GLAC","GRSA"))
 datsub<-dat%>%filter(UnitCode %in%c("GRTE","REDW","GLAC","GRSA"))
 srsub<-smokeranges%>%filter(UnitCode %in%c("GRTE","REDW","GLAC","GRSA"))
 
 srsub$NAME<-ifelse(srsub$UnitCode=="GRTE","Grand Teton",
                    ifelse(srsub$UnitCode=="REDW","Redwood",
                           ifelse(srsub$UnitCode=="GLAC","Glacier","Great Sand Dunes")))
#Need to cut the lines pre/post bp using geom_segment

  
  

  
##This is the one
 ann_text <- data.frame(Value = 1.20,pred = 30500,lab = "Breakpoint is 0.5",
                        NAME = "Grand Teton")
 
 
srsub %>%
  ggplot(.,aes(x=Value,y=pred))+
  tidybayes::stat_lineribbon(aes(fill_ramp = stat(.width)), .width = ppoints(50), fill = "#2171b5") +
  ggdist::scale_fill_ramp_continuous(range = c(0.95, 0),name="Credibility\nInterval" )+
  scale_y_continuous(labels =  scales::comma  )+
  geom_point(data=datsub, aes(x=stdsmokemed,y=RecreationVisits),alpha=0.6)+
  facet_wrap(~NAME,scales="free_x")+theme_minimal()+
  geom_vline(xintercept=0.5,color="darkgray",alpha=0.9,linetype=2)+
  ylab("Monthly Recreational Visits")+xlab("Smoke (Standardized)")+
  theme(strip.text = element_text(size=12,face="bold"))+mytheme+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  geom_text(data = ann_text,label = "Breakpoint is 0.5", fontface="bold",size=3 )+
  geom_segment(x = 0.8, xend = 0.51, y = 30000, yend=  28000,
    arrow = arrow(length = unit(0.03, "npc")),size=1,
    data = data.frame(NAME = "Grand Teton")) +
  theme(axis.title.y = element_text(vjust=2))
  
  
  
  