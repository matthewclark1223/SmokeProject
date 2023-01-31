library(tidyverse)
library(rstan)
library(ggridges)
dat<-read_csv("Data/MergedDataCompleteFINAL2.csv")[,-1]


data<-dat

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

#chaeck a random row to make sure it's all good
#dat[dat$UnitCode == "YOSE" & dat$Year ==2012 & dat$Month==7,]$RecreationVisits == dat[dat$UnitCode == "YOSE" & dat$Year ==2015 & dat$Month==7,]$AR_Vis3

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
  #arVis2 = stdize(dat$AR_Vis2),
  #arVis3 = stdize(dat$AR_Vis3),
  pcode = as.numeric(as.factor(dat$UnitCode ))
  )

options(mc.cores=7)#10

rstan::rstan_options(autowrite=TRUE)

mod<-rstan::stan( file="~/SmokeProject/StanCode/AROnly.stan " , 
                  data=data_list,chains=7,iter=5000, ##10
                  control=list(adapt_delta=0.95,max_treedepth = 10),
                  refresh= max(5000/20, 1),save_warmup=T)
save(mod,file="modAROnly2.rda") #runs with no errors

print( mod , probs=c( 0.05 , 0.95  ))

y<-data_list$count #for pp checking in shinystan
#shinystan::launch_shinystan(mod)
z<-extract(mod)
preds<-apply(z$count_pred,2,median)
plot(y,preds)
cor(y,preds)

dat$PredVis<-preds

dat$Date <- as.Date(paste(dat$Year, dat$Month, sep="-"), "%Y-%M")

dat%>%filter(UnitCode =="GLAC")%>%
ggplot(.,aes(x=Date))+geom_point(aes(y=RecreationVisits),color="blue")+geom_point(aes(y=PredVis),color="red")

#Now look at smoke model
x<-dat%>%group_by(UnitCode)%>%filter(medSmoke == max(medSmoke))

ggplot(x,aes(x=UnitCode))+geom_point(aes(y=RecreationVisits),color="blue")+geom_point(aes(y=PredVis),color="red")

t.test(x$RecreationVisits,x$PredVis)


#ggplot(dat,aes(x=Date,y=medSmoke))+geom_smooth(se=F,method="gam")+facet_wrap(~State,scales="free")
#ggplot(dat,aes(x=Date,y=medSmoke))+geom_line()+facet_wrap(~State)



##Now look at smoke breakpoint
#data for modeling
data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = stdize(dat$medSmoke),
  arVis = stdize(dat$AR_Vis),
  bkpoint = 0.5,
  #arVis2 = stdize(dat$AR_Vis2),
  #arVis3 = stdize(dat$AR_Vis3),
  pcode = as.numeric(as.factor(dat$UnitCode ))
)

options(mc.cores=7) #10

rstan::rstan_options(autowrite=TRUE)

modSmoke2<-rstan::stan( file="~/SmokeProject/StanCode/AR_Set_BP.stan " , 
                  data=data_list,chains=7,iter=5000, #10, 14000
                  control=list(adapt_delta=0.99,max_treedepth = 15), #0.999, 10
                  refresh= 5000/20,save_warmup=F)

save(modSmoke2,file="modSmokeSet0_52.rda")

print( modSmoke2 , probs=c( 0.05 , 0.95  ))
y<-data_list$count #for pp checking in shinystan
z<-extract(modSmoke2)
preds<-apply(z$count_pred,2,median)
plot(y,preds)
cor(y,preds)

### do a bp of 1
##Now look at smoke breakpoint
#data for modeling
data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = stdize(dat$medSmoke),
  arVis = stdize(dat$AR_Vis),
  bkpoint = 1,  #this is the only change from above!
  #arVis2 = stdize(dat$AR_Vis2),
  #arVis3 = stdize(dat$AR_Vis3),
  pcode = as.numeric(as.factor(dat$UnitCode ))
)

options(mc.cores=7)#10

rstan::rstan_options(autowrite=TRUE)

modSmoke3<-rstan::stan( file="~/SmokeProject/StanCode/AR_Set_BP.stan " , 
                        data=data_list,chains=7,iter=5000,
                        control=list(adapt_delta=0.99,max_treedepth = 15),
                        refresh= 5000/20,save_warmup=F)

save(modSmoke3,file="modSmokeSet12.rda")

print( modSmoke3 , probs=c( 0.05 , 0.95  ))
y<-data_list$count #for pp checking in shinystan
z<-extract(modSmoke3)
preds<-apply(z$count_pred,2,median)
plot(y,preds)
cor(y,preds)


median(z$phi)
median(z$Intercept)
median(z$AR_term[30])

### Try a bp of 0
data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = stdize(dat$medSmoke),
  arVis = stdize(dat$AR_Vis),
  bkpoint = 0,
  #arVis2 = stdize(dat$AR_Vis2),
  #arVis3 = stdize(dat$AR_Vis3),
  pcode = as.numeric(as.factor(dat$UnitCode ))
)

options(mc.cores=7)

rstan::rstan_options(autowrite=TRUE)

modSmoke5<-rstan::stan( file="~/SmokeProject/StanCode/AR_Set_BP.stan" , 
                        data=data_list,chains=7,iter=5000,
                        control=list(adapt_delta=0.99,max_treedepth = 15),
                        refresh= 5000/20,save_warmup=F)

save(modSmoke5,file="modSmokeSet02.rda")

print( modSmoke6 , probs=c( 0.05 , 0.95  ))
y<-data_list$count #for pp checking in shinystan
z<-extract(modSmoke5)
preds<-apply(z$count_pred,2,median)
plot(y,preds)
cor(y,preds)

#Try low bp
### Try a bp of 0
data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = stdize(dat$medSmoke),
  arVis = stdize(dat$AR_Vis),
  bkpoint = -0.25,
  #arVis2 = stdize(dat$AR_Vis2),
  #arVis3 = stdize(dat$AR_Vis3),
  pcode = as.numeric(as.factor(dat$UnitCode ))
)

options(mc.cores=7)

rstan::rstan_options(autowrite=TRUE)

modSmoke6<-rstan::stan( file="~/SmokeProject/StanCode/AR_Set_BP.stan" , 
                        data=data_list,chains=7,iter=5000,
                        control=list(adapt_delta=0.99,max_treedepth = 10),
                        refresh= 5000/20,save_warmup=F)


#### Smoke, no bp
dat$stdARM<-stdize(dat$AR_Vis)
fit<-stan_glmer.nb(RecreationVisits~(stdsmokemed+
                                       stdARM|UnitCode),
                   iter=7000,chains=7,data=dat)

save(fit,file="SmokeAR_NoBp2.rda")




### see how much better this model does than autoregressive alone. 


y<-posterior_predict(fit,dat)
y<-apply(y,2,median)
(cor(y,dat$RecreationVisits))^2 #get the R^2
