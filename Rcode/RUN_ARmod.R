library(tidyverse)
library(rstan)
library(ggridges)
dat<-read_csv("Data/MergedDataCompleteFINAL.csv")[,-1]
head(dat)
range(dat$Year)
dat$halfDec<-ifelse(dat$Year %in% 1980:1984,"1980_1984",
                    ifelse(dat$Year %in% 1985:1989,"1985_1989",
                           ifelse(dat$Year %in% 1990:1994,"1990_1994",
                                  ifelse(dat$Year %in% 1995:1999,"1995_1999",
                                         ifelse(dat$Year %in% 2000:2004,"2000_2004",
                                                ifelse(dat$Year %in% 2005:2009,"2005_2010",
                                                       ifelse(dat$Year %in% 2010:2014,"2010_2014",
                                                              ifelse(dat$Year %in% 2015:2019,"2015_2019","XXX"))))))))



data<-dat


dat$AR_Vis<-lag(dat$RecreationVisits,n=384)
dat[385:1920,]$AR_Vis==dat[1:1536,]$RecreationVisits

dat<-na.omit(dat)

#dat<-dat%>%filter(halfDec =="2015_2019")

stdize<-function(x){
  (x-mean(x))/(2*sd(x))}

data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  arVis = stdize(dat$AR_Vis),
  pcode = as.numeric(as.factor(dat$UnitCode )),
  smoke = stdize(dat$medSmoke))

### Trial Zone#################################################
dat<-dat%>%filter(halfDec =="2015_2019")

stdize<-function(x){
  (x-mean(x))/(2*sd(x))}

data_list <- list(
  N = nrow(dat),
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  arVis = stdize(dat$AR_Vis),
  pcode = as.numeric(as.factor(dat$UnitCode )),
  smoke = stdize(dat$medSmoke))

options(mc.cores=6)

rstan::rstan_options(autowrite=TRUE)

#run the mod
mod<-rstan::stan( file="~/SmokeProject/StanCode/AR_Only_RNdm_sl2.stan " , 
                        data=data_list,chains=6,iter=3000,
                        control=list(adapt_delta=0.95,max_treedepth = 10),
                  refresh= max(3000/20, 1),save_warmup=T)


print( mod , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

y<-data_list$count #for pp checking in shinystan
shinystan::launch_shinystan(mod)
z<-extract(mod)
preds<-apply(z$count_pred,2,median)
plot(y,preds)
cor(y,preds)
#####################################################################

options(mc.cores=8)

rstan::rstan_options(autowrite=TRUE)

#run the moddy boi
mod_Basic<-rstan::stan( file="~/SmokeProject/StanCode/Heierarchical_AR_Model.stan " , 
                           data=data_list,chains=8,iter=4000 ,warmup = 2500 ,
                  control=list(adapt_delta=0.95,max_treedepth = 10))


print( mod_Basic , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


shinystan::launch_shinystan(mod_Basic)
save(mod_Basic,file="mod_Basic.rda")

# breakpoint for smoke
mod_BP<-rstan::stan( file="~/SmokeProject/StanCode/AR_Mod_1BP.stan " , 
                        data=data_list,chains=4,iter=5000  ,
                        control=list(adapt_delta=0.98,max_treedepth = 15),
                     refresh= max(5000/100, 1)) #refresh= max(iter/100, 1))


print( mod_BP , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


shinystan::launch_shinystan(mod_BP)


#independent bps
mod_BP_rndm<-rstan::stan( file="~/SmokeProject/StanCode/AR_Mod_MultiBP.stan " , 
                     data=data_list,chains=4,iter=3000,refresh= max(3000/100, 1),
                     control=list(adapt_delta=0.99,max_treedepth = 18))


print( mod_BP_rndm , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )


shinystan::launch_shinystan(mod_BP_rndm)

save(mod_BP_rndm,file="mod_BP_rndm.rda")




dat%>%filter(UnitCode=="ROMO")%>%
  ggplot(., aes(x=stdize(medSmoke),y=RecreationVisits))+geom_point()+geom_smooth()




