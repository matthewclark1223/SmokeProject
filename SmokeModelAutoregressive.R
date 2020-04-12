library(tidyverse)
library(rstan)
library(shinystan)
dat<-read_csv("~/SmokeProject/Data/MergedDataComplete.csv")
#stdize<-function(x) {return((x-mean(x)/(2*sd(x))))}

dat<-dat%>%group_by(UnitCode,Month)%>%mutate(ARVis = lag(RecreationVisits) )
dat<-dat%>%filter(SeasType =="High")

dat<-na.omit(dat)

dat$stdARV<-as.vector(scale(dat$ARVis))
  

data_list2 <- list(
  N = nrow(dat),
  previs = dat$stdARV,
  Nprk = length(unique(dat$UnitCode)),
  count = dat$RecreationVisits,
  smoke = dat$stdsmoke,
  pcode = as.numeric(as.factor(dat$UnitCode )))

A2<- stan( file="~/SmokeProject/StanCode/SmokeNegBinomAutoregressive.stan" , 
           data=data_list2,chains=3 ,warmup=1000,iter = 10000, control = list(max_treedepth = 25))

print( A2 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )

save(A2, file="stdsmokeNbinomAutoregressive.rda")


#Plot outputs
load("~/SmokeProject/ModelObjects/stdsmokeNbinomAutoregressive.rda")
x<-as.data.frame(summary(A2))
zz<-row.names(x[2:61,])
stan_plot(A2,pars = c(zz) ,fill_color = "purple", )


########Compare predictions to overall smokiness##
x$parameter<-row.names(x)
#get just means for parks who's par estimates don't overlap zero
x<-x[c(2:61),c(1,4,8,32)]%>%filter((summary.2.5. >0 & summary.97.5.>0) |
                                     (summary.2.5. <0 & summary.97.5.<0) )


Parks <- as.numeric(str_remove(substr(x$parameter,8,9),"]"))


dat$PF<-as.numeric(as.factor(dat$UnitCode))
sigdat<-dat%>% filter(PF %in% Parks)


ext_fit<-rstan::extract(A2)

intercept_post<-ext_fit$intercept
slope1_post<-ext_fit$slope1
slope2_post<-ext_fit$slope2
phi_post<-ext_fit$phi
sigma_pr_post<-ext_fit$sigma_pr
ran_intercept_post<-ext_fit$ran_intercept
lp__post<-ext_fit$lp__

sp1<-as.data.frame(slope1_post)
sp2<-as.data.frame(slope2_post)
ri<-as.data.frame(ran_intercept_post)



##just one estimate
gen_quant_r <- function(x,y,z) { #x here is the input smoke values #y is park z is AR
  lin_comb <- mean(intercept_post) + 
    x*mean(sp1[,paste0("V",y)])+
    z*mean(sp2[,paste0("V",y)])+
    mean(ri[,paste0("V",y)])
  mu <- exp(lin_comb)
  #phi<-sample(phi_post,size=length(x))
  phi<-mean(phi_post)
  dist <- rnbinom(n=length(intercept_post),size= phi, mu=mu)
  out<-mean(dist)
  return(out)
}
###


empty_vec <- c()
for(i in 1:nrow(dat)){
  sigdat2<-sigdat[i,]%>%dplyr::mutate(x=gen_quant_r(stdsmoke,PF,stdARV))
  empty_vec <- c(empty_vec,sigdat2$x)
}

plot(empty_vec,sigdat$RecreationVisits)
cor(empty_vec,sigdat$RecreationVisits)


#Now try with lower smoke data
sigdat<-sigdat%>%group_by(UnitCode)%>%
  mutate(minsmoke=min(stdsmoke))

predNoSmoke <- c()
for(i in 1:nrow(sigdat)){
  sigdat2<-sigdat[i,]%>%dplyr::mutate(x=gen_quant_r(minsmoke,PF,stdARV))
  predNoSmoke <- c(predNoSmoke,sigdat2$x)
}
plot(predNoSmoke,sigdat$RecreationVisits)

sigdat$predNoSmoke<-predNoSmoke
sum(sigdat$predNoSmoke-sigdat$RecreationVisits)
sigdat$date<-zoo::as.yearmon(paste0(sigdat$Month,sigdat$Year),"%m%Y")


ggplot(sigdat,aes(x=date))+
  geom_line(aes(y=RecreationVisits),color="purple",size=2,alpha=0.99)+
  geom_line(aes(y=predNoSmoke),color="green",size=2,alpha=0.25)+
  theme_classic()

sigdat%>%filter(UnitCode=="GLAC")%>%
  ggplot(.,aes(x=date))+
  geom_line(aes(y=RecreationVisits),color="purple",size=2,alpha=0.99)+
  geom_line(aes(y=predNoSmoke),color="green",size=2,alpha=0.25)+
  theme_classic()+ggtitle("GLAC")

sigdat%>%filter(UnitCode %in% c( "GLAC","HAVO"))%>%
  ggplot(.,aes(x=date))+
  geom_line(aes(y=RecreationVisits),color="purple",size=1,alpha=0.99)+
  geom_line(aes(y=predNoSmoke),color="green",size=2,alpha=0.5)+
  geom_line(aes(y=Smoke*1e14))+
  facet_wrap(~UnitCode,scales = "free")+
  theme_classic()

sigdat%>%filter(!UnitCode %in% c("GLAC","HAVO"))%>%
  ggplot(.,aes(x=date))+
  geom_line(aes(y=RecreationVisits),color="purple",size=1,alpha=0.99)+
  geom_line(aes(y=predNoSmoke),color="green",size=2,alpha=0.5)+
  geom_line(aes(y=Smoke*1e13))+
  facet_wrap(~UnitCode,scales = "free")+
  theme_classic()









