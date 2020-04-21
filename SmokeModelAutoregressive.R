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
  phi<-mean(phi_post)
  dist <- rnbinom(n=length(intercept_post),size= phi, mu=mu)
  mean_est<-mean(dist)
  lower_80_est<-quantile(dist, .10)
  upper_80_est<-quantile(dist, .90)
  df<-data.frame(mean_est,lower_80_est,upper_80_est)
  rownames(df)<-c()
  return(df)
}
###


empty_df <- data.frame()
for(i in 1:nrow(dat)){
  data<-dat[i,]
  dat2<-gen_quant_r(data$stdsmoke,data$PF,data$stdARV)
  empty_df <- rbind(empty_df,dat2)
}

plot(empty_df$mean_est,dat$RecreationVisits)
cor(empty_df$mean_est,dat$RecreationVisits)


#Now try with lower smoke data 
dat<-dat%>%group_by(UnitCode)%>%
  mutate(minsmoke=min(stdsmoke))

predNoSmoke <- c()
for(i in 1:nrow(dat)){
  data<-dat[i,]
  dat2<-gen_quant_r(data$minsmoke,data$PF,data$stdARV)
  predNoSmoke <- rbind(predNoSmoke,dat2)
}
plot(predNoSmoke$mean_est,dat$RecreationVisits)

dat$No_Smoke_Mean_est<-predNoSmoke$mean_est
dat$No_Smoke_lower_80_est<-predNoSmoke$lower_80_est
dat$No_Smoke_upper_80_est<-predNoSmoke$upper_80_est
dat$date<-zoo::as.yearmon(paste0(dat$Month,dat$Year),"%m%Y")


ggplot(dat,aes(x=date))+
  geom_ribbon(aes(ymin=No_Smoke_lower_80_est, ymax=No_Smoke_upper_80_est),fill="#a6cee3",alpha=0.99)+
  geom_line(aes(y=No_Smoke_Mean_est),color="#1f78b4",size=1,linetype="dotted",alpha=0.99)+
  geom_line(aes(y=RecreationVisits),color="black",size=1,alpha=0.99)+
  theme_classic()

dat%>%filter(UnitCode=="KOVA")%>%
  ggplot(.,aes(x=date))+
  geom_ribbon(aes(ymin=No_Smoke_lower_80_est, ymax=No_Smoke_upper_80_est),fill="#a6cee3",alpha=0.99)+
  geom_line(aes(y=No_Smoke_Mean_est),color="#1f78b4",size=1,alpha=0.99)+
  geom_line(aes(y=RecreationVisits),color="black",size=1,alpha=0.99)+
  theme_classic()+ggtitle("KOVA")


sigdat<-dat%>% filter(PF %in% Parks)

LD<-sigdat%>%select(UnitCode,Year,Month,date,RecreationVisits,No_Smoke_Mean_est)

write.csv(LD,file="VisitationEstimatesMinSmokeSignificantParks.csv")







