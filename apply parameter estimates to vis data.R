library(tidyverse)
library(rstan)
params<-read_csv("~/SmokeProject/Data/ParameterOutputs.csv")[,-c(1,3,5,6)]
fulldat<-read_csv("~/SmokeProject/Data/MergedDataComplete.csv")[,-1]
fulldat<-fulldat%>%select(UnitCode,Year,Month,ParkName,Smoke,stdsmoke,RecreationVisits,SeasType)%>%
  filter(SeasType =="High")%>%select(UnitCode,Year,Month,ParkName,Smoke,RecreationVisits,stdsmoke)
names(params)[1]<-"UnitCode"

fulldat<-left_join(fulldat,params,by="UnitCode")

load("~/SmokeProject/ModelObjects/stdsmokeNbinom.rda")
print( l2 , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )
stan_plot(l2,pars = c(zz) ,fill_color = "purple", )

x<-as.data.frame(summary(l2))
x$parameter<-row.names(x)
#get just means for parks who's par estimates don't overlap zero
x<-x[c(2:61),c(1,4,8,32)]%>%filter((summary.2.5. >0 & summary.97.5.>0) |
                    (summary.2.5. <0 & summary.97.5.<0) )


Parks <- as.numeric(str_remove(substr(x$parameter,8,9),"]"))

fulldat$PF<-as.numeric(as.factor(fulldat$UnitCode))
sigdat<-fulldat%>% filter(PF %in% Parks)

data_list_pred <- list(
  N = nrow(sigdat),
  Nprk = length(unique(sigdat$UnitCode)),
  count = sigdat$RecreationVisits,
  smoke = sigdat$stdsmoke,
  pcode = as.numeric(as.factor(sigdat$UnitCode )))

ext_fit<-rstan::extract(l2)

intercept_post<-ext_fit$intercept
slope1_post<-ext_fit$slope1
phi_post<-ext_fit$phi
sigma_pr_post<-ext_fit$sigma_pr
ran_intercept_post<-ext_fit$ran_intercept
lp__post<-ext_fit$lp__

sp<-as.data.frame(slope1_post)
ri<-as.data.frame(ran_intercept_post)

gen_quant_r <- function(x,y) { #x here is the input smoke values #y is park
  lin_comb <- sample(intercept_post, size = length(intercept_post)) + 
    x*sample(sp[,paste0("V",y)], size = length(x))+
    sample(ri[,paste0("V",y)], size = length(x))
  mu <- exp(lin_comb)
  phi<-phi_post
  dist <- rnbinom(n=length(intercept_post),size= phi, mu=mu)
  out<-mean(dist)
  return(out)
}

empty_vec <- c()
for(i in 1:nrow(sigdat)){
sigdat2<-sigdat[i,]%>%dplyr::mutate(x=gen_quant_r(stdsmoke,PF))
empty_vec <- c(empty_vec,sigdat2$x)
  }

plot(empty_vec,sigdat$RecreationVisits)
cor(empty_vec,sigdat$RecreationVisits)
sum(empty_vec)

#Now try with lower smoke data
sigdat$minsmoke<-rep(min(sigdat$stdsmoke),nrow(sigdat))

predNoSmoke <- c()
for(i in 1:nrow(sigdat)){
  sigdat2<-sigdat[i,]%>%dplyr::mutate(x=gen_quant_r(minsmoke,PF))
  predNoSmoke <- c(predNoSmoke,sigdat2$x)
}
plot(predNoSmoke,sigdat$RecreationVisits)

sum(predNoSmoke)






##not used below
pred <- stan(file = "VisPredict.stan",
             data = list( N = nrow(sigdat),
                         N_samples = length(intercept_post),
                         New_smoke = sigdat$stdsmoke,
                         intercept = intercept_post,
                         phi = phi_post,
                         sigma_pr = sigma_pr_post,
                         slope1 = slope1_post,
                         ran_intercept=ran_intercept_post,
                         Npark = length(unique(sigdat$UnitCode)),
                         pcode = sigdat$PF ),
             chains = 1, iter = 100,
             algorithm = "Fixed_param")




