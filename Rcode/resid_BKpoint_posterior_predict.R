ext_fit<-rstan::extract(zz)

intercept_post<-ext_fit$intercept
slope1_post<-ext_fit$slope1
slope2_post<-ext_fit$slope2
sigma_pr_post<-ext_fit$sigma_pr
ran_intercept_post<-ext_fit$ran_intercept
sigma_post<-ext_fit$sigma
bkpoint_post<-ext_fit$bkpoint
lp__post<-ext_fit$lp__


#start from here for points that are beyone breakpoint and come from a significantly (90%ci) slope 2
Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
x<-as.data.frame(zz) #zz is the model object 

x50<-as.data.frame(apply(x,2,quantile,probs=0.5)) #median par estimates
x05<-as.data.frame(apply(x,2,quantile,probs=0.05)) #5% par estimates
x95<-as.data.frame(apply(x,2,quantile,probs=0.95)) #95% par estimates

#put them all into a df
x<-data.frame(UnitCode= unique(dat$UnitCode), 
              slope1_0.5=x50[2:33,1],slope1_0.05=x05[2:33,1],slope1_0.95=x95[2:33,1],
              slope2_0.5=x50[34:65,1],slope2_0.05=x05[34:65,1],slope2_0.95=x95[34:65,1],
              ran_int_0.5 = x50[67:98,1],ran_int_0.05 = x05[67:98,1],ran_int_0.95 = x95[67:98,1],
              bkpt_0.5=x50[100:131,1],bkpt_0.05=x05[100:131,1],bkpt_0.95=x95[100:131,1],
              intercept_0.5=rep(x50[1,1],32),intercept_0.05=rep(x05[1,1],32),intercept_0.95=rep(x95[1,1],32))


bpz<-x%>%select(UnitCode,bkpt_0.5)
negdat<-Data%>%filter(UnitCode %in% x[which(x$slope2_0.05 <0 & x$slope2_0.95<0),]$UnitCode)

datta<-merge(negdat,bpz, by="UnitCode")
library(magrittr)
datta%<>%filter(stdsmoke>=bkpt_0.5)


gen_quant_r <- function(x,y) { #x here is the input smoke values #y is park
  if(x<median(bkpoint_post[,y])){
    mu<-intercept_post+slope1_post[,y]*x+ran_intercept_post[,y]
  }
  
  if(x>=median(bkpoint_post[,y])){
    mu<-intercept_post+slope1_post[,y]*bkpoint_post[,y]+(x-bkpoint_post[,y])*
      slope2_post[,y]+ran_intercept_post[,y]
  }
  
  visDifExp<-rnorm(nrow(intercept_post),mu,median(sigma_post))
  return(visDifExp)
}

gen_quant_r(0.5,7)



pred50<-rep(NA,nrow(datta)) 
for(i in 1:nrow(datta)){
  pred50[i]<-median(gen_quant_r(x=datta[i,]$stdsmoke,y=datta[i,]$UnitCodeFactor))
  
}

cor(pred50,datta$VisDiff)^2
datta$pred50<-pred50

ggplot(datta,aes(x=VisDiff,y=pred50))+
  geom_point(aes(color=UnitCode))+geom_smooth(aes(color=UnitCode),se=F,method="lm")



