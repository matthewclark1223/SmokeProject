ext_fit<-rstan::extract(zz)

intercept_post<-ext_fit$intercept
slope1_post<-ext_fit$slope1
slope2_post<-ext_fit$slope2
sigma_pr_post<-ext_fit$sigma_pr
ran_intercept_post<-ext_fit$ran_intercept
sigma_post<-ext_fit$sigma
bkpoint_post<-ext_fit$bkpoint
lp__post<-ext_fit$lp__


## estimate posterior
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

gen_quant_r <- function(x,y) { #x here is the input smoke values #y is park
  if(x<median(bkpoint_post[,y])){
    mu<-median(intercept_post)+median(slope1_post[,y])*x+median(ran_intercept_post[,y])
  }
  
  if(x>=median(bkpoint_post[,y])){
    mu<-median(intercept_post)+median(slope1_post[,y])*median(bkpoint_post[,y])+(x-median(bkpoint_post[,y]))*
      median(slope2_post[,y])+median(ran_intercept_post[,y])
  }
  
  visDifExp<-rnorm(nrow(intercept_post),mu,median(sigma_post))
  return(visDifExp)
}

gen_quant_r(0.5,7)



Data$UnitCodeFactor<-as.integer(as.factor(Data$UnitCode))
pred50<-rep(NA,nrow(Data)) 
for(i in 1:nrow(Data)){
  pred50[i]<-median(gen_quant_r(x=Data[i,]$stdsmoke,y=Data[i,]$UnitCodeFactor))
  
}

cor(pred50,Data$VisDiff)

Data$pred50<-pred50

ggplot(xxx,aes(x=VisDiff,y=pred50))+
         geom_point(aes(color=UnitCode))
         ylim(-12000,9800)




xxx<-Data%>%filter(VisDiff<1000)%>%filter(stdsmoke>=0.5)
cor(xxx$pred50,xxx$VisDiff)
