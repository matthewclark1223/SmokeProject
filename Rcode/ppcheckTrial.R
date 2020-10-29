a<-rgamma(100,15,0.8)
coef_a<-0.75
int<-4
phi<-100

mu<-(0.55+coef_a*a)



outcome<-vector()
for(i in 1:length(mu)){
  outcome[i]<-rnbinom(n=1,mu=mu[i],size=phi)
}

plot(outcome~a)

df<-data.frame(a,outcome)

data_list_t <- list(
  N = nrow(df),
  count = df$outcome,
  a = df$a)
set.seed(100) #warning about undefined number generated
model<-rstan::stan( file="~/SmokeProject/StanCode/ppCheckTrial.stan " , 
                  data=data_list_t,chains=1,iter=2000 ,warmup = 1000 ,
                  control=list(adapt_delta=0.95,max_treedepth = 10),
                  refresh= max(2000/20, 1),save_warmup = T)

set.seed(1) #warning about location parameter
model<-rstan::stan( file="~/SmokeProject/StanCode/ppCheckTrial.stan " , 
                    data=data_list_t,chains=1,iter=2000 ,warmup = 1000 ,
                    control=list(adapt_delta=0.95,max_treedepth = 10),
                    refresh= max(2000/20, 1),save_warmup = T)


set.seed(2) #no warning
model<-rstan::stan( file="~/SmokeProject/StanCode/ppCheckTrial.stan " , 
                    data=data_list_t,chains=1,iter=2000 ,warmup = 1000 ,
                    control=list(adapt_delta=0.95,max_treedepth = 10),
                    refresh= max(2000/20, 1),save_warmup = T)

set.seed(1) #warning about location parameter
model<-rstan::stan( file="~/SmokeProject/StanCode/ppCheckTrial.stan " , 
                    data=data_list_t,chains=1,iter=2000 ,warmup = 1000 ,
                    control=list(adapt_delta=0.95,max_treedepth = 10),
                    refresh= max(2000/20, 1),save_warmup = F)

set.seed(100) #no warning
model<-rstan::stan( file="~/SmokeProject/StanCode/ppCheckTrial.stan " , 
                    data=data_list_t,chains=1,iter=2000 ,warmup = 1000 ,
                    control=list(adapt_delta=0.95,max_treedepth = 10),
                    refresh= max(2000/20, 1),save_warmup = F)

print( model , probs=c( (1-0.89)/2 , 1-(1-0.89)/2 ) )
z<-extract(model)
preds<-apply(z$count_pred,2,median)
plot(outcome,preds)
cor(outcome,preds)

shinystan::launch_shinystan(model)


plot(outcome~a)
points(preds~a,col="purple")
