data {
  int<lower=0> N;
  int count[N];
  int Nprk;
  int pcode[N];
  real arVis[N];
  real arVis2[N];
  real arVis3[N];
  
}

parameters {
  real Intercept;
  real AR_term[Nprk];
  real AR_term2[Nprk];
  real AR_term3[Nprk];
  real<lower=0> sigma_pr;
  real ran_intercept[Nprk];
  real<lower=0>  phi; //the overdispersion parameters
}

model {
  phi ~ cauchy(0,1);
  sigma_pr ~normal(0,1);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  AR_term ~ cauchy(0,2.5);
  AR_term2 ~ cauchy(0,2.5);
  AR_term3 ~ cauchy(0,2.5);

  
 
  for (n in 1:N){
    
  
    count[n] ~ neg_binomial_2(exp(Intercept + AR_term[pcode[n]]*arVis[n]+
    AR_term2[pcode[n]]*arVis2[n]+AR_term3[pcode[n]]*arVis3[n]+
    ran_intercept[pcode[n]]), phi);}
    
   
  
 

 
  for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
  }
}

generated quantities {
  
 int<lower = 0> count_pred[N];
  
  for (i in 1:N){
 
   count_pred[i] = neg_binomial_2_rng(exp(Intercept + AR_term[pcode[i]]*arVis[i]+
   AR_term2[pcode[i]]*arVis2[i]+AR_term3[pcode[i]]*arVis3[i]+

 ran_intercept[pcode[i]]), phi);}
    
}
    
    
    





