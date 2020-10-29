data {
  int<lower=0> N;
  int count[N];
  int Nprk;
  int pcode[N];
  real smoke[N];
  real arVis[N];
}

parameters {
  real Intercept;
  real AR_term[Nprk];
  real<lower=0> sigma_pr;
  real slope1; //the regression parameters
  real slope2[Nprk]; //the regression parameters
  real ran_intercept[Nprk];
  real<lower=0>  phi; //the overdispersion parameters
  real bkpoint;
}

model {
  phi ~ cauchy(0, 2.5);
  sigma_pr ~normal(0,1);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5);
  AR_term ~ cauchy(0,2.5);
  bkpoint~normal(0,1);
  slope2 ~ cauchy(0,2.5);
 
  for (n in 1:N){
    if(smoke[n]<bkpoint){
   count[n] ~ neg_binomial_2(exp(Intercept + AR_term[pcode[n]]*arVis[n]+
    slope1*smoke[n]+ran_intercept[pcode[n]]), phi);
  }
  else{
  count[n] ~ neg_binomial_2(exp(Intercept + AR_term[pcode[n]]*arVis[n]+
  slope1*bkpoint+(smoke[n]-bkpoint)*slope2[pcode[n]]+
  ran_intercept[pcode[n]]), phi);
}
}
 
  for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
  }
}


generated quantities {
  
  int<lower = 0> count_pred[N];
  
  for (i in 1:N){
    
    if(smoke[i]<bkpoint){
   count_pred[i] = neg_binomial_2_rng(exp(Intercept + AR_term[pcode[i]]*arVis[i]+
    slope1*smoke[i]+ran_intercept[pcode[i]]), phi);
  }
  else{
  count_pred[i] = neg_binomial_2_rng(exp(Intercept + AR_term[pcode[i]]*arVis[i]+
  slope1*bkpoint+(smoke[i]-bkpoint)*slope2[pcode[i]]+
  ran_intercept[pcode[i]]), phi);
}
}}

