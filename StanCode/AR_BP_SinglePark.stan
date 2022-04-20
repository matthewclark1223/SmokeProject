data {
  int<lower=0> N;
  int count[N];
  real smoke[N];
  real arVis[N];
  real arVis2[N];
  real arVis3[N];
}

parameters {
  real Intercept;
  real AR_term;
  real AR_term2;
  real AR_term3;
  real slope1; //the regression parameters
  real slope2; //the regression parameters
  real<lower=0>  phi; //the overdispersion parameters
  real<lower=0, upper=10> bkpoint;
}

model {
  phi ~ cauchy(0, 2.5);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5);
  AR_term ~ cauchy(0,2.5);
  AR_term2 ~ cauchy(0,2.5);
  AR_term3 ~ cauchy(0,2.5);
  bkpoint~normal(1,2);
  slope2 ~ cauchy(0,2.5);
 
  for (n in 1:N){
    if(smoke[n]<bkpoint){
   count[n] ~ neg_binomial_2(exp(Intercept + AR_term*arVis[n]+
     AR_term2*arVis2[n]+AR_term3*arVis3[n]+
    slope1*smoke[n]), phi);
  }
  else{
  count[n] ~ neg_binomial_2(exp(Intercept + AR_term*arVis[n]+
    AR_term2*arVis2[n]+AR_term3*arVis3[n]+
  slope1*bkpoint+(smoke[n]-bkpoint)*slope2), phi);
}
}
 

}


generated quantities {
  
  int<lower = 0> count_pred[N];
  
  for (i in 1:N){
    
    if(smoke[i]<bkpoint){
   count_pred[i] = neg_binomial_2_rng(exp(Intercept + AR_term*arVis[i]+
 AR_term2*arVis2[i]+AR_term3*arVis3[i]+
    slope1*smoke[i]), phi);
  }
  else{
  count_pred[i] = neg_binomial_2_rng(exp(Intercept + AR_term*arVis[i]+
 AR_term2*arVis2[i]+AR_term3*arVis3[i]+
  slope1*bkpoint+(smoke[i]-bkpoint)*slope2), phi);
}
}}

