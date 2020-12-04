data {
  int<lower=0> N;
  int count[N];
  int Nprk;
  int pcode[N];
  real smoke[N];
  real arVis[N];
  real bkpoint;
}

parameters {
  real Intercept;
  real AR_term[Nprk];
  real slope1[Nprk]; //the regression parameters
  real slope2[Nprk]; //the regression parameters
  real ran_intercept[Nprk];
  real<lower=0>  phi; //the overdispersion parameters
  
}

model {
  
  phi ~ normal(2, 1);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ normal(0,3);
  AR_term ~ normal(1,2);
  slope2 ~ cauchy(0,2.5);
 
  for (n in 1:N){
    if(smoke[n]<bkpoint){
   count[n] ~ neg_binomial_2(exp(Intercept + AR_term[pcode[n]]*arVis[n]+
    slope1[pcode[n]]*smoke[n]+ran_intercept[pcode[n]]), phi);
  }
  else{
  count[n] ~ neg_binomial_2(exp(Intercept + AR_term[pcode[n]]*arVis[n]+
  slope1[pcode[n]]*bkpoint+(smoke[n]-bkpoint)*slope2[pcode[n]]+
  ran_intercept[pcode[n]]), phi);
}
}
 
  for(j in 1:Nprk){
  ran_intercept[j]~normal(0,1);
  }
}
