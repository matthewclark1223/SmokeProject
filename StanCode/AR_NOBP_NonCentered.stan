data {
  int<lower=0> N;
  int count[N];
  int Nprk;
  int pcode[N];
  real smoke[N];
  real arVis[N];
  real arVis2[N];
}

parameters {
  real Intercept;
  real AR_term[Nprk];
  real AR_term2[Nprk];
  real AR_term3[Nprk];
  real<lower=0> sigma_pr;
  real slope1[Nprk]; //the regression parameters
  real ran_intercept[Nprk];
  real<lower=0>  phi; //the overdispersion parameters
}

transformed parameters{
  real count_raw[N];
  for (n in 1:N){
    
    count_raw[n] = Intercept + AR_term[pcode[n]]*arVis[n]+
     AR_term2[pcode[n]]*arVis2[n]+
    slope1[pcode[n]]*smoke[n]+ran_intercept[pcode[n]];
    
    
    
  }
 

  
}

model {
  phi ~ cauchy(0, 2.5);
  sigma_pr ~normal(0,1);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5);
  AR_term ~ cauchy(0,2.5);
  AR_term2 ~ cauchy(0,2.5);

 for (i in 1:N){
  count[i] ~ neg_binomial_2_log(count_raw[i], phi);
}
 
  for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
  }
}

generated quantities {
  real mu[N];
  vector[N] log_lik;
  vector[N] y_rep;
  
  for (i in 1:N) {
    
    mu[i] = exp(count_raw[i]);
    log_lik[i] = neg_binomial_2_log_lpmf(count[i] | count_raw[i], phi);
    y_rep[i] = neg_binomial_2_rng(mu[i], phi);
  }
}




