data {
  int<lower=0> N;
  int count[N];
  real a[N];
}

parameters {
  real Intercept;
  real coef_a;
  real<lower=0>  phi; //the overdispersion parameters
  
}

model {
  phi ~ cauchy(0,2.5);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  coef_a ~ cauchy(0,2.5);
 
  for (n in 1:N){
   count[n] ~ neg_binomial_2(exp(Intercept + coef_a*a[n]), phi);
}
}

generated quantities {
  int<lower = 0> count_pred[N];
  
  for (i in 1:N){
    
    count_pred[i] = neg_binomial_2_rng(exp(Intercept + coef_a*a[i]), phi);
  }}
  
