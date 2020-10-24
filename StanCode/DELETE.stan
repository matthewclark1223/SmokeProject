data {
  int<lower=0> N;
  int count[N];
  int Nprk;
  int pcode[N];
  real smoke[N];
  int arVis[N];
}

parameters {
  real Intercept;
  real AR_term;
  real<lower=0> sigma_pr;
  real slope1; //the regression parameters
  real ran_intercept[Nprk];
  real<lower=0>  phi; //the overdispersion parameters
}

model {
  phi ~ cauchy(0, 2.5);
  sigma_pr ~normal(0,1);
  Intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5);
  AR_term ~ cauchy(0,2.5);
  
  
 
  for (n in 1:N){
   count[n] ~ neg_binomial_2(exp(Intercept + AR_term*arVis[n]+
    slope1*smoke[n]+ran_intercept[pcode[n]]), phi);
  }
  for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
  }
}

