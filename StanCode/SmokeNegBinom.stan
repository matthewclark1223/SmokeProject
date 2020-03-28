data {
  int N;
  real smoke[N];
  int count[N];
  int Nprk;
  int pcode[N];
}
parameters {
  real intercept;
  real slope1[Nprk]; //the regression parameters
 
  real<lower=0> phi; //the overdispersion parameters
  real<lower=0> sigma_pr;
  real ran_intercept[Nprk];

  
}
model {  
  intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5); //prior for the slopes following Gelman 2008
  
  phi ~ cauchy(0, 3);
  sigma_pr ~normal(0,1);

  
for(i in 1:N){

  count[i] ~ neg_binomial_2(exp(intercept+slope1[pcode[i]]*smoke[i]+ran_intercept[pcode[i]]) ,phi);


}
for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
}
}

