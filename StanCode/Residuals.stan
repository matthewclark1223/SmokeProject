
data {
  int N;
  real smoke[N];
  real vis[N];
  int Nprk;
  int pcode[N];
}

parameters {
  real intercept;
  real slope1[Nprk]; //the regression parameters
  real<lower=0> sigma_pr;
  real ran_intercept[Nprk];
  real<lower=0> sigma;

  
}

model {  
  intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5); //prior for the slopes following Gelman 2008
  sigma_pr ~normal(0,1);
  sigma ~normal(0,1);

  
for(i in 1:N){
vis[i] ~ normal(intercept+slope1[pcode[i]] * smoke[i]+ran_intercept[pcode[i]], sigma);


}
for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
}
}
