data {
  int N;
  real smoke[N];
  real vis[N];
  int Nprk;
  int pcode[N];
}

parameters {
  real intercept;
  real slope1[Nprk]; //the regression parameters pre break point
  real slope2[Nprk]; //regression parameter post break point
  real<lower=0> sigma_pr;
  real ran_intercept[Nprk];
  real<lower=0> sigma;
  real<lower=-1, upper = 6>  bkpoint[Nprk];

  
}

model {  
  intercept ~ normal(-5,0.5); //prior for the intercept following Gelman 2008 normal(0,1)
  slope1 ~ normal(0,0.5); //prior for the slopes following Gelman 2008 cauchy(0,2.5)
  slope2 ~ cauchy(-50,2.5); //prior for the slopes following Gelman 2008 cauchy(0,2.5)
  sigma ~ normal(350, 1);
  sigma_pr ~normal(115,1);
  bkpoint~normal(1,0.1);


for(i in 1:N){
if(vis[i]<bkpoint[pcode[i]]){
  vis[i] ~ normal(intercept+slope1[pcode[i]]*smoke[i]+ran_intercept[pcode[i]] ,sigma);
}
else{
  vis[i] ~ normal(intercept+slope1[pcode[i]]*bkpoint[pcode[i]]+(smoke[i]-bkpoint[pcode[i]])*slope2[pcode[i]]+ran_intercept[pcode[i]] ,sigma);
}
}


for(j in 1:Nprk){
  ran_intercept[j]~normal(0,sigma_pr);
}
}

