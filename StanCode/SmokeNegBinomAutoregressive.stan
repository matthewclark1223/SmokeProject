data { //Data we are putting into the model
  int N; //length of data
  real smoke[N]; //scaled smoke values, of length N
  int count[N]; //Visitation values of length N
  int Nprk; //Number of Parks used for varying slopes and intercepts 
  int pcode[N]; //Park codes (as factors)
  real previs[N]; // Autoregressive data. Visitation data from given month in the previous year. Scaled and length N
}
parameters { //Things we want the model to give us
  real intercept;   //intercept value
  real slope1[Nprk]; //the regression parameters. Slope of smoke for each park. 
  real slope2[Nprk]; //slope of autoregressive term for each park
 
  real<lower=0> phi; //the overdispersion parameters needed for the negative binomial distribution
  real<lower=0> sigma_pr; // variance for the random intercepts 
  real ran_intercept[Nprk]; //Random intercept for each park

  
}
model {  //lay out the priors first
  intercept ~ normal(0,1); //prior for the intercept following Gelman 2008
  slope1 ~ cauchy(0,2.5); //prior for the slopes following Gelman 2008
  slope2 ~ cauchy(0,2.5); //prior for autoregressive slope
  phi ~ cauchy(0, 3);  // prior for the overdispersion parameter 
  sigma_pr ~normal(0,1); //prior for the variance on the park specific intercept

  //Specify the model formula
for(i in 1:N){ // for every data point 
//observed visitation is dependent on the smoke value, a baseline intercept, park specific intercept, and autoregressive term
  count[i] ~ neg_binomial_2(exp(intercept+slope1[pcode[i]]*smoke[i]+
  slope2[pcode[i]]*previs[i]+
  ran_intercept[pcode[i]]) ,phi);


}
for(j in 1:Nprk){  //This estimates the random intercept for each park, Variance here also gets a prior since we are estimating it
  ran_intercept[j]~normal(0,sigma_pr);
}
}

