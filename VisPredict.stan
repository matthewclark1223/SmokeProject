data {
  int N;   //length of input data
  int N_samples;  //length of posterior draws
  vector[N] New_smoke;  //input smoke values
  vector[N_samples] intercept; //  posterior intercept
  vector[N_samples] phi; //  posterior phi
  vector[N_samples] sigma_pr; //  sigma_pr
  int Nprk;           //input number of parks  
  matrix[N_samples,Nprk] slope1;   //posterior slopes
  vector[N_samples] ran_intercept;   //posterior ran_slopes
  int pcode[N];         //input parks
}
parameters {
}
model {
}
generated quantities {
 int Pred_vis[N_samples,N];
  for(n in 1:N){ 
    for(i in 1:N_samples) {
      Pred_vis[i,n] = neg_binomial_2_rng(exp(intercept[i]+slope1[pcode[i]]*
      New_smoke[n]+ran_intercept[pcode[i]]), phi[n]);
}
   // for(j in 1:Nprk){
//ran_intercept[j] = normal(0,sigma_pr[n]);
}
}


