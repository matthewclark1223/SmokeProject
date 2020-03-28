//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> units;
  int<lower=0> seasons;
  int<lower=0, upper=units> unitid[N];
  int<lower=0, upper=seasons> season[N];
  int<lower = 0> visits[N];
  vector[N] smoke;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> phi;
  vector[seasons] alpha;
  vector[units] beta;
  }
model {
  phi ~ cauchy(0,2);
  beta ~ normal(0,1);
  alpha ~ normal(0,1);
  visits ~ neg_binomial_2_log(alpha[season] + beta[unitid].*smoke, phi);
}

