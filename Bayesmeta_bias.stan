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

data {
  int<lower=0> J;                       // number of studies
  real beta[J];                         // estimated beta coefficient
  real<lower=0> sigma[J];               // standard error of beta coefficient
  int<lower = 0, upper = 1> I_bias1[J];
  int<lower = 0, upper = 1> I_bias2[J];
  int<lower = 0, upper = 1> I_bias3[J];
}

parameters {
  real gamma[J];                         // per study effect
  real bias1[J];                            // bias from bias 1
  real bias2[J];                            // bias from bias 2
  real bias3[J];                            // bias from bias 3
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

transformed parameters {
  real theta[J];                         // per study effect
  theta[J] = gamma[J] + bias1[J] * I_bias1[J] + bias2[J] * I_bias2[J] + bias3[J] * I_bias3[J]
}

model {
  for (j in 1:J)
  beta[j] ~ normal(theta[j], sigma[j]);
  gamma ~ normal(mu, tau);
  mu ~ normal(0, 0.1);
  tau ~ cauchy(0, 0.05);
  bias1 ~ normal(0, 0.1);
  bias2 ~ normal(0, 0.1);
  bias3 ~ normal(0, 0.1);
}







