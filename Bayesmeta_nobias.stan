data {
  int<lower=0> J;                       // number of studies
  real beta[J];                         // estimated beta coefficient
  real<lower=0> sigma[J];               // standard error of beta coefficient
}

parameters {
  real gamma[J];                         // per study effect
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

#transformed parameters {
#}

model {
  for (j in 1:J)
  beta[j] ~ normal(gamma[j], sigma[j]);
  gamma ~ normal(mu, tau);
  mu ~ normal(0, 0.1);
  tau ~ cauchy(0, 0.05);
}

