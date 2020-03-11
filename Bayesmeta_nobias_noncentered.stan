data {
  int<lower=0> J;                         // number of studies
  vector[J] beta;                         // estimated beta coefficient
  vector<lower=0>[J] sigma;               // standard error of beta coefficient
}

parameters {
  vector[J] gamma_tilde;                 // per study effect
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

transformed parameters {
  vector[J] gamma;
  gamma = mu + tau * gamma_tilde;

}

model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 0.0025);
  gamma_tilde ~ normal(0, 1);
  beta ~ normal(gamma, sigma);
}
