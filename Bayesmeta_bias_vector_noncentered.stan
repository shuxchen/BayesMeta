data {
  int<lower=0> J;                         // number of studies
  int<lower=0> K;                         // number of biases
  vector[J] beta;                         // estimated beta coefficient
  vector<lower=0>[J] sigma;               // standard error of beta coefficient
  matrix[J, K] I_bias;
}

parameters {
  vector[J] gamma_tilde;                        // per study effect
  vector[K] bias;                        // biases
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

transformed parameters {
  vector[J] theta;
  vector[J] gamma;
  gamma = mu + tau * gamma_tilde;
  theta = gamma + I_bias * bias;
}

model {
  mu ~ normal(0, 0.1);
  tau ~ cauchy(0, 0.05);
  bias ~ normal(0, 0.1);
  gamma_tilde ~ normal(0, 1);
  beta ~ normal(theta, sigma);
}

generated quantities {
}


