data {
  int<lower=0> J;                         // number of studies
  int<lower=0> K;                         // number of biases
  vector[J] beta;                         // estimated beta coefficient
  vector<lower=0>[J] sigma;               // standard error of beta coefficient
  matrix[J, K] I_bias;
}

parameters {
  vector[J] gamma;                        // per study effect
  vector[K] bias;                        // biases
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

transformed parameters {
  vector[J] theta;
  theta = gamma + I_bias * bias;
}

model {
  beta ~ normal(theta, sigma);
  gamma ~ normal(mu, tau);
  mu ~ normal(0, 0.1);
  tau ~ cauchy(0, 0.05);
  bias ~ normal(0, 0.1);
}

generated quantities {
}


