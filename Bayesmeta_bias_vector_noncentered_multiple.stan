data {
  int<lower=0> J;                         // number of studies
  int<lower=0> L;                         // number of models (may be multiple for one study)
  int<lower=0> K;                         // number of biases
  vector[L] beta;                         // estimated beta coefficient
  vector<lower=0>[L] sigma;               // which study the model belongs to 
  matrix[L, K] I_bias;
  matrix[L, J] L_matrix;
}

parameters {
  vector[J] gamma_tilde;                 // per study effect
  vector[K] bias;                        // biases
  real mu;                               // mean effect
  real<lower=0> tau;                     // deviation of effects
}

transformed parameters {
  vector[L] theta;
  vector[J] gamma;
  gamma = mu + tau * gamma_tilde;
  theta = L_matrix *gamma   + I_bias * bias;
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


