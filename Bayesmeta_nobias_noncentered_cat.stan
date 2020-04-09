data {
  int<lower=0> J;                         // number of studies with CTS N
  vector[J] beta;                         // estimated beta coefficient
  vector<lower=0>[J] sigma;               // standard error of beta coefficient
  vector<lower=0>[J] N_mean;              // mean # of entrants
  #vector<lower=0>[J] sd_mean;             // sd # of entrants
  
  int<lower=0> M;                         // number of studies with categorical N
  #vector[M] beta1;                         // estimated beta1 coefficient
  #vector[M] beta2;                         // estimated beta2 coefficient
  #vector[M] beta3;                         // estimated beta3 coefficient
  real beta1;                         // estimated beta1 coefficient
  real beta2;                         // estimated beta2 coefficient
  real beta3;                         // estimated beta3 coefficient

  #vector<lower=0>[M] sigma1;               // standard error of beta1 coefficient
  #vector<lower=0>[M] sigma2;               // standard error of beta2 coefficient
  #vector<lower=0>[M] sigma3;               // standard error of beta3 coefficient
  real<lower=0> sigma1;               // standard error of beta1 coefficient
  real<lower=0> sigma2;               // standard error of beta2 coefficient
  real<lower=0> sigma3;               // standard error of beta3 coefficient

}

parameters {
  vector[J] gamma1_tilde;                 // per study effect
  vector[J] gamma2_tilde;                 // per study effect
  vector[J] gamma3_tilde;                 // per study effect
  
  #vector[M] gamma1_cat_tilde;             // per study effect
  #vector[M] gamma2_cat_tilde;             // per study effect
  #vector[M] gamma3_cat_tilde;             // per study effect
  real gamma1_cat_tilde;             // per study effect
  real gamma2_cat_tilde;             // per study effect
  real gamma3_cat_tilde;             // per study effect

  real mu1;                               // mean effect of second entrant
  real mu2;                               // mean effect of third entrant
  real mu3;                               // mean effect of 4+ entrants

  real<lower=0> tau1;                     // deviation of effects
  real<lower=0> tau2;                     // deviation of effects
  real<lower=0> tau3;                     // deviation of effects
  
  vector<lower=0, upper=1>[J] p1;         // % with second entrant
  vector<lower=0, upper=1>[J] p2;         // % with third entrant
  vector<lower=0, upper=1>[J] p3;         // % with 4+ entrants

  real<lower=0> N4;                      // Number of entrants (4+)

}

transformed parameters {
  vector[J] gamma;
  #vector[M] gamma1;
  #vector[M] gamma2;
  #vector[M] gamma3;

  real gamma1;
  real gamma2;
  real gamma3;

  gamma = mu1 * p1 + mu2 * p2 + mu3 * p3 + tau1 * gamma1_tilde + tau2 * gamma2_tilde + tau3 * gamma3_tilde;
 
  gamma1 = mu1 + tau1 * gamma1_cat_tilde;

  gamma2 = mu2 + tau2 * gamma2_cat_tilde;

  gamma3 = mu3 + tau3 * gamma3_cat_tilde;

}

model {
  mu1 ~ normal(0, 10);
  mu2 ~ normal(0, 10);
  mu3 ~ normal(0, 10);
  
  tau1 ~ cauchy(0, 0.0025);
  tau2 ~ cauchy(0, 0.0025);
  tau3 ~ cauchy(0, 0.0025);
  
  gamma1_tilde ~ normal(0, 1);
  gamma2_tilde ~ normal(0, 1);
  gamma3_tilde ~ normal(0, 1);
  
  gamma1_cat_tilde ~ normal(0, 1);
  gamma2_cat_tilde ~ normal(0, 1);
  gamma3_cat_tilde ~ normal(0, 1);
  
  p1 ~ beta(36, 40);
  p2 ~ beta(33, 40);
  p3 ~ beta(29, 30);
  
  N4 ~ uniform(4, 8);
  
  beta ~ normal(gamma, sigma);

  beta1 ~ normal(gamma1, sigma1);  
  beta2 ~ normal(gamma2, sigma2);  
  beta3 ~ normal(gamma3, sigma3);  

  N_mean ~ normal(1 + p1 * 2 + p2 * 3 + p3 * N4, 3);

}
