data {
  int<lower = 0> J_obs;                   // number of studies with pi given 
  int<lower = 0> J_mis;                   // number of studies with pi NA
  
  vector[J_obs + J_mis] beta;                         // estimated beta coefficient
  vector<lower=0>[J_obs + J_mis] sigma;           // standard error of beta coefficient
  vector<lower=0>[J_obs + J_mis] N_mean;          // mean # of entrants

  vector<lower=0, upper=1>[J_obs + J_mis] p1;   // % with second entrant
  vector<lower=0, upper=1>[J_obs + J_mis] p2;   // % with third entrant
  vector<lower=0, upper=1>[J_obs + J_mis] p3;   // % with 4+ entrants

  int<lower=0> M;                         // number of studies with categorical N

  real beta1;                         // estimated beta1 coefficient
  real beta2;                         // estimated beta2 coefficient
  real beta3;                         // estimated beta3 coefficient

  real<lower=0> sigma1;               // standard error of beta1 coefficient
  real<lower=0> sigma2;               // standard error of beta2 coefficient
  real<lower=0> sigma3;               // standard error of beta3 coefficient
  
}

transformed data {
  
  int<lower = 0> J = J_obs + J_mis;

}

parameters {
  real gamma[J];                         // per study effect
  
  real gamma1_cat;             // per study effect
  real gamma2_cat;             // per study effect
  real gamma3_cat;             // per study effect

  real mu1;                               // mean effect of second entrant
  real mu2;                               // mean effect of third entrant
  real mu3;                               // mean effect of 4+ entrants

  real<lower=0> tau1;                     // deviation of effects
  real<lower=0> tau2;                     // deviation of effects
  real<lower=0> tau3;                     // deviation of effects
 }

#transformed parameters {
#}

model {
  for (j in 1:J)
  beta[j] ~ normal(gamma[j], sigma[j]);
  gamma ~ normal(mu1*p1 + mu2*p2 + mu3*p3, tau);
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 0.005);
}

