data {
  int<lower = 0> J_obs;                   // number of studies with pi given 
  int<lower = 0> J_mis;                   // number of studies with pi NA
  int<lower = 1, upper = J_obs + J_mis> ii_mis[J_mis];  // number of studies with pi missing
  int<lower = 1, upper = J_obs + J_mis> ii_obs;  // number of studies with pi given 

  vector[J_obs + J_mis] beta;                         // estimated beta coefficient
  vector<lower=0>[J_obs + J_mis] sigma;           // standard error of beta coefficient
  vector<lower=0>[J_obs + J_mis] N_mean;          // mean # of entrants

  int<lower=0> P;                          // number of entrant groups (1, 2, 3, ..., 10, 10+)

  vector<lower = 0, upper =1>[P] p_obs;           // % with each entrant group, in observed 

  int<lower=0> M;                         // number of studies with categorical N

  real beta1;                         // estimated beta1 coefficient
  real beta2;                         // estimated beta2 coefficient
  real beta3;                         // estimated beta3 coefficient

  real<lower=0> sigma1;               // standard error of beta1 coefficient
  real<lower=0> sigma2;               // standard error of beta2 coefficient
  real<lower=0> sigma3;               // standard error of beta3 coefficient
  
  vector<lower=0, upper=1>[P] alpha;      // dirichelet distribution 

}

transformed data {
  
    int<lower = 0> J = J_obs + J_mis;

}

parameters {
  
  vector[J] gamma1_tilde;                 // per study effect
  vector[J] gamma2_tilde;                 // per study effect
  vector[J] gamma3_tilde;                 // per study effect
  
  simplex[P] p_mis[J_mis];       // % with different entrants

  real gamma1_cat_tilde;             // per study effect
  real gamma2_cat_tilde;             // per study effect
  real gamma3_cat_tilde;             // per study effect

  real mu1;                               // mean effect of second entrant
  real mu2;                               // mean effect of third entrant
  real mu3;                               // mean effect of 4+ entrants

  real<lower=0> tau1;                     // deviation of effects
  real<lower=0> tau2;                     // deviation of effects
  real<lower=0> tau3;                     // deviation of effects
  
}

transformed parameters {
  vector[J] gamma;

  real gamma1;
  real gamma2;
  real gamma3;
  
  vector<lower=0, upper=1>[J] p1;     // % with second entrant
  vector<lower=0, upper=1>[J] p2;     // % with third entrant
  vector<lower=0, upper=1>[J] p3;     // % with 4+ entrants


  vector<lower=0>[J] N4_sim;                      // Number of entrants (4+)
  vector<lower=0>[J] N4;                      // Number of entrants (4+)
  
  vector<lower=0, upper=1>[J_mis] p1_mis;     // % with second entrant
  vector<lower=0, upper=1>[J_mis] p2_mis;     // % with third entrant
  vector<lower=0, upper=1>[J_mis] p3_mis;     // % with 4+ entrants

  for (j in 1:J_mis){
    p1_mis[j] = p_mis[j][2];
    p2_mis[j] = p_mis[j][3];
    p3_mis[j] = p_mis[j][4];

  } 
  
  p1[ii_mis] = p1_mis;
  p2[ii_mis] = p2_mis;
  p3[ii_mis] = p3_mis;
  p1[ii_obs] = p_obs[2];
  p2[ii_obs] = p_obs[3];
  p3[ii_obs] = p_obs[4];

  for (j in 1:J) {
    N4_sim[j] = (N_mean[j] - ( 1 - p1[j] - p2[j] - p3[j]) - 2*p1[j] - 3*p2[j] )/p3[j];
  }
  
  for (j in 1:J_mis) {
    N4[j] = fmin(N4_sim[j], 10);
  }
  N4[4] = N4_sim[4];
  
  gamma = mu1 * p1 + mu2 * p2 + N4'*mu3 * p3 + tau1 * gamma1_tilde + tau2 * gamma2_tilde + tau3 * gamma3_tilde;


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
  
  for (j in 1:J_mis) {
      p_mis[j] ~ dirichlet(alpha);
    }  

  beta ~ normal(gamma, sigma);

  beta1 ~ normal(gamma1, sigma1);  
  beta2 ~ normal(gamma2, sigma2);  
  beta3 ~ normal(gamma3, sigma3);  

}