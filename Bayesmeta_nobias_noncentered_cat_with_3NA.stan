data {
  int<lower = 0> J_obs;                   // number of studies with pi given 
  int<lower = 0> J_mis;                   // number of studies with pi NA
  int<lower = 1, upper = J_obs + J_mis> ii_mis[J_mis];  // number of studies with pi missing
  int<lower = 1, upper = J_obs + J_mis> ii_obs;  // number of studies with pi given 

  vector[J_obs + J_mis] beta;                         // estimated beta coefficient
  vector<lower=0>[J_obs + J_mis] sigma;           // standard error of beta coefficient
  vector<lower=0>[J_obs + J_mis] N_mean;          // mean # of entrants
  #vector[J] beta;                         // estimated beta coefficient
  #vector<lower=0>[J] sigma;           // standard error of beta coefficient
  #vector<lower=0>[J] N_mean;          // mean # of entrants


  real<lower=0, upper=1> p1_obs;   // % with second entrant
  real<lower=0, upper=1> p2_obs;   // % with third entrant
  real<lower=0, upper=1> p3_obs;   // % with 4+ entrants
  #vector<lower=0, upper=1>[J_obs + J_mis] p1_obs;   // % with second entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p2_obs;   // % with third entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p3_obs;   // % with 4+ entrants
  #vector<lower=0, upper=1>[J_obs + J_mis] p1;   // % with second entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p2;   // % with third entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p3;   // % with 4+ entrants


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

transformed data {
  
    int<lower = 0> J = J_obs + J_mis;

}

parameters {
  
  vector[J] gamma1_tilde;                 // per study effect
  vector[J] gamma2_tilde;                 // per study effect
  vector[J] gamma3_tilde;                 // per study effect
  
  vector<lower=0, upper=1>[J_mis] p1_mis;     // % with second entrant
  vector<lower=0, upper=1>[J_mis] p2_mis;     // % with third entrant
  vector<lower=0, upper=1>[J_mis] p3_mis;     // % with 4+ entrants

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
  
  vector<lower=0, upper=1>[P] alpha;      // dirichelet distribution 
  simplex[P] p[J];         

}

transformed parameters {
  vector[J] gamma;
  #vector[M] gamma1;
  #vector[M] gamma2;
  #vector[M] gamma3;

  real gamma1;
  real gamma2;
  real gamma3;
  
  vector[J] p1;     // % with second entrant
  vector[J] p2;     // % with third entrant
  vector[J] p3;     // % with 4+ entrants

  vector<lower=0>[J_mis] N4_mis;                      // Number of entrants (4+)


  p1[ii_obs] = p1_obs;
  p1[ii_mis] = p1_mis;
  p2[ii_obs] = p2_obs;
  p2[ii_mis] = p2_mis;
  p3[ii_obs] = p3_obs;
  p3[ii_mis] = p3_mis;
  #p1 = p1_obs;
  #p2 = p2_obs;
  #p3 = p3_obs;
  
  
  for (j in ii_mis) {
    N4_mis[j] = (N_mean[j] - ( 1 - p[j] )/p3[j];
  }
  
  gamma = mu1 * p1 + mu2 * p2 + mu3 * p3 + tau1 * gamma1_tilde + tau2 * gamma2_tilde + tau3 * gamma3_tilde;

 
  gamma1 = mu1 + tau1 * gamma1_cat_tilde;

  gamma2 = mu2 + tau2 * gamma2_cat_tilde;

  gamma3 = mu3 + tau3 * gamma3_cat_tilde;
  


  
  #p1[3:4] = p1_obs[1:2];
  #p1[1:2] = p1_mis[1:2];
  #p2[3:4] = p2_obs[1:2];
  #p2[1:2] = p2_mis[1:2];
  #p3[3:4] = p3_obs[1:2];
  #p3[1:2] = p3_mis[1:2];



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
  
  for (i in 1:J_mis) {
      p[i] ~ dirichlet(alpha);
    }  

  beta ~ normal(gamma, sigma);

  beta1 ~ normal(gamma1, sigma1);  
  beta2 ~ normal(gamma2, sigma2);  
  beta3 ~ normal(gamma3, sigma3);  

}