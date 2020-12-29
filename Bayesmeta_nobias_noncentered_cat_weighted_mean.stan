data {
  int<lower = 0> J_obs;                   // number of studies with pi given 
  int<lower = 0> J_mis;                   // number of studies with pi NA
  int<lower = 1, upper = J_obs + J_mis> ii_mis[J_mis];  // number of studies with pi missing
  int<lower = 1, upper = J_obs + J_mis> ii_obs;  // number of studies with pi given 
  
  vector[J_obs + J_mis] beta;                         // estimated beta coefficient
  vector<lower=0>[J_obs + J_mis] sigma;           // standard error of beta coefficient
  vector<lower=0>[J_mis] N_mean;          // mean # of entrants
  #vector[J] beta;                         // estimated beta coefficient
  #vector<lower=0>[J] sigma;           // standard error of beta coefficient
  #vector<lower=0>[J] N_mean;          // mean # of entrants


  //vector<lower=0, upper=1>[J_obs] p1_obs;   // % with second entrant
  //vector<lower=0, upper=1>[J_obs] p2_obs;   // % with third entrant
  //vector<lower=0, upper=1>[J_obs] p3_obs;   // % with 4+ entrants
  #vector<lower=0, upper=1>[J_obs + J_mis] p1_obs;   // % with second entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p2_obs;   // % with third entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p3_obs;   // % with 4+ entrants
  #vector<lower=0, upper=1>[J_obs + J_mis] p1;   // % with second entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p2;   // % with third entrant
  #vector<lower=0, upper=1>[J_obs + J_mis] p3;   // % with 4+ entrants

  int<lower=0> P;                          // number of entrant groups (1, 2, 3, ..., 10, 10+)

  vector<lower = 0, upper =1>[P] p_obs;           // % with each entrant group, in observed 

  vector[P-4] beta1;                         // estimated beta coefficient, for k = 4, 5, ..., 10
  vector[P-4] beta2;                         // estimated beta coefficient, for k = 4, 5, ..., 10
  vector[P-4] beta3;                         // estimated beta coefficient, for k = 4, 5, ..., 10

  vector<lower=0>[P-4] sigma1;                     // standard error of beta1 coefficient
  vector<lower=0>[P-4] sigma2;                     // standard error of beta2 coefficient
  vector<lower=0>[P-4] sigma3;                // standard error of beta3 coefficient, for k = 4, 5, ..., 10
  
  

}

transformed data {
  
    int<lower = 0> J = J_obs + J_mis;

}

parameters {
  
  vector[J] gamma1_tilde;                 // per study effect
  vector[J] gamma2_tilde;                 // per study effect
  vector[J] gamma3_tilde;                 // per study effect
  
  


  #vector[M] gamma1_cat_tilde;             // per study effect
  #vector[M] gamma2_cat_tilde;             // per study effect
  #vector[M] gamma3_cat_tilde;             // per study effect
  vector[P-4] gamma1_cat_tilde;             // per study effect
  vector[P-4] gamma2_cat_tilde;             // per study effect
  vector[P-4] gamma3_cat_tilde;             // per study effect

  real mu1;                               // mean effect of second entrant
  real mu2;                               // mean effect of third entrant
  real mu3;                               // mean effect of 4+ entrants

  real<lower=0> tau1;                     // deviation of effects
  real<lower=0> tau2;                     // deviation of effects
  real<lower=0> tau3;                     // deviation of effects
  
  vector[J_mis] phi_transform;                 // dispersion for studies with missing p


}

transformed parameters {
  vector[J] gamma;
  #vector[M] gamma1;
  #vector[M] gamma2;
  #vector[M] gamma3;

  vector[P-4] gamma1;
  vector[P-4] gamma2;
  vector[P-4] gamma3;
  
  //vector<lower=0>[J_mis] N4_mis;                      // Number of entrants (4+)
  vector[J_mis] phi;                

  
  vector<lower=0, upper=1>[J] p1;     // % with first entrant
  vector<lower=0, upper=1>[J] p2;     // % with second entrant
  vector<lower=0, upper=1>[J] p3;     // % with third entrant

  vector[J_mis] pb_NB_log;
  vector[J_mis] p1_NB_log;     
  vector[J_mis] p2_NB_log;     
  vector[J_mis] p3_NB_log;     

  vector<lower=0, upper=1>[J_mis] pb_NB;     // % of branded drugs with no entrant
  vector<lower=0, upper=1>[J_mis] p1_NB;     // % with first entrant
  vector<lower=0, upper=1>[J_mis] p2_NB;     // % with second entrant
  vector<lower=0, upper=1>[J_mis] p3_NB;     // % with third entrant

  vector<lower=0>[J] weight_sim;    
  vector<lower=0>[J] p4_sum;     
 
  for (j in 1:J_mis){
    phi[j] = 1/(phi_transform[j]'*phi_transform[j]);
  }
  
  for (j in 1:J_mis) {
    pb_NB_log[j] = neg_binomial_2_lpmf(0 | N_mean[j], phi[j]);
    p1_NB_log[j] = neg_binomial_2_lpmf(1 | N_mean[j], phi[j]);
    p2_NB_log[j] = neg_binomial_2_lpmf(2 | N_mean[j], phi[j]);
    p3_NB_log[j] = neg_binomial_2_lpmf(3 | N_mean[j], phi[j]);

    pb_NB[j] = exp(pb_NB_log[j]);
    p1_NB[j] = exp(p1_NB_log[j]);
    p2_NB[j] = exp(p2_NB_log[j]);
    p3_NB[j] = exp(p3_NB_log[j]);

    //p1_mis[j] = p1_NB[j]/(1 - pb_NB[j]);
    //p2_mis[j] = p2_NB[j]/(1 - pb_NB[j]);
    //p3_mis[j] = (1 - pb_NB[j] - p0_NB[j] - p1_NB[j])/(1 - pb_NB[j]);
    
  }
  
  p1[ii_mis] = p1_NB;
  p2[ii_mis] = p2_NB;
  p3[ii_mis] = p3_NB;

  p1[ii_obs] = p_obs[1];
  p2[ii_obs] = p_obs[2];
  p3[ii_obs] = p_obs[3];
  
  //p4_sum = p4*4 + p5*5 + p6*6 + p7*7 +p8*8 + p9*9 + p10*10;

  weight_sim[ii_obs] = 1/(1 - p1[ii_obs]);
  p4_sum[ii_obs] = 1 - p1[ii_obs] - p2[ii_obs] - p3[ii_obs];

  
  for (j in 1:J_mis) {
    weight_sim[j] = 1/(1 - p1[j] - pb_NB[j]);
    p4_sum[j] = 1 - p1[j] - p2[j] - p3[j];
  }
  
  for (j in 1:J) {
    gamma[j] = mu1 * p2[j]' * weight_sim[j] + mu2 * p3[j]' * weight_sim[j] + mu3 * p4_sum[j]' * weight_sim[j] + tau1 * gamma1_tilde[j] + tau2 * gamma2_tilde[j] + tau3 * gamma3_tilde[j];
    
  }
 
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
  
  phi_transform ~ normal(0.6, 0.1);
  //phi_transform ~ normal(0, 10);
  
  gamma1_tilde ~ normal(0, 1);
  gamma2_tilde ~ normal(0, 1);
  gamma3_tilde ~ normal(0, 1);
  
  gamma1_cat_tilde ~ normal(0, 1);
  gamma2_cat_tilde ~ normal(0, 1);
  gamma3_cat_tilde ~ normal(0, 1);
  
  beta ~ normal(gamma, sigma);

  beta1 ~ normal(gamma1, sigma1);  
  beta2 ~ normal(gamma2, sigma2);  
  beta3 ~ normal(gamma3, sigma3);  

}

generated quantities {

}