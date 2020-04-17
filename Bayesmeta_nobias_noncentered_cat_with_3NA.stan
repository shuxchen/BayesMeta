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

  #int<lower=0> M;                         // number of studies with categorical N

  #vector[P-4] beta1;                         // estimated beta coefficient, for k = 4, 5, ..., 10
  #vector[P-4] beta2;                         // estimated beta coefficient, for k = 4, 5, ..., 10
  #vector[P-4] beta3;                         // estimated beta coefficient, for k = 4, 5, ..., 10

  #vector<lower=0>[P-4] sigma1;                     // standard error of beta1 coefficient
  #vector<lower=0>[P-4] sigma2;                     // standard error of beta2 coefficient
  #vector<lower=0>[P-4] sigma3;                // standard error of beta3 coefficient, for k = 4, 5, ..., 10

  real beta1;
  real beta2;
  real beta3;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  real<lower=0> sigma3;
  
  vector<lower=0, upper=1>[P] alpha;        // dirichelet distribution 

}

transformed data {
  
  int<lower = 0> J = J_obs + J_mis;

}

parameters {
  
  vector[J] gamma1_tilde;                 // per study effect
  vector[J] gamma2_tilde;                 // per study effect
  vector[J] gamma3_tilde;                 // per study effect
  
  simplex[P] p_mis[J_mis];           // % with different entrants

  #vector[P-4] gamma1_cat_tilde;             // per study effect
  #vector[P-4] gamma2_cat_tilde;             // per study effect
  #vector[P-4] gamma3_cat_tilde;             // per study effect

  real gamma1_cat_tilde;
  real gamma2_cat_tilde;
  real gamma3_cat_tilde;
  
  
  real mu1;                               // mean effect of second entrant
  real mu2;                               // mean effect of third entrant
  real mu3;                               // mean effect of 4+ entrants

  real<lower=0> tau1;                     // deviation of effects
  real<lower=0> tau2;                     // deviation of effects
  real<lower=0> tau3;                     // deviation of effects
  
}

transformed parameters {
  vector[J] gamma;

  #vector[P-4] gamma1;
  #vector[P-4] gamma2;
  #vector[P-4] gamma3;
  real gamma1;
  real gamma2;
  real gamma3;

  vector<lower=0, upper=1>[J] p1;     // % with first entrant
  vector<lower=0, upper=1>[J] p2;     // % with second entrant
  vector<lower=0, upper=1>[J] p3;     // % with third entrant
  vector<lower=0, upper=1>[J] p4;     // % with 4 entrants
  vector<lower=0, upper=1>[J] p5;     // % with 5 entrants
  vector<lower=0, upper=1>[J] p6;     // % with 6 entrants
  vector<lower=0, upper=1>[J] p7;     // % with 7 entrants
  vector<lower=0, upper=1>[J] p8;     // % with 8 entrants
  vector<lower=0, upper=1>[J] p9;     // % with 9 entrants
  vector<lower=0, upper=1>[J] p10;     // % with 10 entrants


  #vector<lower=0>[J] N11_sim;                  // Number of entrants (4+)
  #vector<lower=0>[J] N11;                      // Number of entrants (4+)
  
  vector<lower=0, upper=1>[J_mis] p1_mis;     // % with first entrant
  vector<lower=0, upper=1>[J_mis] p2_mis;     // % with second entrant
  vector<lower=0, upper=1>[J_mis] p3_mis;     // % with third entrant
  vector<lower=0, upper=1>[J_mis] p4_mis;     // % with 4 entrants
  vector<lower=0, upper=1>[J_mis] p5_mis;     // % with 5 entrants
  vector<lower=0, upper=1>[J_mis] p6_mis;     // % with 6 entrants
  vector<lower=0, upper=1>[J_mis] p7_mis;     // % with 7 entrants
  vector<lower=0, upper=1>[J_mis] p8_mis;     // % with 8 entrants
  vector<lower=0, upper=1>[J_mis] p9_mis;     // % with 9 entrants
  vector<lower=0, upper=1>[J_mis] p10_mis;     // % with 10 entrants

  vector<lower=0>[J] weight_sim;    
  #vector<lower=0>[J] weight;     
  vector<lower=0>[J] p4_sum;     
 
  for (j in 1:J_mis){
    p1_mis[j] = p_mis[j][1];
    p2_mis[j] = p_mis[j][2];
    p3_mis[j] = p_mis[j][3];
    p4_mis[j] = p_mis[j][4];
    p5_mis[j] = p_mis[j][5];
    p6_mis[j] = p_mis[j][6];
    p7_mis[j] = p_mis[j][7];
    p8_mis[j] = p_mis[j][8];
    p9_mis[j] = p_mis[j][9];
    p10_mis[j] = p_mis[j][10];

  } 
  
  p1[ii_mis] = p1_mis;
  p2[ii_mis] = p2_mis;
  p3[ii_mis] = p3_mis;
  p4[ii_mis] = p4_mis;
  p5[ii_mis] = p5_mis;
  p6[ii_mis] = p6_mis;
  p7[ii_mis] = p7_mis;
  p8[ii_mis] = p8_mis;
  p9[ii_mis] = p9_mis;
  p10[ii_mis] = p10_mis;

  p1[ii_obs] = p_obs[1];
  p2[ii_obs] = p_obs[2];
  p3[ii_obs] = p_obs[3];
  p4[ii_obs] = p_obs[4];
  p5[ii_obs] = p_obs[5];
  p6[ii_obs] = p_obs[6];
  p7[ii_obs] = p_obs[7];
  p8[ii_obs] = p_obs[8];
  p9[ii_obs] = p_obs[9];
  p10[ii_obs] = p_obs[10];
  

  #for (j in 1:J) {
  #  N11_sim[j] = (N_mean[j] - 1*p1[j] - 2*p2[j] - 3*p3[j] - 4*p4[j] - 5*p5[j] - 6*p6[j] - 7*p7[j] - 8*p8[j] - 9*p9[j] - 10*p10[j])/(1 - (p1[j] + p2[j] + p3[j] + p4[j] + p5[j] + p6[j] + p7[j] + p8[j] + p9[j] + p10[j]));
  #}
  
  #for (j in 1:J_mis) {
  #  N11[j] = fmin(N11_sim[j], 15);
  #}
  #N11[4] = N11_sim[4];
  
    p4_sum = p4*4 + p5*5 + p6*6 + p7*7 +p8*8 + p9*9 + p10*10;

  for (j in 1:J) {
    weight_sim[j] = 1/(1 - p1[j]);
    #weight[j] = fmin(weight_sim[j], 20);
  }
  #gamma = mu1 * p2 + mu2 * p3 +  mu3 * p4_sum + tau1 * gamma1_tilde + tau2 * gamma2_tilde + tau3 * gamma3_tilde;
  
    for (j in 1:J) {
      
    #gamma[j] = mu1 * p2_weighted[j] + mu2 * p3_weighted[j] +  mu3 * (p4_weighted[j]*4 + p5_weighted[j]*5 + p6_weighted[j]*6 + p7_weighted[j]*7 +p8_weighted[j]*8 + p9_weighted[j]*9 + p10_weighted[j]*10) + tau1 * gamma1_tilde[j] + tau2 * gamma2_tilde[j] + tau3 * gamma3_tilde[j];
    #gamma[j] = mu1 * p2_weighted[j] + mu2 * p3_weighted[j] +  4*mu3 * p4_weighted[j] + tau1 * gamma1_tilde[j] + tau2 * gamma2_tilde[j] + tau3 * gamma3_tilde[j];

    #gamma[j] = (mu1 * p2[j] + mu2 * p3[j] +  mu3 * (p4[j]*4 + p5[j]*5 + p6[j]*6 + p7[j]*7 +p8[j]*8 + p9[j]*9 + p10[j]*10))/(1 - p1[j]) + tau1 * gamma1_tilde[j] + tau2 * gamma2_tilde[j] + tau3 * gamma3_tilde[j];
   #gamma[j] = mu1 * p2_weighted[j] + mu2 * p3_weighted[j] + mu3 * p4_weighted[j] + tau1 * gamma1_tilde[j] + tau2 * gamma2_tilde[j] + tau3 * gamma3_tilde[j];
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