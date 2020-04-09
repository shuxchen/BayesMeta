publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Helland (2016)", "Grabowski (2007)")
yi <- c(-0.094, -0.097, -0.053, -0.108,  -0.09)
sei <- c(0.008, 0.038, 0.009, 0.031, 0.01)
N <- c(1740, 154, 9648, 9648, 40)

sd(sei)

bias_IV <- c(1, 0, 1, 0, 1)
bias_t <- c(0, 0, 1, 1, 0)
bias_pb <- c(0, 0, 1, 1, 1)
bias_unit <- c(0, 1, 1, 1, 1)
bias_class <- c(1, 1, 0, 0, 0)
bias_size <- c(1, 1, 0, 0, 1)

L1 <- c(1, 0, 0, 0, 0) 
L2 <- c(0, 1, 0, 0, 0) 
L3 <- c(0, 0, 1, 1, 0) 
L4 <- c(0, 0, 0, 0, 1) 
L_matrix <- data.frame(L1, L2, L3, L4)


df <- data.frame(publication, yi, sei, bias_IV, bias_pb, bias_unit)

set.seed(123)
L <- nrow(df) 
J <- df %>% distinct(publication) %>% nrow()
K <- ncol(df) - 3
stan.dat <- list(L = L,
                 J = J,
                 K = K,
                 beta = df$yi, 
                 sigma = df$sei,
                 I_bias = df[, 4:(K+3)],
                 L_matrix = L_matrix)

fit3 <- stan(
  file = "Bayesmeta_bias_vector_noncentered_multiple.stan",  # Stan program
  data = stan.dat,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.99)
)

fit3_sim <- extract(fit3)

#posterior predictive simulation
n_sims <- length(fit3_sim$lp__)
beta_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims) {
  beta_rep[s,] <- rnorm(J, fit3_sim$theta[s,], df$sei)
}

##Replicated data in new study
gamma_rep <- array(NA, c(n_sims, J))
bias1_rep <- array(NA, c(n_sims, J))
bias2_rep <- array(NA, c(n_sims, J))
bias3_rep <- array(NA, c(n_sims, J))
theta_rep <- array(NA, c(n_sims, J))
beta_rep <- array(NA, c(n_sims, J))

#If biases 1 and 2 are present for future study 
for (s in 1:n_sims){
  gamma_rep[s,] <- rnorm(J, fit3_sim$mu[s], fit3_sim$tau[s])
  
  bias1_rep[s,] <- fit3_sim$bias[s,1]
  bias2_rep[s,] <- fit3_sim$bias[s,2] 
  bias3_rep[s,] <- fit3_sim$bias[s,3] 
  
  theta_rep[s,] <-  gamma_rep[s,] + bias1_rep[s,] + bias2_rep[s,]
  
  beta_rep[s,] <- rnorm(J, theta_rep[s,], df$sei)
}

hist(beta_rep[,1])
hist(beta_rep[,2])
hist(beta_rep[,3])
hist(beta_rep[,4])

median(beta_rep)

quantile(beta_rep, probs = c(.025, .975))
