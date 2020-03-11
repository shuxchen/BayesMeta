library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


set.seed(123)
J <- nrow(df)
K <- ncol(df) - 3
stan.dat <- list(J = J, 
                 K = K,
                 beta = df$yi, 
                 sigma = df$sei,
                 I_bias = df[, 4:(K+3)])

stan.dat_nobias <- list(J = J, 
                  beta = df$yi, 
                  sigma = df$sei)

fit0 <- stan(
  file = "Bayesmeta_nobias_noncentered.stan",  # Stan program
  data = stan.dat_nobias,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999)
)

fit0_sim <- extract(fit0)
str(fit0_sim)
#posterior predictive simulation
n_sims <- length(fit0_sim$lp__)
beta_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims) {
  beta_rep[s,] <- rnorm(J, fit0_sim$gamma[s,], df$sei)
}
##Replicated data in new study
theta_rep <- array(NA, c(n_sims, J))
beta_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims){
  theta_rep[s,] <- rnorm(J, fit0_sim$mu[s], fit0_sim$tau[s])
  beta_rep[s,] <- rnorm(J, theta_rep[s,], df$sei)
}

median(beta_rep)
quantile(beta_rep, probs = c(.025, .975))
quantile(beta_rep, probs = c(.25, .75))

#use pooled se
#pooled SE
pooled_se <- sqrt(((0.008^2)*1740 + (0.038^2)*154 + (0.009^2)*9648 + (0.01^2)*40)/(1740 + 154 + 9648 + 40))
pooled_se  #0.009830299

for (s in 1:n_sims){
  theta_rep[s,] <- rnorm(J, fit0_sim$mu[s], fit0_sim$tau[s])
  beta_rep[s,] <- rnorm(J, theta_rep[s,], pooled_se)
}
median(beta_rep)
quantile(beta_rep, probs = c(.025, .975))



fit1 <- stan(
  file = "Bayesmeta_bias_vector.stan",  # Stan program
  data = stan.dat,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999,
                 max_treedepth = 20)
)

fit2 <- stan(
  file = "Bayesmeta_bias_vector_noncentered.stan",  # Stan program
  data = stan.dat,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.99)
)


pairs(fit1)
print(fit1)
plot(fit1)
plot(fit1, plotfun = "trace", pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)
plot(fit2, plotfun = "trace", pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)


print(fit2,"mu",probs=c(.025,.975))
print(fit2,"tau",probs=c(.025,.975))

fit2_sim <- extract(fit2)
hist(fit2_sim$mu)
hist(fit2_sim$tau)

#posterior predictive simulation
n_sims <- length(fit2_sim$lp__)
beta_rep <- array(NA, c(n_sims, J))
for (s in 1:n_sims) {
  beta_rep[s,] <- rnorm(J, fit2_sim$theta[s,], df$sei)
}

par(mfrow=c(5,4), mar=c(4,4,2,2))
hist(df$yi, xlab="", main="yi")
for(s in 1:19)
  hist(beta_rep[s,], xlab="", main=paste("beta_rep",s))

#test statistics
test <- function(y){
  y_sort <- rev(sort(y))
  return(y_sort[1] - y_sort[2])
}
t_y <- test(df$yi)
t_rep <- rep(NA, n_sims)
for(s in 1:n_sims){
  t_rep[s] <- test(beta_rep[s,])
}

par(mfrow=c(1,1))
cat("T(y) =", round(t_y,1), " and T(beta_rep) has mean",
    round(mean(t_rep),1), "and sd", round(sd(t_rep),1),
    "\nPr (T(beta_rep) > T(beta)) =", round(mean(t_rep>t_y),2), "\n")
hist0 <- hist(t_rep, xlim=range(t_y,t_rep), xlab="T(beta_rep)")
lines(rep(t_y,2), c(0,1e6))
text(t_y, .9*max(hist0$count), "T(y)", adj=0)

##Replicated data in new study
gamma_rep <- array(NA, c(n_sims, J))
bias1_rep <- array(NA, c(n_sims, J))
bias2_rep <- array(NA, c(n_sims, J))
bias3_rep <- array(NA, c(n_sims, J))
theta_rep <- array(NA, c(n_sims, J))
beta_rep <- array(NA, c(n_sims, J))

#If biases 1 and 2 are present for future study 
for (s in 1:n_sims){
  gamma_rep[s,] <- rnorm(J, fit2_sim$mu[s], fit2_sim$tau[s])
  
  bias1_rep[s,] <- fit2_sim$bias[s,1]
  bias2_rep[s,] <- fit2_sim$bias[s,2] 
  bias3_rep[s,] <- fit2_sim$bias[s,3] 
  
  theta_rep[s,] <-  gamma_rep[s,] + bias1_rep[s,] + bias2_rep[s,]
  
  beta_rep[s,] <- rnorm(J, theta_rep[s,], df$sei)
}

hist(beta_rep[,1])
hist(beta_rep[,2])
hist(beta_rep[,3])
hist(beta_rep[,4])

median(beta_rep)

quantile(beta_rep, probs = c(.025, .975))


for (s in 1:n_sims){
  gamma_rep[s,] <- rnorm(J, fit2_sim$mu[s], fit2_sim$tau[s])
  
  bias1_rep[s,] <- fit2_sim$bias[s,1]
  bias2_rep[s,] <- fit2_sim$bias[s,2] 
  bias3_rep[s,] <- fit2_sim$bias[s,3] 
  
  theta_rep[s,] <-  gamma_rep[s,] + bias1_rep[s,] + bias3_rep[s,]
  
  beta_rep[s,] <- rnorm(J, theta_rep[s,], pooled_se)
}

median(beta_rep)

quantile(beta_rep, probs = c(.025, .975))


##plot diagnosis
posterior_fit1 <- as.array(fit1)
posterior_fit2 <- as.array(fit2)
np_fit1 <- nuts_params(fit1)
np_fit2 <- nuts_params(fit2)

#divergent transitions
color_scheme_set("darkgray")
mcmc_parcoord(posterior_fit1, np = np_fit1)
mcmc_parcoord(posterior_fit2, np = np_fit2)

scatter_theta_fit2 <- mcmc_scatter(
  posterior_fit2,
  pars = c("theta[1]", "tau"),
  transform = list(tau = "log"), # can abbrev. 'transformations'
  np = np_fit2,
  size = 1
)
scatter_theta_fit2

scatter_gamma_fit1 <- mcmc_scatter(
  posterior_fit1,
  pars = c("gamma[1]", "tau"),
  transform = list(tau = "log"), # can abbrev. 'transformations'
  np = np_fit1,
  size = 1
) 
scatter_gamma_fit1 + ylim(-15, 0)

scatter_gamma_fit2 <- mcmc_scatter(
  posterior_fit2,
  pars = c("gamma[1]", "tau"),
  transform = list(tau = "log"), # can abbrev. 'transformations'
  np = np_fit2,
  size = 1
) 
scatter_gamma_fit2 + ylim(-15, 0)

scatter_gamma_fit2 <- mcmc_scatter(
  posterior_fit2,
  pars = c("gamma_tilde[1]", "tau"),
  transform = list(tau = "log"), # can abbrev. 'transformations'
  np = np_fit2,
  size = 1
)
scatter_gamma_tilde_fit2

#traceplot with divergence
color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_fit1, pars = "tau", np = np_fit1) +
  xlab("Post-warmup iteration")

mcmc_trace(posterior_fit2, pars = "tau", np = np_fit2) +
  xlab("Post-warmup iteration")

#effective sample size
mcmc_neff(neff_ratio(fit1))

mcmc_neff(neff_ratio(fit2))

#autocorrelation
mcmc_acf(fit1, pars = "mu", lags = 10)
mcmc_acf(fit1, pars = "tau", lags = 10)
mcmc_acf(fit1, pars = "theta[1]", lags = 10)
mcmc_acf(fit1, pars = "gamma[1]", lags = 10)

mcmc_acf(fit2, pars = "mu", lags = 10)
mcmc_acf(fit2, pars = "tau", lags = 10)
mcmc_acf(fit2, pars = "theta[1]", lags = 10)
mcmc_acf(fit2, pars = "gamma[1]", lags = 10)





