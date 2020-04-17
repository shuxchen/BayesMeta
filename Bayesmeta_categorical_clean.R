library(bayesmeta)
library(bayesplot)
library(ggplot2)
library(dplyr)
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Four studies with CTS N
publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)")
yi <- c(-0.094, -0.097, -0.053, -0.09)
sei <- c(0.008, 0.038, 0.009, 0.01)
N_mean <- c(3.62, 3.62, 3.31, 8)

df <- data.frame(publication, yi, sei, N_mean)

# One study with categorical N
publication2 <- rep("Tenn (2014)", 7)
y1i <- rep(-0.091, 7)
se1i <- rep(0.035, 7)
y2i <- rep(-0.087, 7)
se2i <- rep(0.056, 7)

y3i <- c(-0.109/4, -0.221/5, -0.295/6, -0.362/7, -0.406/8, -0.455/9, -0.581/10)
se3i <- c(0.065/4, 0.077/5, 0.082/6, 0.108/7, 0.126/8, 0.152/9, 0.246/10)

publication2 <- c("Tenn (2014)")
y1i <- c(-0.091)
se1i <- c(0.035)
y2i <- c(-0.087)
se2i <- c(0.056)
y3i <- c(-0.455)
se3i <- c(0.152)


df2 <- data.frame(publication2, y1i, se1i, y2i, se2i, y3i, se3i)

set.seed(123)
J <- nrow(df)
M <- nrow(df2)

# Specify which studies have % of entrants missing (among 4 studies with CTS N)
J_obs <- 1
J_mis <- nrow(df) - J_obs

ii_obs <- c(4)
ii_mis <- c(1, 2, 3)

# Dirichlet prior for missing % (from MEPS)
alpha = c(0.140, 0.143, 0.177, 0.083, 0.081, 0.075, 0.108, 0.063, 0.048, 0.123, 0.130)

# % from one study where % of entrants is not missing
p_obs = c(4/40, 3/40, 4/40, 3/40, 4/40, 4/40, 1/40, 2/40, 3/40, 2/40, 10/40)


## Combine as data input
stan.dat_nobias_cat <- list(J_obs = J_obs, 
                                     J_mis = J_mis,
                                     ii_obs = ii_obs,
                                     ii_mis = ii_mis,
                                     beta = df$yi, 
                                     sigma = df$sei,
                                     N_mean = df$N_mean,
                                     P = 11, # number of entrant groups 
                                     p_obs = p_obs,
                                     alpha = alpha,
                                     #M = 1,
                                     beta1 = df2$y1i,
                                     sigma1 = df2$se1i,
                                     beta2 = df2$y2i,
                                     sigma2 = df2$se2i,
                                     beta3 = df2$y3i,
                                     sigma3 = df2$se3i)

fit <- stan(
  file = "Bayesmeta_nobias_noncentered_cat_with_3NA.stan",  # Stan program
  data = stan.dat_nobias_cat,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999,
                 max_treedepth = 20)
)

plot(fit, plotfun = "trace", pars = c("mu1", "mu2", "mu3"), inc_warmup = TRUE, nrow = 3)

plot(fit, plotfun = "trace", pars = c("weight_sim[1]", "weight_sim[2]", "weight_sim[3]", "weight_sim[4]"), inc_warmup = TRUE, nrow = 4)

print(fit)