library(bayesmeta)
library(bayesplot)
library(ggplot2)
library(dplyr)
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)")
yi <- c(-0.094, -0.097, -0.053, -0.09)
sei <- c(0.008, 0.038, 0.009, 0.01)
N_mean <- c(3.62, 3.62, 3.31, 8)

df <- data.frame(publication, yi, sei, N_mean)


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


stan.dat_nobias_cat <- list(J = J, 
                 M = M,
                 beta = df$yi, 
                 sigma = df$sei,
                 N_mean = df$N_mean,
                 beta1 = df2$y1i,
                 sigma1 = df2$se1i,
                 beta2 = df2$y2i,
                 sigma2 = df2$se2i,
                 beta3 = df2$y3i,
                 sigma3 = df2$se3i)

fit0 <- stan(
  file = "Bayesmeta_nobias_noncentered_cat.stan",  # Stan program
  data = stan.dat_nobias_cat,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999)
)

plot(fit0, plotfun = "trace", pars = c("mu1", "mu2", "mu3"), inc_warmup = TRUE, nrow = 3)

fit1 <- stan(
  file = "Bayesmeta_nobias_noncentered_cat_with_parameter.stan",  # Stan program
  data = stan.dat_nobias_cat,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 7000,            # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999)
)

plot(fit1, plotfun = "trace", pars = c("mu1", "mu2", "mu3"), inc_warmup = TRUE, nrow = 3)


set.seed(123)

M <- nrow(df2)

ii_obs <- c(3, 4)
ii_mis <- c(1, 2)

p1_obs <- c(0.8, 36/40)
p2_obs <- c(0.7, 33/40)
p3_obs <- c(0.6, 29/40)

J_obs <- 4
J_mis <- 0

ii_obs <- c(1, 2, 3, 4)

p1_obs <- c(0.8, 0.8, 0.8, 36/40)
p2_obs <- c(0.7, 0.7, 0.7, 33/40)
p3_obs <- c(0.5, 0.4, 0.6, 29/40)


stan.dat_nobias_cat_with_NA <- list(J_obs = J_obs, 
                            J_mis = J_mis,
                            ii_obs = ii_obs,
                            ii_mis = ii_mis,
                            M = M,
                            beta = df$yi, 
                            sigma = df$sei,
                            N_mean = df$N_mean,
                            p1_obs = p1_obs,
                            p2_obs = p2_obs,
                            p3_obs = p3_obs,
                            beta1 = df2$y1i,
                            sigma1 = df2$se1i,
                            beta2 = df2$y2i,
                            sigma2 = df2$se2i,
                            beta3 = df2$y3i,
                            sigma3 = df2$se3i)

fit2 <- stan(
  file = "Bayesmeta_nobias_noncentered_cat_with_NA.stan",  # Stan program
  data = stan.dat_nobias_cat_with_NA,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 7000,            # total number of iterations per chain
  #cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999)
)

plot(fit2, plotfun = "trace", pars = c("mu1", "mu2", "mu3"), inc_warmup = TRUE, nrow = 3)


J_obs <- 1
J_mis <- nrow(df) - J_obs

ii_obs <- c(4)
ii_mis <- c(1, 2, 3)

stan.dat_nobias_cat_with_3NA <- list(J_obs = J_obs, 
                                    J_mis = J_mis,
                                    ii_obs = ii_obs,
                                    ii_mis = ii_mis,
                                    M = M,
                                    beta = df$yi, 
                                    sigma = df$sei,
                                    N_mean = df$N_mean,
                                    p1_obs = p1_obs,
                                    p2_obs = p2_obs,
                                    p3_obs = p3_obs,
                                    beta1 = df2$y1i,
                                    sigma1 = df2$se1i,
                                    beta2 = df2$y2i,
                                    sigma2 = df2$se2i,
                                    beta3 = df2$y3i,
                                    sigma3 = df2$se3i)

fit3 <- stan(
  file = "Bayesmeta_nobias_noncentered_cat_with_3NA.stan",  # Stan program
  data = stan.dat_nobias_cat_with_3NA,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 120000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.9999,
                 max_treedepth = 20)
)

plot(fit3, plotfun = "trace", pars = c("mu1", "mu2", "mu3"), inc_warmup = TRUE, nrow = 3)



p1_obs <- c(36/40)
p2_obs <- c(33/40)
p3_obs <- c(29/40)
