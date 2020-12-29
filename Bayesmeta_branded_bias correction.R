publication <- c("Dafny (2016)", "Frank (1995)", "Frank (1995)", "Regan (2008)", "Regan (2008)")
yi <- c(0.012, 0.007, 0.016, 0.0104, 0.0199)
sei <- c(0.006, 0.0031, 0.0037, 0.0016, 0.0033)


bias_IV <- c(1, 1, 0, 1, 0)
bias_unit <- c(0, 1, 1, 1, 1)

L1 <- c(1, 0, 0, 0, 0) 
L2 <- c(0, 1, 1, 0, 0) 
L3 <- c(0, 0, 0, 1, 1) 


L_matrix <- data.frame(L1, L2, L3)


df <- data.frame(publication, yi, sei, bias_IV, bias_unit)

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
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 6000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 0,            # no progress shown
  control = list(adapt_delta = 0.999,
                 max_treedepth = 20)
)

print(fit3)
print(summary(fit3),digits=5)