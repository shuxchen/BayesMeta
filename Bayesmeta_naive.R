#install.packages("bayesmeta")
#install.packages("bayesplot")

library(bayesmeta)
library(bayesplot)
library(ggplot2)
library(dplyr)

publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)")
yi <- c(-0.094, -0.097, -0.053, -0.09)
sei <- c(0.008, 0.038, 0.009, 0.01)
N <- c(1740, 154, 9648, 40)

#SA including Regan's study
publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)", "Regan (2008)")
yi <- c(-0.094, -0.073, -0.053, -0.09, -0.0012)
sei <- c(0.008, 0.019, 0.009, 0.01, 0.0039)
df <- data.frame(publication, yi, sei)
bias_IV <- c(1, 0, 1, 1, 0)
bias_pb <- c(0, 0, 1, 0, 0)
bias_unit <- c(0, 1, 1, 1, 1)


sd(sei)

bias_IV <- c(1, 0, 1, 1)
bias_t <- c(0, 0, 1, 0)
bias_pb <- c(0, 0, 1, 0)
bias_unit <- c(0, 1, 1, 1)
bias_class <- c(1, 1, 0, 0)
bias_size <- c(1, 1, 0, 1)

df <- data.frame(publication, yi, sei, bias_IV, bias_pb, bias_unit)
df <- data.frame(publication, yi, sei, bias_IV, bias_pb)

      
ma01 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 10,
                  tau.prior = function(t){dhalfcauchy(t,scale=0.005)})


ma01 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  #mu.prior.mean = -0.1, 
                  #mu.prior.sd = 0.1,
                  tau.prior = "Jeffreys")

ma01

forestplot(ma01)

ma01$summary
ma01$summary[,"mu"]
posterior_mean = ma01$summary[2,"mu"]
posterior_LB = ma01$summary[4,"mu"]
posterior_UB = ma01$summary[5,"mu"]


plot(ma01)

log()


#tests
hn005 <- normalmixture(cdf=function(t){phalfnormal(t, scale=0.05)})
hc005 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.05)})
hn01 <- normalmixture(cdf=function(t){phalfnormal(t, scale=0.1)})
hc01 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.1)})
hn001 <- normalmixture(cdf=function(t){phalfnormal(t, scale=0.01)})
hc001 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.01)})

hc005 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.005)})
hc0025 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.0025)})
hc0001 <- normalmixture(cdf=function(t){phalfcauchy(t, scale=0.0001)})


q975 <- c(
          "half-normal(0.1)" = hn01$quantile(0.975),
          "half-normal(0.05)" = hn005$quantile(0.975),
          "half-normal(0.01)" = hn001$quantile(0.975),
          "half-Cauchy(0.1)" = hc01$quantile(0.975),
          "half-Cauchy(0.05)" = hc005$quantile(0.975),
          "half-Cauchy(0.01)" = hc001$quantile(0.975))
print(cbind("tau"=q975))

q50 <- c(
  "half-normal(0.1)" = hn01$quantile(0.5),
  "half-normal(0.05)" = hn005$quantile(0.5),
  "half-normal(0.01)" = hn001$quantile(0.5),
  "half-Cauchy(0.1)" = hc01$quantile(0.5),
  "half-Cauchy(0.05)" = hc005$quantile(0.5),
  "half-Cauchy(0.01)" = hc001$quantile(0.5))
print(cbind("tau"=q50))

#mean and variance of gamma (posterior)
posterior_gamma_mean <- function(mean_hyperprior = 0,
                                 sd_hyperprior = 10,
                                 tau_hyperprior = 1,
                                 data = df){
  #' Produces the conjugate posterior of mean from a NNHM
  #' 
  #' Args:
  #' @param mean_hyperprior 
  #' @param sd_hyperprior 
  #' @param tau_hyperprior 
  #' @param data dataframe of data 
  #' 
  #' Returns the posterior mean for mu (gamma)
  
  J = length(data$yi)
  sum_denominator <- 0
  sum_numerator <- 0
  for (i in 1:J){
    sum_denominator =  sum_denominator + data$yi[i]/((data$sei[i])^2 + (tau_hyperprior)^2)
    sum_numerator =  sum_numerator + 1/((data$sei[i])^2 + (tau_hyperprior)^2)
    
  }
  mean <- (mean_hyperprior/(sd_hyperprior)^2 + sum_denominator)/(1/(sd_hyperprior)^2 + sum_numerator)
  return(mean)
}

#plot posterior mean as a function of tau's value 
tau_sim <-  seq(0, 1, by = 0.01)
mean_sim <- posterior_gamma_mean(tau = tau_sim)
df_mean_sim <- data.frame(tau_sim, mean_sim)
ggplot(df_mean_sim, aes(x = tau_sim, y = mean_sim)) +
  geom_point()


posterior_gamma_sd <- function(mean_hyperprior = 0,
                                     sd_hyperprior = 10,
                                     tau_hyperprior = 1,
                                     data = df){
  #' Produces the conjugate posterior of mean's variance from a NNHM
  #' 
  #' Args:
  #' @param mean_hyperprior 
  #' @param sd_hyperprior 
  #' @param tau_hyperprior 
  #' @param data dataframe of data 
  #' 
  #' Returns the posterior mu (gamma)'s standard deviation
  
  J = length(data$yi)
  sum_numerator <- 0
  for (i in 1:J){
    sum_numerator =  sum_numerator + 1/((data$sei[i])^2 + (tau_hyperprior)^2)
    
  }
  variance <- 1/(1/(sd_hyperprior)^2 + sum_numerator)
  sd <- sqrt(variance)
  return(sd)
}

#plot posterior sd as a function of tau's value 
tau_sim <-  seq(0, 1, by = 0.01)

mean_10_sim <- posterior_gamma_mean(tau = tau_sim, sd_hyperprior = 10)
mean_5_sim <- posterior_gamma_mean(tau = tau_sim, sd_hyperprior = 5)
mean_1_sim <- posterior_gamma_mean(tau = tau_sim, sd_hyperprior = 1)
mean_0.1_sim <- posterior_gamma_mean(tau = tau_sim, sd_hyperprior = 0.1)


sd_10_sim <- posterior_gamma_sd(tau = tau_sim, sd_hyperprior = 10)
sd_5_sim <- posterior_gamma_sd(tau = tau_sim, sd_hyperprior = 5)
sd_1_sim <- posterior_gamma_sd(tau = tau_sim, sd_hyperprior = 1)
sd_0.1_sim <- posterior_gamma_sd(tau = tau_sim, sd_hyperprior = 0.1)

#reshape 
sd_sim <- sd_10_sim %>%
  append(sd_1_sim) %>%
  append(sd_0.1_sim)

mean_sim <- mean_10_sim %>%
  append(mean_1_sim) %>%
  append(mean_0.1_sim)

sd_hyperprior <- rep("10", 101) %>%
  append(rep("1", 101)) %>%
  append(rep("0.1", 101)) 

tau_sim <- tau_sim %>%
  append(tau_sim) %>%
  append(tau_sim) 
  
df_sd_sim <- data.frame(tau_sim, mean_sim, sd_sim, sd_hyperprior)

df_sd_sim <- df_sd_sim %>%
  mutate(LB = mean_sim - 1.96*sd_sim,
         UB = mean_sim + 1.96*sd_sim)

ggplot(df_sd_sim, aes(x = tau_sim, y = mean_sim, color = sd_hyperprior)) + 
  geom_errorbar(aes(ymin=LB, ymax=UB), width=.01) +
  geom_line() +
  geom_point()
