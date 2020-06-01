N <- seq(0, 20, 1)
sim <- data.frame(N)
sim <- sim %>% mutate(y_hat = 100*(exp(-0.07*N)))
Tenn <- c(1, -0.091, -0.087, -0.109, -0.221, -0.295, -0.362, -0.406, -0.455, -0.581, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819, -0.819)
sim <- sim %>%
  cbind(Tenn) %>%
  mutate(Tenn_noAG = 100*exp(Tenn),
          Tenn_AG = 100*exp(Tenn + 0.514 - 0.109*N + 0.005*N^2))

row1 <- sim %>% 
  select(1:2) %>%
  rename(y = y_hat) %>%
  mutate(group = "BMA")

row2 <- sim %>% 
  select(1, 4) %>%
  rename(y = Tenn_noAG) %>%
  mutate(group = "Tenn_noAG")

row3 <- sim %>% 
  select(1, 5) %>%
  rename(y = Tenn_AG) %>%
  mutate(group = "Tenn_AG")

load("~/Dropbox/Advanced Method Project/Data/price_ratio.Rdata")

price_ratio <- price_ratio %>% 
  filter(N != 0) %>%
  mutate(group = "MEPS_price ratio_branded")

load("~/Dropbox/Advanced Method Project/Data/price_ratio_base.Rdata")

price_ratio_base <- price_ratio_base %>% 
  filter(N != 0) 

load("~/Dropbox/Advanced Method Project/Data/price_ratio_prior_LOE.Rdata")
price_ratio_prior_LOE <- price_ratio_prior_LOE %>% 
  filter(N != 0) 

sim_new <- row1 %>%
  rbind(row2) %>%
  rbind(row3) %>%
  #rbind(price_ratio) %>%
  rbind(price_ratio_base) %>%
  rbind(price_ratio_prior_LOE)

ggplot(sim_new, aes(x = N, y = y, color = group)) + 
  geom_line() + 
  xlab('# of competitors') + 
  ylab("% price of original")


N <- seq(1, 10, 1)
sim <- data.frame(N)
sim <- sim %>% mutate(sim_cts = 100*(exp(-0.058*N)))
sim_cat <- c(NA, 0.0842, 0.075, 0.117, 0.144, 0.170, 0.195, 0.220, 0.243, 0.266)
sim <- sim %>%
  cbind(sim_cat) %>%
  mutate(sim_cat = 100 - 100*sim_cat)

row1 <- sim %>% 
  select(1:2) %>%
  rename(y = sim_cts) %>%
  mutate(group = "BMA_CTS_N")

row2 <- sim %>% 
  select(1, 3) %>%
  rename(y = sim_cat) %>%
  mutate(group = "BMA_CAT_N")

sim_new <- row1 %>%
  rbind(row2) %>%
  rbind(price_ratio_prior_LOE) %>%
  filter(N <= 10)

ggplot(sim_new, aes(x = N, y = y, color = group)) + 
  geom_line() + 
  xlab('# of competitors') + 
  ylab("% price of original")
