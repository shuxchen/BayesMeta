publication <- c("Dafny (2016)", "Frank (1995)", "Regan (2008)")
yi <- c(0.012, 0.007, 0.0104)
sei <- c(0.006, 0.0031, 0.0016)
N <- c(1740, 343, 339)

sd(sei)
#use pooled se
#pooled SE
pooled_se <- sqrt(((0.006^2)*1740 + (0.0031^2)*343 + (0.0016^2)*339)/(1740 + 343 + 339))
pooled_se  #0.005251875


df <- data.frame(publication, yi, sei)

ma02 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 10,
                  tau.prior = function(t){dhalfcauchy(t,scale=0.0025)})

ma02 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 10,
                  tau.prior = function(t){dhalfnormal(t,scale=0.05)})

ma02 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 10,
                  tau.prior = "uniform") ##how to bound it?

forestplot(ma02)

