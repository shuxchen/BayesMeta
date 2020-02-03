publication <- c("Dafny (2016)", "Frank (1995)", "Regan (2008)")
yi <- c(0.012, 0.007, 0.0104)
sei <- c(0.006, 0.0031, 0.0016)

sd(sei)

df <- data.frame(publication, yi, sei)

ma02 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 10,
                  tau.prior = function(t){dhalfcauchy(t,scale=0.05)})

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
                  tau.prior = "uniform")

forestplot(ma02)

