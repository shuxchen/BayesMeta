#install.packages("bayesmeta")

library(bayesmeta)

publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Saha (2006)", "Grabowski (2007)", "Regan (2008)")
yi <- c(-0.094, -0.073, -0.053, -0.0013, -0.09, 0.0023)
sei <- c(0.008, 0.019, 0.009, 0.0052, 0.01, 0.0018)

#publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)", "Regan (2008)")
#yi <- c(-0.094, -0.073, -0.053, -0.09, -0.0011)
#sei <- c(0.008, 0.019, 0.009, 0.01, 0.0038)


df <- data.frame(publication, yi, sei)

ma01 <- bayesmeta(y = df[,"yi"], 
                  sigma = df[,"sei"],
                  labels = df[,"publication"], 
                  mu.prior.mean = 0, 
                  mu.prior.sd = 100,
                  tau.prior = function(t){dhalfnormal(t,scale=1)})
ma01

forestplot(ma01)

ma01$summary

plot(ma01)

log()

