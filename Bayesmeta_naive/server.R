library(bayesmeta)
library(bayesplot)


shinyServer(function(input, output) {

    publication <- c("Dafny (2016)", "Frank (1995)", "Helland (2016)", "Grabowski (2007)")
    yi <- c(-0.094, -0.097, -0.053, -0.09)
    sei <- c(0.008, 0.038, 0.009, 0.01)
    df <- data.frame(publication, yi, sei)
  
    output$forestPlot1 <- renderPlot({

        ma01 <- bayesmeta(y = df[,"yi"], 
                          sigma = df[,"sei"],
                          labels = df[,"publication"], 
                          mu.prior.mean = input$mu_prior_mean, 
                          mu.prior.sd = input$mu_prior_sd,
                          tau.prior = function(t){dhalfcauchy(t,scale=input$tau_prior)})
        
        forestplot(ma01)
        
    })
    
    output$forestPlot2 <- renderPlot({
      
      ma02 <- bayesmeta(y = df[,"yi"], 
                        sigma = df[,"sei"],
                        labels = df[,"publication"], 
                        mu.prior.mean = input$mu_prior_mean, 
                        mu.prior.sd = input$mu_prior_sd,
                        tau.prior = function(t){dhalfnormal(t,scale=input$tau_prior)})
      
      forestplot(ma02)
      
    })

})
