library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Forest plot"),

    # Sidebar with a slider input for priors
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu_prior_mean",
                        "Prior of mu's mean:",
                        min = -1,
                        max = 0,
                        value = -0.1),
            
            sliderInput("mu_prior_sd",
                        "Prior of mu's sd:",
                        min = 0,
                        max = 1,
                        value = 0.1),
            
            sliderInput("tau_prior",
                        "Prior of tau:",
                        min = 0,
                        max = 1,
                        value = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Half Cauchy prior for tau"),
            plotOutput("forestPlot1"),
            h3("Half Normal prior for tau"),
            plotOutput("forestPlot2")
            
        )
    )
))
