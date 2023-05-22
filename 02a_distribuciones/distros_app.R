library(shiny)
library(hrbrthemes)
library(ggplot2)

ui <- shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    navbarPage(
      "Distributions Viz",
      tabPanel(
        "Gaussian",
        sidebarLayout(
          sidebarPanel(
            sliderInput("mean", "Mean", min = -10, max = 10, value = 0),
            sliderInput("sd", "Standard Deviation", min = 0.1, max = 5, value = 1)
          ),
          mainPanel(
            plotOutput("gaussianPlot")
          )
        )
      ),
      tabPanel(
        "t-Distribution",
        sidebarLayout(
          sidebarPanel(
            sliderInput("df", "Degrees of Freedom", min = 1, max = 30, value = 10)
          ),
          mainPanel(
            plotOutput("tPlot")
          )
        )
      ),
      tabPanel(
        "Chi-Square Distribution",
        sidebarLayout(
          sidebarPanel(
            sliderInput("df_chi", "Degrees of Freedom", min = 1, max = 30, value = 10)
          ),
          mainPanel(
            plotOutput("chiPlot")
          )
        )
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  output$gaussianPlot <- renderPlot({
    x <- seq(-10, 10, length.out = 100)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    ggplot() +
      geom_line(aes(x, y), color = "darkred") +
      labs(title = "Gaussian Distribution", x = "x", y = "PDF") +
      theme_ipsum()
  })
  
  output$tPlot <- renderPlot({
    x <- seq(-10, 10, length.out = 100)
    y <- dt(x, df = input$df)
    ggplot() +
      geom_line(aes(x, y), color = "purple") +
      labs(title = "t-Distribution", x = "x", y = "PDF") +
      theme_ipsum()
  })
  
  output$chiPlot <- renderPlot({
    x <- seq(0, 30, length.out = 100)
    y <- dchisq(x, df = input$df_chi)
    ggplot() +
      geom_line(aes(x, y), color = "darkgreen") +
      labs(title = "Chi-Square Distribution", x = "x", y = "PDF") +
      theme_ipsum()
  })
})

shinyApp(ui = ui, server = server)
