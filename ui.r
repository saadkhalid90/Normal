library(shiny)

shinyUI(fluidPage(
  h3("Unlimited Test Bank - Normal Distribution"),
  hr(),
  numericInput("Prob", label = "Probability: ", value = 0.5, min = 0, max = 1, step = 0.05,
               width = NULL),
  actionButton("answer", label = "Answer"),
  actionButton("action", label = "Next Problem"),
  hr(),
  h4(textOutput("ci")),
  h5(textOutput("cor")),
  h5(textOutput("incor")),
  mainPanel(
    plotOutput("NormalPlot")
  )
))
