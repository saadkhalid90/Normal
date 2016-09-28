library(shiny)

shinyUI(fluidPage(
  h3("Normal Distribution Practice"),
  hr(),
  numericInput("Prob", label = "Probability: ", value = 0.5, min = 0, max = 1, step = 0.05,
               width = NULL),
  actionButton("answer", label = "Answer"),
  actionButton("action", label = "Next Problem"),
  hr(),
  h4(textOutput("ci")),
  mainPanel(
    plotOutput("NormalPlot")
  )
))
