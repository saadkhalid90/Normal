library(shiny)

sd_bound <- signif(sort(runif(n=2,min=-5,max=5)), 2)
mean <- round(runif(n = 1, min = 0, max = 100), digits = 1)
# mean <- round(runif(n = 1, min = -100, max = 100), digits = 1) - Changed the range on the mean from -100 to 0
sd <- round(runif(n = 1, min = 1, max = 20), digits = 0)
# sd_bound <- signif(sort(runif(n = 2, min = -4, max = 4)), 3) - Replaced above to make sure the z score is in Table A
bounds <- mean + (sd_bound * sd)

normal_plot <- function(mean, sd, lb, ub) {
  ## finding the x and y coordinates for plotting
  x <- seq(-4,4,length=1000)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  ## plot
  plot(x, hx, type="n", xlab="X", ylab="", axes=FALSE)
  # plot(x, hx, type="n", xlab="R.V", ylab="", axes=FALSE) - Changed R.V. to X throughout

  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(1,0,0, alpha = 0.75)) 
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  # Changed the text above the graph a bit and removed the answer
  rv_text <- paste("X has Normal distribution")
  mean_txt <- paste("with Mean: ", mean)
  sd_text <- paste("and S.D: ", sd)
  result <- paste(rv_text, "\n", mean_txt, "\n", sd_text, "\n", "What is P(",lb,"< X <",ub,")?")
  mtext(result,3)
  axis(1, at=seq(mean - (4 * sd), mean + ( 4 * sd), sd), pos=0)
}

# mean <- round(runif(n = 1, min = -100, max = 100), digits = 1)
# sd <- round(runif(n = 1, min = 1, max = 20), digits = 0)
# sd_bound <- signif(sort(runif(n = 2, min = -4, max = 4)), 3)
# bounds <- mean + (sd_bound * sd)

shinyServer(function(input, output, session){
  v <- reactiveValues(cor_inc = "", 
                      mean = mean, 
                      sd = sd, 
                      sd_bound = sd_bound, 
                      bounds = bounds,
                      correct = 0,
                      incorrect = 0,
                      question_count = 0,
                      test_area = 0)

  observeEvent(input$action, {
    # This is changed as above, at the beginning
    v$sd_bound <- signif(sort(runif(n=2, min=-5, max=5)), 2)
    v$mean <- round(runif(n = 1, min = 0, max = 100), digits = 1)
    v$sd <- round(runif(n = 1, min = 1, max = 20), digits = 0)
    v$bounds <- v$mean + (v$sd_bound * v$sd)
    v$cor_inc <- ""
  })
  
  observeEvent(input$answer, {
    mean <- v$mean
    sd <- v$sd
    lb <- v$bounds[1]
    ub <- v$bounds[2]
    area <- signif(pnorm(ub, mean, sd) - pnorm(lb, mean, sd), 3)
    if (area >= input$Prob - 0.005 & area <= input$Prob + 0.005){
      v$cor_inc <- "Correct!"
      if (area != v$test_area){
        v$correct <- v$correct + 1
      }
    }
    else{
      v$cor_inc <- "Incorrect! Please try again"
      if (area != v$test_area){
        v$incorrect <- v$incorrect + 1
      }
    }
    v$test_area <- area
  })

  output$NormalPlot <- renderPlot(normal_plot(v$mean, v$sd, v$bounds[1], v$bounds[2]))
  output$ci <- renderText(v$cor_inc)
  output$cor <- renderText(paste("Correct: ", v$correct))
  output$incor <- renderText(paste("Incorrect: ", v$incorrect))
})
