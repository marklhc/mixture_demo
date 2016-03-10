library(shiny)
library(mixtools)

ui <- fluidPage(
  titlePanel("Simple Demonstration of Univariate Normal Mixture"),
  fluidRow(
    column(width = 12, 
           wellPanel(
             p("In this app, there are two variables from two different data
               sets."), 
             p("You may specify the number of clusters and click the 
               'Iterate!' button to see how the EM algorithm proceeds."),
             p("The tables below the distribution showed the estimated
               mixing proportion as well as the means and SDs for each 
               latent class after each iterations. The colored solid lines 
               represent the normal curve for each class, and the dashed black
               line represents the combined density."), 
             p("Try running the EM alogrithm multiple times, and you will
               find that sometimes the algorithm will converge to different
               solutions!")
           ))
  ), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_clus", label = "Number of clusters:",
                  min = 2, max = 4, value = 2, step = 1, ticks = FALSE), 
      checkboxInput("sd_equal", "Equal SDs across classes", value = FALSE), 
      selectInput("n_iter", "Number of iterations", list(1, 10, 100)),
      actionButton("iter", "Iterate!"), 
      br(), 
      br(),
      h5("By Mark Lai")
    ), 
    mainPanel(
      h4("waiting comes from the `faithful` data set"), 
      h4("CO2 comes from the `CO2data` data set"), 
      tabsetPanel(id = "data", 
                  tabPanel("waiting"), 
                  tabPanel("CO2")), 
      plotOutput("plot"),
      textOutput("loglik"),
      br(), 
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  # Density function of normal mixture
  dnormmix <- function(x, lambda, mu, sigma) {
    as.vector(lambda %*% 
                sapply(x, dnorm, mean = mu, sd = sigma))
  }
  v <- reactiveValues(lambda0 = NULL, mu0 = NULL, sigma0 = NULL, 
                       initialized = FALSE, loglik = NA)
  observeEvent(input$iter, {
    v$initialized <- TRUE
  })
  observeEvent(input$n_clus, {
    v$initialized <- FALSE
  })
  observeEvent(input$data, {
    v$initialized <- FALSE
  })
  output$plot <- renderPlot({
    input$iter
    k <- input$n_clus
    y <- if (input$data == "waiting") {
      faithful$waiting
    } else {
      data(CO2data)
      CO2data$CO2
    }
    if (input$sd_equal) sd_constr <- rep("a", k)
    else sd_constr <- NULL
    isolate({
      if (v$initialized == FALSE) {
        v$lambda0 <- runif(k)
        v$mu0 <- rnorm(k, mean = mean(y, na.rm = TRUE), 
                     sd = sd(y, na.rm = TRUE))
        v$sigma0 <- rep(sd(y, na.rm = TRUE), k)
        n_iter <- 0
      } else {
        n_iter <- as.numeric(input$n_iter)
      }
      print(sd_constr)
      mixmdl <- normalmixEM(y, v$lambda0, v$mu0, v$sigma0, 
                            maxit = n_iter, sd.constr = sd_constr)
      v$lambda0 <- mixmdl$lambda
      v$mu0 <- mixmdl$mu
      v$sigma0 <- mixmdl$sigma
      v$loglik <- mixmdl$loglik
      plot(mixmdl, which = 2, breaks = 10)
      curve(dnormmix(x, mixmdl$lambda, mixmdl$mu, mixmdl$sigma), 
            from = min(y), to = max(y), add = TRUE, lwd = 2, lty = 2)
    })
  })
  output$loglik <- renderText({
    paste("log-likelihood:", v$loglik)
  })
  output$table <- renderTable({
    out_table <- rbind(v$lambda0, v$mu0, v$sigma0)
    rownames(out_table) <- c("Mixing Proportion", "Mean", "SD")
    out_table
  })
}

shinyApp(ui, server)