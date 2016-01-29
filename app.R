library(shiny)
library(mixtools)

ui <- fluidPage(
  titlePanel("Simple Demonstration of Univariate Normal Mixture"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_clus", label = "Number of clusters:",
                  min = 2, max = 4, value = 2, step = 1, ticks = FALSE), 
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
                       initialized = FALSE)
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
      mixmdl <- normalmixEM(y, v$lambda0, v$mu0, v$sigma0, 
                            maxit = n_iter)
      v$lambda0 <- mixmdl$lambda
      v$mu0 <- mixmdl$mu
      v$sigma0 <- mixmdl$sigma
      plot(mixmdl, which = 2, breaks = 10)
      curve(dnormmix(x, mixmdl$lambda, mixmdl$mu, mixmdl$sigma), 
            from = min(y), to = max(y), add = TRUE, lwd = 2, lty = 2)
    })
  })
  output$table <- renderTable({
    out_table <- rbind(v$lambda0, v$mu0, v$sigma0)
    rownames(out_table) <- c("Mixing Proportion", "Mean", "SD")
    out_table
  })
}

shinyApp(ui, server)