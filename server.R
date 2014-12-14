library(shiny)

shinyServer(function(input, output) {  
  
# ui outputs --------------------------------------------------------------
  output$y_from_R <- renderUI({
    choices <- objects(envir = .GlobalEnv)
    selectizeInput("y_name", label = "", choices = c("", choices), 
                 options = list(placeholder = "data vector"))
  })

  output$X_from_R <- renderUI({
    choices <- objects(envir = .GlobalEnv)
    selectizeInput("X_name", label = "", choices = c("", choices), 
                 options = list(placeholder = "model matrix"))
  })

  output$beta_from_R <- renderUI({
    choices <- objects(envir = .GlobalEnv)
    selectizeInput("beta_name", label = "", choices = c("", choices), 
                 options = list(placeholder = "coefficient samples"))
  })

  output$sigma_from_R <- renderUI({
    choices <- objects(envir = .GlobalEnv)
    selectizeInput("sigma_name", label = "", choices = c("", choices), 
                 options = list(placeholder = "sd samples"))
  })

  output$replications <- renderUI({
    numericInput("nReps", label = withMathJax("Number of replicated data vectors \\(\\mathbf{y}_{rep}\\) to generate"), value = 500, step = 50)
  })

  output$select_plot <- renderUI({
#     selectizeInput("plot", label = "Select a plot", choices = 1:6)
    fluidRow(
      column(5, 
        sliderInput("plot", label = h5("Select a plot or press play to cycle through the options"), min = 1, max = 6, value = 1, step = 1,
                    animate = animationOptions(interval = 2500, loop = TRUE, playButton = NULL, pauseButton = NULL))
      )
    )
  })
  
  output$plot <- renderUI({
    which_plot <- as.numeric(input$plot)
    plot_names <- c("plot_hists_rep_vs_obs",
                    "plot_dens_sample_reps",
                    "plot_obs_vs_avg_y_hat", 
                    "plot_obs_vs_avg_y_rep",
                    "plot_avg_fitted_vs_avg_resid",
                    "plot_avg_rep_vs_avg_resid_rep"
                    ) 
    plot <- plot_names[which_plot]
    if (which_plot == 1) return(plotOutput(plot, height = "600px"))
    
    plotOutput(plot)
  })

  output$plot_description <- renderUI({
    which_plot <- as.numeric(input$plot)
    descriptions <- c("Histograms of observed data (black) and a random sample of replications (gray)",
                    "Density of observed data (purple) and a random sample of 20 replications",
                    "Observed data vs average fitted value (y vs. mean[y_hat])",
                    "Observed data vs average simulated value (y vs. mean[y_rep])",
                    "Average fitted value vs average residual (mean[y_hat] vs mean[y - y_hat])",
                    "Average simulated value vs average residual (mean[y_rep] vs mean[y - y_rep])")
    helpText(descriptions[which_plot])
  })

# functions ---------------------------------------------------------------
  y_hat <- reactive({
    X <- get(input$X_name)
    beta <- get(input$beta_name)
    nIter <- nrow(beta)
    out <- sapply(1:nIter, function(i) X%*%beta[i,])
    out
  })

  y_rep <- reactive({
    X <- get(input$X_name)
    beta <- get(input$beta_name)
    sigma <- get(input$sigma_name)
    nIter <- nrow(beta)
    nReps <- input$nReps
    
    stopifnot(nrow(beta) == length(sigma))
    out <- sapply(1:nIter, function(iter) rnorm(nReps, X%*%beta[iter, ], sigma[iter]))
    out
  })

  get_avg_y_hat <- function(y_hat) {
    rowMeans(y_hat)
  }
  get_avg_y_rep <- function(y_rep) {
    rowMeans(y_rep)
  }
  get_avg_resids_hat <- function(y, y_hat) {
    rowMeans(y_hat - y)
  }
  get_avg_resids_rep <- function(y, y_rep) {
    rowMeans(y_rep - y)
  }

# plots -------------------------------------------------------------------
  
  # tests for inputs
  tests <- reactive({
    t1 <- need(input$y_name != "", message = "Waiting for y: vector of observations \n")
    t2 <- need(input$X_name != "", message = "Waiting for X: model matrix with first column just ones (for intercept) \n")
    t3 <- need(input$beta_name != "", message = "Waiting for beta: matrix of posterior samples of regression coefficients \n")
    t4 <- need(input$sigma_name != "", message = "Waiting for sigma: vector of posterior samples of the standard deviation \n")
    validate(t1, t2, t3, t4)
  })

  output$plot_obs_vs_avg_y_hat <- renderPlot({
    tests()
    y <- get(input$y_name)
    avg_y_hat <- get_avg_y_hat(y_hat())
    
    plot(y, avg_y_hat, xlab = "Observed", ylab = "Avg. Fitted", bty = "l", col = "maroon")  
  })

  output$plot_obs_vs_avg_y_rep <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    if (length(y != nrow(y_rep))) {
      y_rep <- y_rep[sample(nrow(y_rep), length(y)), ]
    }
    avg_y_rep <- get_avg_y_rep(y_rep)
  
    plot(y, avg_y_rep, xlab = "Observed", ylab = "Avg. Simulated", bty = "l", col = "maroon")  
  })

  output$plot_dens_sample_reps <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    
    y_rep_20 <- y_rep[sample(nrow(y_rep), 20) ,]
    ymax <- max(c(density(y_rep_20)$y, density(y)$y))
    
    plot(density(y_rep_20[1,]), ylim = c(0, ymax+0.05), bty = "l", 
         xlab = "Value", main = "")
    for (i in 2:20) lines(density(y_rep_20[i,]))
    lines(density(y), col = "purple", lwd = 3)
  })
  

  # check figure 6.11 in Gelman et al. (2013).
  output$plot_avg_fitted_vs_avg_resid <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_hat <- y_hat()
    avg_resids <- get_avg_resids_hat(y, y_hat)
    avg_y_hat <- get_avg_y_hat(y_hat)
    plot(avg_y_hat, avg_resids, bty = "l", col = "maroon")
  })

  output$plot_avg_rep_vs_avg_resid_rep <- renderPlot({    
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    avg_resids_rep <- get_avg_resids_rep(y, y_rep)
    avg_y_rep <- get_avg_y_rep(y_rep)
    plot(avg_y_rep, avg_resids_rep, bty = "l", col = "maroon")
  })

  output$plot_hists_rep_vs_obs <- renderPlot({    
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    sample_ids <- sample(nrow(y_rep), 11)
    y_rep_11 <- y_rep[sample_ids, ]
    par(mfrow = c(4,3))
    hist(y, border = "white", col = "black", ylab = "", yaxt = "n", main = "Histogram of y")
    for (i in 1:11) hist(y_rep_11[i,],
                         main = paste0("y_rep #", sample_ids[i]),
                         border = "white", col = "darkgray",
                         ylab = "", yaxt = "n")
  })

})
