library(shiny)

# plot_names <- c("plot_hists_rep_vs_obs",
#                 "plot_dens_sample_reps",
#                 "plot_obs_vs_avg_y_rep",
#                 "plot_hist_resids",
#                 "plot_avg_rep_vs_avg_resid_rep",
#                 #                     "plot_compare_min_max",
#                 #                     "plot_compare_mean_sd",
#                 "plot_test_statistics"
# ) 
# 
# plot_descriptions <- c("Histograms of observed data and a random sample of replications",
#                   "Density estimate of observed data (purple) and a random sample of 20 replications",
#                   "Observed data vs average simulated value",
#                   "Histogram of a single realization of the residuals (with normal density curve)",
#                   "Average simulated value vs average residual",
#                   "Histograms of test statistics")
# 
# plot_details <- list(
#   plot_hist_resids = 
#   wellPanel(
#     withMathJax("1) Draw a single set of values \\(\\beta^\\star, \\sigma^\\star \\sim p(\\beta,\\sigma | y)\\)"),
#     br(),
#     withMathJax("2) Draw a vector \\(y^{rep} \\sim N(X\\beta^\\star, \\sigma^\\star) \\)"), 
#     br(),
#     withMathJax("3) Compute and plot the vector \\(y - y^{rep}\\)")
#   ),
#   plot_hists_rep_vs_obs = 
#   wellPanel(
#     withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\sim p(\\beta,\\sigma | y)\\)"),
#     br(),
#     withMathJax("2) For each \\(s \\in 1, \\dots, S\\) draw a vector \\(y^{rep,[s]} \\sim N(X\\beta^{[s]}, \\sigma^{[s]}) \\)"), 
#     br(),
#     withMathJax("3) Plots histogram of \\(y\\) and histograms of each  \\(y^{rep,[s]}\\)")
#   )
#   )

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

  output$select_plot <- renderUI({
    selectizeInput("plot", label = h5(style = "color: #5b391e;", "Select a plot"), choices = plot_choices,
                   width = "400px")
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
    y <- get(input$y_name)
    X <- get(input$X_name)
    beta <- get(input$beta_name)
    sigma <- get(input$sigma_name)
    nIter <- nrow(beta)
    nReps <- length(y)
    
    stopifnot(nrow(beta) == length(sigma))
    out <- sapply(1:nIter, function(iter) rnorm(nReps, X%*%beta[iter, ], sigma[iter]))
    out
  })

  get_avg_y_hat <- function(y_hat) {
    apply(y_hat, 1, mean)
  }
  get_avg_y_rep <- function(y_rep) {
    apply(y_rep, 1, mean)
  }
#   get_avg_resids_hat <- function(y, y_hat) {
#     rowMeans(y_hat - y)
#   }

  get_resids <- function(y, y_rep) {
    y - y_rep
  }
  
  get_avg_resids <- function(y, y_rep) {
    rowMeans(y - y_rep)
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

#   output$plot_obs_vs_avg_y_hat <- renderPlot({
#     tests()
#     y <- get(input$y_name)
#     avg_y_hat <- get_avg_y_hat(y_hat())
#     
#     plot(y, avg_y_hat, xlab = "Observed", ylab = "Avg. Fitted", bty = "l", col = "maroon")  
#   })

  sample_id_for_resids <- reactive({
    go <- input$resample_resids_go          
    isolate({
      y_rep <- y_rep()
      sample_id <- sample(ncol(y_rep), 1)  
      sample_id
    })
  })
  output$plot_hist_resids <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    s <- sample_id_for_resids()
    resids <- y - y_rep[, s]
    

    x1 <- hist(resids, plot = FALSE)
    max1 <- max(x1$density)
    x2 <- dnorm(x1$breaks, mean(resids), sd(resids))
    max2 <- max(x2)
    ymax <- max(max1, max2)
    
    hist(resids, freq = FALSE, border = "white", col = "skyblue", 
         xlab = expression(residual == y - y^rep), main = "", ylim = c(0, ymax), yaxt = "n", ylab = "")
    curve(dnorm(x, mean(resids), sd(resids)), add = TRUE, lty = 2, col = "maroon")
    
    axis(side = 1, lwd = 4)
  })

  output$plot_obs_vs_avg_y_rep <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    avg_y_rep <- get_avg_y_rep(y_rep)
  
    plot(y, avg_y_rep, xlab = "Observed", ylab = "Avg. Simulated", bty = "l", col = "maroon")  
    axis(side = 1, lwd = 4)
    axis(side = 2, lwd = 4)
    box(lwd = 4, bty = "l")
  })

  sample_ids_for_dens <- reactive({
      go <- input$resample_dens_go          
      isolate({
        y_rep <- y_rep()
        sample_ids <- sample(ncol(y_rep), min(50, ncol(y_rep)))  
        sample_ids
    })
  })
  output$plot_dens_sample_reps <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    
#     sample_ids <- sample_ids_for_dens()
#     y_rep_20 <- y_rep[, sample_ids]
#     ymax20 <- max(c(density(y_rep_20)$y, density(y)$y))
    
#     par(mfrow = c(1, 2))

#     plot(density(y_rep_20[,1]), ylim = c(0, ymax20+0.05), bty = "l", col = "gray",
#          xlab = "y", main = "")
#     for (i in 2:20) lines(density(y_rep_20[,i]), col = "gray")
#     lines(density(y), col = "purple", lwd = 3)
#     
#     axis(side = 1, lwd = 4)
#     axis(side = 2, lwd = 4)
#     box(lwd = 4, bty = "l")


    sample_ids <- sample_ids_for_dens()
    y_rep_50 <- y_rep[, sample_ids]
    ymax <- max(c(density(y_rep_50)$y, density(y)$y))
    plot(density(y_rep_50[,1]), ylim = c(0, ymax+0.05), bty = "l", col = "gray80",
          xlab = "y", main = "")
#     for (i in 2:ncol(y_rep)) lines(density(y_rep[,i]), col = "gray80")
    lapply(2:ncol(y_rep_50), function(i) lines(density(y_rep_50[,i]), col = "gray80"))
    lines(density(y), col = "purple", lwd = 3)

    axis(side = 1, lwd = 4)
    axis(side = 2, lwd = 4)
    box(lwd = 4, bty = "l")
    
  })
  

  # check figure 6.11 in Gelman et al. (2013).
  output$plot_avg_rep_vs_avg_resid_rep <- renderPlot({    
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    avg_resids <- get_avg_resids(y, y_rep)
    avg_y_rep <- get_avg_y_rep(y_rep)
    plot(avg_y_rep, avg_resids, bty = "l", col = "maroon", 
         xlab = expression(paste("Average ", y^rep)), ylab = "Average residual")
    
    axis(side = 1, lwd = 4)
    axis(side = 2, lwd = 4)
    box(lwd = 4, bty = "l")
  })

  sample_ids <- reactive({
    go <- input$resample_hist_go          
    isolate({
      y_rep <- y_rep()
      sample_ids <- sample(ncol(y_rep), 8)  
      sample_ids
    })
  })

  output$plot_hists_rep_vs_obs <- renderPlot({    
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
#     sample_ids <- sample(nrow(y_rep), 11)  
    sample_ids <- sample_ids()
#     y_rep_samp <- y_rep[sample_ids, ]
    y_rep_samp <- y_rep[, sample_ids]
    par(mfrow = c(3,3), cex.main = 1.5)
    hist(y, border = "white", col = "purple", ylab = "", yaxt = "n", main = "Observed y")
    for (i in 1:8) hist(y_rep_samp[,i], # hist(y_rep_samp[i,],
                        main = bquote(paste(y^rep, " #", .(sample_ids[i]))),
                        border = "white", col = "skyblue",
                        xlab = "y",
                        ylab = "", 
                        yaxt = "n")
  })


  output$plot_test_statistics <- renderPlot({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    
    min_y <- min(y)
    min_y_rep <- apply(y_rep, 2, min)
    max_y <- max(y)
    max_y_rep <- apply(y_rep, 2, max)
    mean_y <- mean(y)
    mean_y_rep <- apply(y_rep, 2, mean)
    sd_y <- sd(y)
    sd_y_rep <- apply(y_rep, 2, sd)
    
    
    par(mfrow = c(2,2), cex.main = 1.5)
    hist(mean_y_rep, border = "white", col = "skyblue", freq = FALSE,
         xlab = expression(mean(y^rep)), yaxt = "n", ylab = "", main = "Mean")
    axis(side = 1, lwd = 4)
    abline(v = mean_y, col = "purple", lty = 1, lwd = 2)
#     abline(v = mean(mean_y_rep), lty = 2, lwd = 2)
#     mtext(expression(paste("Observed ", mean(y))), side = 3, line = 1, adj = 0, col = "purple")
#     mtext(expression(paste("Avg ", mean(y^rep))), side = 3, line = 2, adj = 0)
   
    
    hist(sd_y_rep, border = "white", col = "skyblue", freq = FALSE,
         xlab = expression(sd(y^rep)), yaxt = "n", ylab = "", main = "Standard Deviation")
    axis(side = 1, lwd = 4)
    abline(v = sd(y), col = "purple", lty = 1, lwd = 2)
#     abline(v = mean(sd_y_rep), lty = 2, lwd = 2)
#     mtext(expression(paste("Observed ", sd(y))), side = 3, line = 1, adj = 0, col = "purple")
#     mtext(expression(paste("Avg ", sd(y^rep))), side = 3, line = 2, adj = 0)
    
    hist(min_y_rep, border = "white", col = "skyblue", freq = FALSE,
         xlab = expression(min((y^rep))), yaxt = "n", ylab = "", main = "Minimum")
    axis(side = 1, lwd = 4)
    abline(v = min_y, col = "purple", lty = 1, lwd = 2)
#     abline(v = mean(min_y_rep), lty = 2, lwd = 2)
#     mtext(expression(paste("Osberved ",min((y)))), 
#           side = 3, line = 1, adj = 0, col = "purple")
#     mtext(expression(paste("Avg. ", min((y^rep)))),
#           side = 3, line = 2, adj = 0)
    
    
    hist(max_y_rep, border = "white", col = "skyblue", freq = FALSE,
         xlab = expression(max((y^rep))), yaxt = "n", ylab = "", main = "Maximum")
    axis(side = 1, lwd = 4)
    abline(v = max_y, col = "purple", lty = 1, lwd = 2)
#     abline(v = mean(max_y_rep), lty = 2, lwd = 2)
#     mtext(expression(paste("Osberved ",max((y)))), 
#           side = 3, line = 1, adj = 0, col = "purple")
#     mtext(expression(paste("Avg. ", max((y^rep)))),
#           side = 3, line = 2, adj = 0)
    
    
    par(mfrow = c(1,1))
    
  })


})
