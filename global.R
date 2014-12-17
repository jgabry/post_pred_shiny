plot_choices <- list(
  "Compare observed data to replications"  = 
    list(
      "Histograms: obs vs replications" = 1,
      "Densities: obs vs replications" = 2,
      "Scatter: obs vs avg replicated" = 3
      ),
  "Residuals" = 
    list(
      "Histogram: residuals vs normal density" = 4,
      "Scatter: avg replicated vs avg residual" = 5
      ),
  "Distributions of test statistics" = 
    list(
      "Histograms: test statistics" = 6
      )
)
#   
#   "Histograms: obs vs replications" = 1,
#   "Densities: obs vs replications" = 2,
#   "Scatter: obs vs avg replicated" = 3,
#   "Histogram: residuals" = 4,
#   "Scatter: avg replicated vs avg residual" = 5,
#   "Histograms: test statistics" = 6
#   )


plot_names <- c("plot_hists_rep_vs_obs",
                "plot_dens_sample_reps",
                "plot_obs_vs_avg_y_rep",
                "plot_hist_resids",
                "plot_avg_rep_vs_avg_resid_rep",
                #                     "plot_compare_min_max",
                #                     "plot_compare_mean_sd",
                "plot_test_statistics"
) 

plot_descriptions <- c("Histograms of observed data and a random sample of replications",
                       "Density estimate of observed data (purple) and a random sample of 20 replications",
                       "Observed data vs average simulated value",
                       "Histogram of a single realization of the residuals (with normal density curve)",
                       "Average simulated value vs average residual",
                       "Histograms of test statistics")

plot_details <- list(
  plot_hist_resids = 
    wellPanel(
      h5("Making this plot: "),
      withMathJax("1) Draw a single set of values \\(\\beta^\\star, \\sigma^\\star \\) from the posterior"),
      br(),
      withMathJax("2) Given \\(\\beta^\\star, \\sigma^\\star \\) draw a vector \\(y^{rep} \\) from the posterior predictive distribution"), 
      br(),
      withMathJax("3) Plot histogram of the vector \\(residuals = y - y^{rep}\\)"),
      br(),
      withMathJax("4) Add density curve for normal distribution with mean \\(\\mu = \\text{mean}(residuals)\\) and standard deviation \\( \\sigma = \\text{sd}(residuals)\\) ")
    ),
  
  plot_hists_rep_vs_obs = 
    wellPanel(
      h5("Making this plot: "),
      withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\) from the posterior"),
      br(),
      withMathJax("2) For each of the \\(S\\) draws from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
      br(),
      withMathJax("3) Plots histogram of \\(y\\) and histograms of each of the \\(S\\) replications")
    ),
  
  plot_dens_sample_reps = 
    wellPanel(
      h5("Making this plot: "),
      withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\) from the posterior"),
      br(),
      withMathJax("2) For each of the \\(S\\) draws from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
      br(),
      withMathJax("3) Plot kernel density estimate of \\(y\\) and kernel density estimate of each of the \\(S\\) replications")
    ),
  
  plot_test_statistics = 
    wellPanel(
      h5("Making this plot: "),
      withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\) from the posterior"),
      br(),
      withMathJax("2) For each of the \\(S\\) draws from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
      br(),
      withMathJax("3) For each of the \\(S\\) replications compute the value of the test statistic (e.g. the mean)"),
      br(),
      withMathJax("4) Plot histogram of the \\(S \\) values of the test statistic"),
      br(),
      withMathJax("5) Add lines showing the value of the test statistic in the observed data and the average value of the test statistic over the \\(S\\) replications")
    )
)