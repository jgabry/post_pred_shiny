library(shiny)

shinyUI(fluidPage(
  
  h1(style = "color: #5b391e", "Graphical posterior predictive checks"),
  h4(style ="color:#66bbae", "for Bayesian linear regression"),
  hr(),
  
  navlistPanel(widths = c(4,8),   
               
               tabPanel("INTRO", 
                        h3(style = "color: #5b391e", "What is posterior predictive checking?"),
                        p(strong("The idea behind posterior predictive checking is simple:")),
                        p(style = "text-indent: 10px", 
                          em("If our model is a good fit then we should be able to use it to generate")), 
                        p(style = "text-indent: 10px", em("data that looks a lot like the data we observed.")),
                        br(),
                        p("To generate this 'replicated' data we use the", 
                          em("posterior predictive distribution")),
                        withMathJax("$$ p(y^{rep} | y )  = \\int p(y^{rep} | \\theta) p(\\theta | y ) d \\theta,$$"),
                        p(withMathJax("where \\(y\\) is the observed data and \\(\\theta\\) the parameters in our model.")),
                        br(),
                        p("In practice we typically use a ", 
                          a("Markov chain Monte Carlo method", href = "http://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo"), 
                          withMathJax("to draw samples from the posterior distribution \\(p(\\theta | y)\\) 
                               and for each of these samples we can simulate data \\(y^{rep}\\) from the posterior predictive distribution.")),
                        br(),
                        p(withMathJax("Using the simulations of \\(y^{rep}\\) we can make various
                              graphical displays comparing our observed data to the replications.")),
                        hr(),
                        helpText("Based on ideas presented in ", a("Bayesian Data Analysis, Third Edition", href = "http://www.crcpress.com/product/isbn/9781439840955"), "by Gelman et al.")
                        
               ),
               
               
               tabPanel("Using the app",
                        h4(style = "color: #5b391e", "Input your data"),
                        helpText("Click on 'Input data' in the menu on the left and select the appropriate objects from your R Global Environment."),
                        br(),
                        h4(style = "color: #5b391e", "Plots"),
                        helpText("Click on a plot description in the menu on the left to view the plot. Below each plot is also a brief description of the steps required to create it."),
                        br(),
                        h4(style = "color: #5b391e", "Examples"),
                        a("Click to view examples on GitHub", href = "https://github.com/jgabry/post_pred_shiny")
               ),
               
               "------",
               tabPanel("Input data",
                        
                        h4(style = "color: #5b391e", "Select the appropriate objects from your R Global Environment"),
                        br(),
                        h5(style = "color:#87c6c3", withMathJax("\\(\\mathbf{y}\\)")),
                        fluidRow(
                          column(5, uiOutput("y_from_R")),
                          column(7, helpText("The dependent variable, a vector of observations."))
                        ),
                        
                        br(),
                        h5(style = "color:#87c6c3", withMathJax("\\(\\mathbf{X}\\)")),
                        fluidRow(
                          column(5, uiOutput("X_from_R")),
                          column(7, helpText("Model matrix of predictors. If your model has an intercept the first column should be a column of 1s."))
                        ),
                        
                        br(),
                        h5(style = "color:#87c6c3", withMathJax("\\(\\beta\\)")),
                        fluidRow(
                          column(5, uiOutput("beta_from_R")),
                          column(7, helpText("A matrix (number of simulations * number of coefficients) of posterior samples of the regression coefficients. If your model has an intercept this should be the first column."))
                        ),
                        
                        br(),
                        h5(style = "color:#87c6c3", withMathJax("\\(\\sigma\\)")),
                        fluidRow(
                          column(5, uiOutput("sigma_from_R")),
                          column(7, helpText("A vector of posterior samples of the standard deviation."))
                        )
               ),
               
               
               
               "------",
               "PLOTS",
               "compare observed data to replications",
               
               tabPanel("Histograms of observed data and replications",
                        helpText(withMathJax(plot_descriptions["plot_hists_rep_vs_obs"])),
                        plotOutput("plot_hists_rep_vs_obs"),
                        actionButton("resample_hist_go", label = "Show different replications", icon = icon("refresh")),
                        hr(),
                        wellPanel(
                          h5("Making the plot: "),
                          withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\) from the posterior"),
                          br(),
                          withMathJax("2) For each of the \\(S\\) draws from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
                          br(),
                          withMathJax("3) Plots histogram of \\(y\\) and histograms of each of the \\(S\\) replications")
                        )
               ),
               tabPanel("Density estimates of observed data and replications",
                        helpText(withMathJax(plot_descriptions["plot_dens_sample_reps"])),
                        plotOutput("plot_dens_sample_reps"),
                        actionButton("resample_dens_go", label = "Show different replications", icon = icon("refresh")),
                        hr(),
                        wellPanel(
                          h5("Making the plot: "),
                          withMathJax("1) Draw \\(S\\) sets of values \\(\\beta^{[s]}, \\sigma^{[s]} \\) from the posterior"),
                          br(),
                          withMathJax("2) For each of the \\(S\\) draws from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
                          br(),
                          withMathJax("3) Plot kernel density estimate of \\(y\\) and kernel density estimate of each of the \\(S\\) replications")
                        )
               ),
               tabPanel("Scatter plot of observed data vs average simulated value",
                        helpText(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
                        plotOutput("plot_obs_vs_avg_y_rep")
               ),
               
               "Residuals",
               
               tabPanel("Distribution of realized residuals with normal density curve",
                        helpText(withMathJax(plot_descriptions["plot_hist_resids"])),
                        plotOutput("plot_hist_resids"),
                        actionButton("resample_resids_go", label = "Show a different replication", icon = icon("refresh")),
                        hr(),
                        wellPanel(
                          h5("Making the plot: "),
                          withMathJax("1) Draw a single set of values \\(\\beta^\\star, \\sigma^\\star \\) from the posterior"),
                          br(),
                          withMathJax("2) Given \\(\\beta^\\star, \\sigma^\\star \\) draw a vector \\(y^{rep} \\) from the posterior predictive distribution"), 
                          br(),
                          withMathJax("3) Plot histogram of the vector \\(residuals = y - y^{rep}\\)"),
                          br(),
                          withMathJax("4) Add density curve for normal distribution with mean \\(\\mu = \\text{mean}(residuals)\\) and standard deviation \\( \\sigma = \\text{sd}(residuals)\\) ")
                        )
               ),
               tabPanel("Scatter plot of average simulated value vs average residual",
                        helpText(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
                        plotOutput("plot_avg_rep_vs_avg_resid_rep")
               ),
               
               "Distributions of test statistics",
               
               tabPanel("Distributions of test statistics",
                        helpText(withMathJax(plot_descriptions["plot_test_statistics"])),
                        plotOutput("plot_test_statistics"),
                        hr(),
                        wellPanel(
                          h5("Making the plot: "),
                          withMathJax("1) Draw \\(S\\) samples of \\(\\beta\\) and \\(\\sigma\\) from the posterior"),
                          br(),
                          withMathJax("2) For each of the \\(S\\) simulations from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
                          br(),
                          withMathJax("3) For each of the \\(S\\) replications compute the value of the test statistic"),
                          br(),
                          withMathJax("4) Plot a histogram of the \\(S \\) values of the test statistic"),
                          br(),
                          withMathJax("5) Add a line showing the value of the test statistic computed from the observed data")
                        )
               )
               
  )
  
)
)