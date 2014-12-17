library(shiny)

shinyUI(fluidPage(

  # Application title
#   titlePanel("Graphical posterior predictive checks"),
  h1(style = "color: #5b391e", "Graphical posterior predictive checks"),
  h4(style="color:#66bbae", "for Bayesian linear regression"),
  hr(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(style="border-color:#c6c386;", width = 3, 
      helpText(#style = "color:;", 
               strong("Select the appropriate objects from your R Global Environment")),
      h5(style = "color:#87c6c3", withMathJax("\\(\\mathbf{y}\\)")),
      uiOutput("y_from_R"),
      h5(style = "color:#87c6c3", withMathJax("\\(\\mathbf{X}\\)")),
      uiOutput("X_from_R"),
      h5(style = "color:#87c6c3", withMathJax("\\(\\beta\\)")),
      uiOutput("beta_from_R"),
      h5(style = "color:#87c6c3", withMathJax("\\(\\sigma\\)")),
      uiOutput("sigma_from_R"),
      hr(),
      a(strong("Or view examples on GitHub"), href = "https://github.com/jgabry/post_pred_shiny")
#       uiOutput("replications")
    ),

    # Show a plot of the generated distribution
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("", icon = icon("home", "fa-2x"),
#                  a("Examples on GitHub", href = "https://github.com/jgabry/post_pred_shiny"),
#                  hr(),
                 h4("What is posterior predictive checking?"),
                 p(strong("The idea behind posterior predictive checking is simple:"), 
                   em("if our model is good then we should be able to use it to generate 
                      data that looks a lot like the data we observed.")),
                 p("To generate this 'replicated' data we use the", 
                   em("posterior predictive distribution")),
                 withMathJax("$$ p\\left(y^{rep} | y \\right)  = \\int p\\left(y^{rep} | \\theta\\right) p\\left(\\theta | y \\right) d \\theta. $$"),
                 p(withMathJax("In practice we typically use a Markov chain Monte Carlo method to draw samples from the posterior distribution \\(p(\\theta | y)\\) 
                               and for each of these samples we simulate data \\(y^{rep}\\) from the posterior predictive distribution.")),
                 p("Using the simulations from the posterior predictive distribution we can make various
                   graphical displays comparing our observed data to the simulated data."),
                 h5("The 'Plots' tab contains the following graphs:"),
                 helpText(style = "text-indent: 25px", "1. Histogram of observed data alongside histograms of replicated data"),
                 helpText(style = "text-indent: 25px", "2. Density estimates of observed data and replicated data"),
                 helpText(style = "text-indent: 25px", "3. Scatter plot of observed data vs average simulated value"),
                 helpText(style = "text-indent: 25px", "4. Histogram of residuals with normal density curve"),
                 helpText(style = "text-indent: 25px", "5. Scatter plot of average simulated value vs average residual"),
                 helpText(style = "text-indent: 25px", "6. Histograms of the posterior predictive distributions various test statistics (min, max, mean, sd) for comparisons to the observed values of the test statistics")
      ),
      tabPanel(""),tabPanel(""),
        tabPanel("Plots", icon = icon("bar-chart-o", "fa-2x"), 
                 uiOutput("select_plot"),
                 br(),br(),
                 uiOutput("plot_description"),
                 br(),
                 uiOutput("plot"),
                 uiOutput("resample_hist"),
                 uiOutput("resample_resids"),
                 uiOutput("resample_dens"),
                 hr(),
                 uiOutput("plot_details")
                 )

        )
    )
  )
))
