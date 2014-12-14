library(shiny)

shinyUI(fluidPage(

  # Application title
#   titlePanel("Graphical posterior predictive checks"),
  h1(style = "color: #5b391e", "Graphical posterior predictive checks"),
  h4(style="color:#66bbae", "for Bayesian linear regression"),
  hr(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(style="border-color:#c6c386",
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
      uiOutput("replications")
    ),

    # Show a plot of the generated distribution
    mainPanel( 
      uiOutput("select_plot"),
      br(),br(),
      uiOutput("plot_description"),
      br(),
      uiOutput("plot")
    )
  )
))
