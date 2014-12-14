
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Posterior predictive checks"),
  h5("for Bayesian linear regression"),
  hr(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Select the appropriate objects from your Global Environment"),
      uiOutput("y_from_R"),
      uiOutput("X_from_R"),
      uiOutput("beta_from_R"),
      uiOutput("sigma_from_R"),
      hr(),
      uiOutput("replications")
    ),

    # Show a plot of the generated distribution
    mainPanel( 
      uiOutput("select_plot"),
      br(),br(),
      uiOutput("plot_description"),
      uiOutput("plot")
    )
  )
))
