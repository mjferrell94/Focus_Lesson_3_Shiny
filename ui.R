#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title="Regression Activity"),
  
  dashboardSidebar(    
    sidebarMenu(
      actionButton("sample","Sample"),
      numericInput(inputId="corr", label="Correlation",value=NULL,min=-1, max=1,step=.01),
      checkboxInput("regline","Add Regression Line"),
      checkboxInput("reg_output","Show Regression Output"),
      checkboxInput("sub_group","By Race")
    )
  ),
  
  dashboardBody(
    plotOutput("Scatter"),
    conditionalPanel("input.reg_output",
                     tableOutput("Fit")
    ),
    conditionalPanel("input.corr",
                     textOutput("Corr_Guess")
    )
    )
)