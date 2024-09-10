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
    numericInput(inputId="corr", label="Correlation",value=0,min=-1, max=1),
    checkboxInput("resfit","ResFit")
  )
  ),
  
  dashboardBody(plotOutput(output$Scatter)
  )
)