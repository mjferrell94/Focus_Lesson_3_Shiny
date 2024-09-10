#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(tidycensus)

my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"


function(input, output, session) {
  data <- reactiveVal()
  
  observeEvent(input$sample){
    data <- get_pums(
      variables = c("JWMNP", "PINCP"), #travel and income, likely need to subset out 0 travel time people... dont' worry about too much of that now!
      state = "PA",
      recode = TRUE,
      year = 2022,
      survey = "acs1",
      key = my_key
    )
    rows <- sample(1:nrow(data),100)
    
    output$Scatter <- renderPlot({
      ggplot(data[rows,],aes(x=JWMNP,y=PINCP))+geom_point()
    })
  }
}
