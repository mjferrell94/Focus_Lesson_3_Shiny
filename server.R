#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(tidycensus)

my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"

#just for now!
PA_data <- get_pums(
  variables = c("JWMNP", "PINCP"), #travel and income, likely need to subset out 0 travel time people... dont' worry about too much of that now!
  state = "PA",
  recode = TRUE,
  year = 2022,
  survey = "acs1",
  key = my_key
  ) |>
  mutate(JWMNP = as.numeric(JWMNP), PINCP = as.numeric(PINCP)) |>
  filter(PINCP > 0)

#We need a loading screen

function(input, output, session) {
  sample_data <- reactiveValues(my_sample = NULL)
  sample_linreg <- reactiveValues(my_linreg = NULL)
  sample_corr <- reactiveValues(my_corr = NULL)
  
  observeEvent(input$sample, {
    sample_data$my_sample <- PA_data[sample(1:nrow(PA_data),100), ] 
    sample_linreg$my_linreg <- lm(PINCP ~ JWMNP, data=sample_data$my_sample)
    sample_corr$my_corr <- round(sqrt(summary(sample_linreg$my_linreg)$r.squared),3)
  })
  
  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$Scatter <- renderPlot({
    if (!is.null(sample_data$my_sample)){

      if (input$regline){
      ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP)) +
        geom_point()+geom_abline(slope = sample_linreg$my_linreg$coefficients[[2]], 
                                 intercept=sample_linreg$my_linreg$coefficients[[1]] )
      } else {
        ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP)) +
          geom_point()
      }
    }
  })
  
  #Code for rendering the regression output table
    output$Fit <- renderTable(rownames = TRUE,{
    if (!is.null(sample_data$my_sample)){
      state_fit <- summary(sample_linreg$my_linreg)
      state_fit$coefficients
    } else {
      "Click on the Sample button to begin"
    }
    })
    
    
  #Code for the correlation guessing game
    observeEvent(input$corr,{
      if (!is.null(sample_data$my_sample)){
        error <- abs(input$corr - sample_corr$my_corr) <= .05
        
        if(error){
          
          output$Corr_Guess <- renderText(paste("Nicely done! The actual correlation is", sample_corr$my_corr, sep=" "))
          
        } else {
          
          if(input$corr > sample_corr$my_corr){
            
          output$Corr_Guess <- renderText("Try guessing lower!")
          } else {
            
            output$Corr_Guess <- renderText("Try guessing higher!")
            
          }
        }

      }

    })

}
