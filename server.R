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
  variables = c("JWMNP", "PINCP", "RAC1P"), #travel and income, likely need to subset out 0 travel time people... dont' worry about too much of that now!
  state = "PA",
  recode = TRUE,
  year = 2022,
  survey = "acs1",
  key = my_key
  ) |>
  mutate(JWMNP = as.numeric(JWMNP), PINCP = as.numeric(PINCP)) |>
  filter(PINCP > 0, RAC1P %in% c(1,2)) |>
  mutate(RAC1P = recode(RAC1P, `1`="White",`2`="Black")) |>
  rename(Race = RAC1P)

#We need a loading screen

function(input, output, session) {
  sample_data <- reactiveValues(my_sample = NULL)
  sample_linreg <- reactiveValues(my_linreg = NULL, by_race_linreg = NULL)
  sample_corr <- reactiveValues(my_corr = NULL)
  
  observeEvent(input$sample, {
    sample_data$my_sample <- PA_data |> group_by(Race) |> slice_sample(n=50)
    sample_linreg$my_linreg <- lm(PINCP ~ JWMNP, data=sample_data$my_sample)
    sample_linreg$by_race_linreg <- lm(PINCP ~ JWMNP+as.factor(Race), data=sample_data$my_sample)
    sample_corr$my_corr <- round(sqrt(summary(sample_linreg$my_linreg)$r.squared),3)
  })
  
  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$Scatter <- renderPlot({
    if (!is.null(sample_data$my_sample)){
      
      #This is when subgroup button is pressed
      if (input$sub_group){
        
        if (input$regline){
          ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP, color=Race)) +
            geom_point()+geom_smooth(method="lm", fill=NA) 
        } else {
          ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP, color=Race)) +
            geom_point()
        }
        
      #This is when subgroup button is not pressed
      } else {
        
      if (input$regline){
      ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP)) +
        geom_point()+geom_smooth(method="lm", fill=NA)
      } else {
        ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP)) +
          geom_point()
      }
    }
  }
    })
  
  #Code for rendering the regression output table
    output$Fit <- renderTable(rownames = TRUE,{
    if (!is.null(sample_data$my_sample)){
      
      #If subgroup button is on, we'll display this one
      if(input$sub_group){
        by_group_fit <- summary(sample_linreg$by_race_linreg)
        by_group_fit$coefficients
        
      #If subgroup button is not on
      } else {
      state_fit <- summary(sample_linreg$my_linreg)
      state_fit$coefficients
    } 
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
