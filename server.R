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
  
  observeEvent(input$sample, {
    sample_data$my_sample <- PA_data[sample(1:nrow(PA_data),100), ] 
      
  })
  
  output$Scatter <- renderPlot({
    if (!is.null(sample_data$my_sample)){
      ggplot(sample_data$my_sample, aes(x=JWMNP, y=PINCP)) +
        geom_point()
    }
  })
  
  
  observeEvent(input$resfit,{  
    output$Fit <- renderTable({
    if (!is.null(sample_data$my_sample)){
      state_fit <- summary(lm(PINCP ~ JWMNP, data=sample_data$my_sample))
      state_fit$coefficients
    }
    
  }
  )
    }
  )

}
