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

my_sample <- readRDS("my_sample_temp.rds")

function(input, output, session) {
  
  
  #################################################3
  ##Correlation tab
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  #make sure two variables are selected
  observeEvent(input$corr_sample, {
    
    if(length(input$corr_vars) != 2) {
      shinyalert(title = "Exactly Two Variables are Required",
                 "Please select two variables and try again.",
                 type = "error")
    } else {
      if(input$hhl_corr == "all"){
        hhl_sub <- HHLvals
      } else if(input$hhl_corr == "english"){
        hhl_sub <- HHLvals["1"]
      } else if(input$hhl_corr == "spanish"){
        hhl_sub <- HHLvals["2"]
      } else {
        hhl_sub <- HHLvals[c("0", "3", "4", "5")]
      }
      
      if(input$fs_corr == "all"){
        fs_sub <- FSvals
      } else if(input$fs_corr == "yes"){
        fs_sub <- FSvals["1"]
      } else {
        fs_sub <- FSvals["2"]
      }
      
      if(input$schl_corr == "all"){
        schl_sub <- SCHLvals
      } else if(input$schl_corr == "no_hs"){
        schl_sub <- SCHLvals[as.character(0:15)]
      } else if(input$schl_corr == "hs"){
        schl_sub <- SCHLvals[as.character(16:19)]
      } else {
        schl_sub <- SCHLvals[as.character(20:24)]
      }
      
      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
        ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% input$corr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% input$corr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% input$corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% input$corr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% input$corr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% input$corr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% input$corr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% input$corr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% input$corr_vars) filter(., !is.na(JWMNP)) else .} 
        
      index <- sample(1:nrow(subsetted_data), 
                      size = input$corr_n, 
                      replace = TRUE, 
                      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
      sample_corr$corr_data <- subsetted_data[index, ]
      sample_corr$corr_truth <- cor(sample_corr$corr_data |> 
                                      select(input$corr_vars))[1,2]
    }
  })
  
  

  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$corr_scatter <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_vars)[1], y = isolate(input$corr_vars)[2])) +
      geom_point()
    })
  
    
    
  #Code for the correlation guessing game
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success"
                 )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })

    
    ########################################################3
    ##SLR stuff
    sample_data <- reactiveValues(my_sample = NULL)
    sample_linreg <- reactiveValues(my_linreg = NULL, by_race_linreg = NULL)
    
    # #Code for rendering the regression output table
    # output$Fit <- renderTable(rownames = TRUE,{
    #   if (!is.null(sample_data$my_sample)){
    #     
    #     #If subgroup button is on, we'll display this one
    #     if(input$sub_group){
    #       by_group_fit <- summary(sample_linreg$by_race_linreg)
    #       by_group_fit$coefficients
    #       
    #       #If subgroup button is not on
    #     } else {
    #       state_fit <- summary(sample_linreg$my_linreg)
    #       state_fit$coefficients
    #     } 
    #   }
    # })
    
}
