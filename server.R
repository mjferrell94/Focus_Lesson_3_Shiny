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
  
  #update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices)
    }
  })
  
  #make sure two variables are selected
  observeEvent(input$corr_sample, {
    
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
    
    corr_vars <- c(input$corr_x, input$corr_y)

    subsetted_data <- my_sample |>
      filter(#cat vars first
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .} 
      
    index <- sample(1:nrow(subsetted_data), 
                    size = input$corr_n, 
                    replace = TRUE, 
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> 
                                    select(corr_vars))[1,2]
  })
  
  

  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$corr_scatter <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
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
    sample_slr <- reactiveValues(slr_data = NULL, slr_ls = NULL, slr_user = NULL)

    #update input boxes so they can't choose the same variable
    observeEvent(c(input$slr_x, input$slr_y), {
      slr_x <- input$slr_x
      slr_y <- input$slr_y
      choices <- numeric_vars
      if (slr_x == slr_y){
        choices <- choices[-which(choices == slr_x)]
        updateSelectizeInput(session,
                             "slr_y",
                             choices = choices)
      }
    })
    
    #make sure two variables are selected
    observeEvent(input$slr_sample, {
      
      if(input$hhl_slr == "all"){
        hhl_sub <- HHLvals
      } else if(input$hhl_slr == "english"){
        hhl_sub <- HHLvals["1"]
      } else if(input$hhl_slr == "spanish"){
        hhl_sub <- HHLvals["2"]
      } else {
        hhl_sub <- HHLvals[c("0", "3", "4", "5")]
      }
      
      if(input$fs_slr == "all"){
        fs_sub <- FSvals
      } else if(input$fs_slr == "yes"){
        fs_sub <- FSvals["1"]
      } else {
        fs_sub <- FSvals["2"]
      }
      
      if(input$schl_slr == "all"){
        schl_sub <- SCHLvals
      } else if(input$schl_slr == "no_hs"){
        schl_sub <- SCHLvals[as.character(0:15)]
      } else if(input$schl_slr == "hs"){
        schl_sub <- SCHLvals[as.character(16:19)]
      } else {
        schl_sub <- SCHLvals[as.character(20:24)]
      }
      
      slr_vars <- c(input$slr_x, input$slr_y)
      
      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
        ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% slr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% slr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% slr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% slr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% slr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% slr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% slr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% slr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% slr_vars) filter(., !is.na(JWMNP)) else .} 
      
      index <- sample(1:nrow(subsetted_data), 
                      size = input$slr_n, 
                      replace = TRUE, 
                      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
      sample_slr$slr_data <- subsetted_data[index, ]
      sample_slr$slr_ls <- lm(get(input$slr_y) ~ get(input$slr_x), data = sample_slr$slr_data)
    })
    
    #create all the residuals and find the SSE/MSE for both lines
    observeEvent(c(input$slr_sample, input$slr_int, input$slr_slope), {
      slr_data <- sample_slr$slr_data
      
    })
    
    #Create graph
    output$slr_scatter <- renderPlot({
      validate(
        need(!is.null(sample_slr$slr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
      )
      #data and user values for line
      slr_data <- sample_slr$slr_data
      user_line <- function(x){
        input$slr_int + input$slr_slope * x
      }
      #values for plotting purposes
      x_values <- slr_data |> 
        pull(input$slr_x)
      x_min <- min(x_values)
      x_max <- max(x_values)
      y_min <- min(c(user_line(x_min), user_line(x_max)))
      y_max <- max(c(user_line(x_min), user_line(x_max)))
      g <- ggplot(sample_slr$slr_data, aes_string(x = isolate(input$slr_x), y = isolate(input$slr_y))) +
        geom_point() +
        geom_line(data = data.frame(x = c(x_min, x_max), y = c(user_line(x_min), user_line(x_max))), aes(x = x, y = y))
      if(input$add_ls_line){
        g <- g +
          geom_smooth(method = "lm", se = FALSE)
      }
      g
    })
    
    output$slr_info <- renderText({
      "temp text"
    })
    
}
