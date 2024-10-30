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
library(plotly)

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
    if (((input$corr_x == "GRPIP") & (input$corr_y %in% c("TAXAMT", "VALP"))) | ((input$corr_y == "GRPIP") & (input$corr_x %in% c("TAXAMT", "VALP")))){
      shinyalert(title = "Oh no!", "Those with Property taxes and/or Property Values usually don't have a rent payment. Please select a different combination of variables.", type = "error")
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices[-2],
                           selected = choices[1]
      )
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices[-1],
                           selected = choices[2]
      )
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
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04", 
                             "05", "06", "07", "08", "09",
                             "10", "11", "12", "13", "14", "15")]
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
                                    select(all_of(corr_vars)))[1,2]
  })
  
  

  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$corr_scatter <- renderPlotly({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    g <- ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
      geom_point() 
    ggplotly(g)
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
    sample_slr <- reactiveValues(slr_data = NULL, slr_ls = NULL, slr_user = NULL, variable_issue = FALSE)

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
      if (((input$slr_x == "GRPIP") & (input$slr_y %in% c("TAXAMT", "VALP"))) | ((input$slr_y == "GRPIP") & (input$slr_x %in% c("TAXAMT", "VALP")))){
        shinyalert(title = "Oh no!", "Those with Property taxes and/or Property Values usually don't have a rent payment. Please select a different combination of variables.", type = "error")
        updateSelectizeInput(session,
                             "slr_x",
                             choices = choices[-2],
                             selected = choices[1]
                              )
        updateSelectizeInput(session,
                             "slr_y",
                             choices = choices[-1],
                             selected = choices[2]
        )
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
          schl_sub <- SCHLvals[c("0", "01", "02", "03", "04", 
                                 "05", "06", "07", "08", "09",
                                 "10", "11", "12", "13", "14", "15")]
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
    
    #update the user slider for the intercept
    observeEvent(input$slr_sample, {
      #grab data
      slr_data <- sample_slr$slr_data
      #grab the fit and summaries needed
      fit <- sample_slr$slr_ls
      coefs <- coef(fit)
      sigma <- summary(fit)$sigma
      #grab x-values to find max y-hat
      x_values <- slr_data |> 
        pull(input$slr_x)
      #find the y-intercept value to compare +/- 3 SE of line
      int_low <- coefs[1]-3*sigma
      int_high <- coefs[1]+3*sigma
      #find the max y-hat value and min y-hat value
      end_low <- min(coefs[1] + coefs[2]*x_values) -3*sigma
      end_high <- max(coefs[1] + coefs[2]*x_values) +3*sigma
      #set min and max for ease below
      min <- floor(min(int_low, end_low))
      max <- ceiling(max(int_high, end_high))
      updateSliderInput(session, 
                        "slr_int", 
                        min = min, 
                        max = max, 
                        value = round(slr_data |> 
                                        pull(input$slr_y) |>
                                        mean()),
                        step = (max-min)/1000)
      slope_min <- (ceiling(max(int_high, end_high))- floor(min(int_low, end_low)))/(min(x_values)-max(x_values))
      if(abs(slope_min) < 1) {
        slope_min <- signif(slope_min, 3)
      } else{
        slope_min <- ceiling(slope_min)
      }
      slope_max <- -slope_min
      updateSliderInput(session,
                        "slr_slope",
                        min = slope_min,
                        max = slope_max,
                        value = 0,
                        step = (slope_max-slope_min)/1000)
      })
    
    #create all the residuals and find the SSE/MSE for both lines
    observeEvent(c(input$slr_sample, input$slr_int, input$slr_slope), {
      slr_data <- sample_slr$slr_data
    })
    
    #update graphs
    observeEvent(input$add_to_plot, {
        if(!("Show Least Squares Line" %in% isolate(input$add_to_plot))){
          updateCheckboxGroupInput(session, "add_to_plot", choices = c("Show User Line Residuals", "Show Least Squares Line"), inline = TRUE, selected = isolate(input$add_to_plot))
        }
        if("Show Least Squares Line" %in% isolate(input$add_to_plot)){
          updateCheckboxGroupInput(session, "add_to_plot", choices = c("Show User Line Residuals", "Show Least Squares Line", "Show Least Squares Line Residuals"), selected = isolate(input$add_to_plot), inline = TRUE)
        }
      })
    
    observeEvent(input$tabset1, {
      if(input$tabset1 != "Scatter Plot with Line(s)"){
        updateCheckboxGroupInput(session, "add_to_plot", choices = c("Show User Line Residuals", "Show Least Squares Line"), inline = TRUE, selected = NULL)
      }
      if(input$tabset1 != "Residual Plot(s)") {
        updateCheckboxInput(session, "plot_slr_resid", value = FALSE)
      }
      if(input$tabset1 != "QQ Plot(s)") {
        updateCheckboxInput(session, "plot_slr_qq", value = FALSE)
      }
    })

    #Create graph
    output$slr_scatter <- renderPlotly({
      validate(
        need(!is.null(sample_slr$slr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
      )
      #data and user values for line
      slr_data <- sample_slr$slr_data
      user_line <- function(x){
        input$slr_int + input$slr_slope * x
      }
      colors <- c("User Line" = "green", "Least Squares Line" = "blue")
      #values for plotting purposes
      x_values <- slr_data |> 
        pull(isolate(input$slr_x))
      x_min <- min(x_values)
      x_max <- max(x_values)
      y_min <- min(c(user_line(x_min), user_line(x_max)))
      y_max <- max(c(user_line(x_min), user_line(x_max)))
      
      #add residuals and such to data
      slr_data$x_values <- x_values
      #user stuff
      user_y <- user_line(x_values)
      true_y <- slr_data |>
        pull(isolate(input$slr_y))
      user_resids <- true_y - user_y
      
      slr_data$User_Residual <- user_resids
      slr_data$user_y <- user_y
      
      #slr stuff
      fit <- sample_slr$slr_ls
      coefs <- coef(fit)
      ls_y <- coefs[1] + coefs[2]*x_values
      ls_resids <- true_y - ls_y
      
      slr_data$ls_y <- ls_y
      slr_data$Least_Squares_Residual <- round(ls_resids, 3)
      
      #############
      #create full plots within if then else to get tooltip right...
      #only the main plot
      if(!("Show User Line Residuals" %in% input$add_to_plot) & !("Show Least Squares Line" %in% input$add_to_plot)){
        #create base plot  
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point() +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) + 
          labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color")
        ggplotly(g, tooltip = tooltip)
        ##Just the main plot and user residuals
      } else if(("Show User Line Residuals" %in% input$add_to_plot) & !("Show Least Squares Line" %in% input$add_to_plot)){
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point(aes(label = User_Residual)) +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) +
          geom_segment(aes(x = x_values, 
                           xend = x_values, 
                           y = true_y, 
                           yend = user_y, 
                           color = "User Line", 
                           label = User_Residual), 
                       linetype = "dashed", 
                       linewidth = 0.25)+ 
          labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color", "label")
        ggplotly(g, tooltip = tooltip)
        ##Just the main plot and SLR fit
      } else if(!("Show User Line Residuals" %in% input$add_to_plot) & ("Show Least Squares Line" %in% input$add_to_plot) & !("Show Least Squares Line Residuals" %in% input$add_to_plot)){
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point(aes(label = User_Residual)) +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) +
          geom_smooth(method = "lm", 
                      se = FALSE, 
                      aes(color = "Least Squares Line")) +
          labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color")
        ggplotly(g, tooltip = tooltip)
        ##User line residuals and LS line
      } else if(("Show User Line Residuals" %in% input$add_to_plot) & ("Show Least Squares Line" %in% input$add_to_plot) & !("Show Least Squares Line Residuals" %in% input$add_to_plot)){
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point(aes(label = User_Residual)) +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) +
          geom_smooth(method = "lm", 
                      se = FALSE, 
                      aes(color = "Least Squares Line")) +
          geom_segment(aes(x = x_values, 
                           xend = x_values, 
                           y = true_y, 
                           yend = user_y, 
                           color = "User Line", 
                           label = User_Residual), 
                       linetype = "dashed", 
                       linewidth = 0.25)+ 
        labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color", "label")
        ggplotly(g, tooltip = tooltip)
        ##main plot, LS line, and LS resids
      } else if(!("Show User Line Residuals" %in% input$add_to_plot) & ("Show Least Squares Line" %in% input$add_to_plot) & ("Show Least Squares Line Residuals" %in% input$add_to_plot)){
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point(aes(label = Least_Squares_Residual)) +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) +
          geom_smooth(method = "lm", 
                      se = FALSE, 
                      aes(color = "Least Squares Line")) +
          geom_segment(aes(x = x_values, 
                           xend = x_values, 
                           y = true_y, 
                           yend = ls_y, 
                           color = "Least Squares Line", 
                           label = Least_Squares_Residual), 
                       linetype = "dotted", 
                       linewidth = 0.25) + 
          labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color", "label")
        ggplotly(g, tooltip = tooltip)
        ##all things!
      } else if(("Show User Line Residuals" %in% input$add_to_plot) & ("Show Least Squares Line" %in% input$add_to_plot) & ("Show Least Squares Line Residuals" %in% input$add_to_plot)){
        g <- ggplot(slr_data, 
                    aes_string(x = isolate(input$slr_x), 
                               y = isolate(input$slr_y))) +
          geom_point(aes(label = User_Residual, 
                         label2 = Least_Squares_Residual)) +
          geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                      y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                    aes(x = x, 
                        y = y, 
                        color = "User Line")) +
          geom_smooth(method = "lm", 
                      se = FALSE, 
                      aes(color = "Least Squares Line")) +
          geom_segment(aes(x = x_values, 
                           xend = x_values,
                           y = true_y,
                           yend = ls_y, 
                           color = "Least Squares Line", 
                           label2 = Least_Squares_Residual), 
                       linetype = "dotted", 
                       linewidth = 0.25) + 
          geom_segment(aes(x = x_values, 
                           xend = x_values, 
                           y = true_y, 
                           yend = user_y, 
                           color = "User Line", 
                           label = User_Residual), 
                       linetype = "dashed", 
                       linewidth = 0.25)+ 
          labs(color = "Legend") + 
          scale_color_manual(values = colors)
        tooltip <- c("x", "y", "color", "label", "label2")
        ggplotly(g, tooltip = tooltip)
      }
        
    })
    
    output$slr_info <- renderTable({
      if(!input$slr_sample){
        NULL
      } else {
        #find the SSE for the user line
        slr_data <- sample_slr$slr_data
        user_line <- function(x){
          input$slr_int + input$slr_slope * x
        }
        x_values <- slr_data |> 
          pull(isolate(input$slr_x))
        user_y <- user_line(x_values)
        true_y <- slr_data |>
          pull(isolate(input$slr_y))
        user_resids <- true_y - user_y
        results <- data.frame("Line" = "User Line", "DF" = as.integer(isolate(input$slr_n)-2), "SSE" = round(sum(user_resids^2), 2), "MSE" = round(sum(user_resids^2)/(isolate(input$slr_n)-2), 2), "RMSE" = round(sqrt(sum(user_resids^2)/(isolate(input$slr_n)-2)), 2))
        if((("Show Least Squares Line" %in% input$add_to_plot) & (input$tabset1 == "Scatter Plot with Line(s)")) | ((input$tabset1 == "Residual Plot(s)") & input$plot_slr_resid) | ((input$tabset1 == "QQ Plot(s)") & input$plot_slr_qq)){
          fit <- sample_slr$slr_ls
          coefs <- coef(fit)
          ls_y <- coefs[1] + coefs[2]*x_values
          true_y <- slr_data |>
            pull(isolate(input$slr_y))
          ls_resids <- true_y - ls_y
          results[2, ] <- c("Least Squares Line", 
                            isolate(input$slr_n)-2,
                            round(sum(ls_resids^2), 4), 
                            round(sum(ls_resids^2)/(isolate(input$slr_n)-2), 4),
                            round(sqrt(sum(ls_resids^2)/(isolate(input$slr_n)-2)), 2))
        }
        results
      }
    })
    
    
    #create ls output
    output$slr_ls_info <- renderTable({
      if (
        (("Show Least Squares Line" %in% input$add_to_plot) & (input$tabset1 == "Scatter Plot with Line(s)")) | 
        ((input$tabset1 == "Residual Plot(s)") & input$plot_slr_resid) | 
        ((input$tabset1 == "QQ Plot(s)") & input$plot_slr_qq)
        ){
        #find the SSE for the user line
        fit <- sample_slr$slr_ls
        temp_df <- as.data.frame(summary(fit)$coefficients)
        temp_df$Parameter <- c("Intercept", "Slope") 
        temp_df |>
          select(Parameter, everything())
      } else {
        NULL
      }
      })


    
    #Create graph
    output$slr_residual <- renderPlotly({
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
        pull(isolate(input$slr_x))
      user_y <- user_line(x_values)
      true_y <- slr_data |>
        pull(isolate(input$slr_y))
      user_resids <- true_y - user_y
      
      if(!input$plot_slr_resid){
        #create one resid plot
        resid_df <- data.frame(x = x_values, y = user_resids)
        g <- ggplot(resid_df, aes(x = x, y = y)) +
          geom_point() +
          geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0)
        ggplotly(g)
      } else {
        fit <- sample_slr$slr_ls
        coefs <- coef(fit)
        ls_y <- coefs[1] + coefs[2]*x_values
        true_y <- slr_data |>
          pull(isolate(input$slr_y))
        ls_resids <- true_y - ls_y
        
        resid_df <- data.frame(x = rep(x_values, 2), y = c(user_resids, ls_resids), line = factor(c(rep("User", length(x_values)), rep("Least Squares", length(x_values))), levels = c("User", "Least Squares")))
        g <- ggplot(resid_df, aes(x = x, y = y)) +
          geom_point() +
          geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0) +
          facet_grid(~line)
        ggplotly(g)
      }
    })

    
    #Create graph
    output$slr_qq <- renderPlotly({
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
        pull(isolate(input$slr_x))
      user_y <- user_line(x_values)
      true_y <- slr_data |>
        pull(isolate(input$slr_y))
      user_resids <- true_y - user_y
      
      if(!input$plot_slr_qq){
        #create one resid plot
        resid_df <- data.frame(x = x_values, y = user_resids)
        g <- ggplot(resid_df, aes(sample = y)) +
          stat_qq() +
          stat_qq_line()
        ggplotly(g)
      } else {
        fit <- sample_slr$slr_ls
        coefs <- coef(fit)
        ls_y <- coefs[1] + coefs[2]*x_values
        true_y <- slr_data |>
          pull(isolate(input$slr_y))
        ls_resids <- true_y - ls_y
        
        resid_df <- data.frame(x = rep(x_values, 2), y = c(user_resids, ls_resids), line = factor(c(rep("User", length(x_values)), rep("Least Squares", length(x_values))), levels = c("User", "Least Squares")))
        g <- ggplot(resid_df, aes(sample = y)) +
          stat_qq() +
          stat_qq_line() +
          facet_grid(~line)
        ggplotly(g)
      }
    })
#################################################3
##Group SLR Stuff
sample_group <- reactiveValues(group_data = NULL, group_ls = NULL)

#update input boxes so they can't choose the same variable
observeEvent(c(input$group_x, input$group_y), {
  group_x <- input$group_x
  group_y <- input$group_y
  choices <- numeric_vars
  if (group_x == group_y){
    choices <- choices[-which(choices == group_x)]
    updateSelectizeInput(session,
                         "group_y",
                         choices = choices)
  }
})

#make sure two variables are selected 
observeEvent(input$group_sample, {
  
  group_vars <- c(input$group_x, input$group_y)
  
  subsetted_data <- my_sample |>
    mutate(SCHLfac = ifelse(SCHLfac %in% SCHLvals[as.character(20:24)],"College","No College")) |>
    filter(#cat vars first
      HHLfac %in% c("English Only","Spanish"),
      FSfac %in% c("Yes","No")
    ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
    {if("WKHP" %in% group_vars) filter(., WKHP > 0) else .} %>%
    {if("VALP" %in% group_vars) filter(., !is.na(VALP)) else .} %>%
    {if("TAXAMT" %in% group_vars) filter(., !is.na(TAXAMT)) else .} %>%
    {if("GRPIP" %in% group_vars) filter(., GRPIP > 0) else .} %>%
    {if("GASP" %in% group_vars) filter(., GASP > 0) else .} %>%
    {if("ELEP" %in% group_vars) filter(., ELEP > 0) else .} %>%
    {if("WATP" %in% group_vars) filter(., WATP > 0) else .} %>%
    {if("PINCP" %in% group_vars) filter(., AGEP > 18) else .} %>%
    {if("JWMNP" %in% group_vars) filter(., !is.na(JWMNP)) else .} 
    
    
  
  index <- sample(1:nrow(subsetted_data), 
                  size = input$group_n, 
                  replace = TRUE, 
                  prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
  sample_group$group_data <- subsetted_data[index, ]
  sample_group$group_ls <- lm(get(input$group_y) ~ get(input$group_x), data = sample_group$group_data)

    
  })

output$group_scatter <- renderPlot({
  validate(
    need(!is.null(input$groups_comp) , "Please select a group"),
    need(!is.null(sample_group$group_data), "Please click the 'Get a Sample!' button")
  ) #### Trying to have message show up before plot is made, having trouble here
  
  if(input$groups_comp=="snap"){
      #data and user values for line
      group_data <- sample_group$group_data %>% group_by(FSfac)
      print(head(group_data))
      #values for plotting purposes
      x_values <- group_data |> 
        pull(input$group_x)
      x_min <- min(x_values)
      x_max <- max(x_values)
      
      
      ####Having trouble getting multiple lines with different colors with aes_string
      g <- ggplot(sample_group$group_data, aes_string(x = isolate(input$group_x), y = isolate(input$group_y))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
      
      g
      
      
    }
  if (input$groups_comp=="college"){
      #data and user values for line
      group_data <- sample_group$group_data %>% group_by(SCHLfac)
      
      #values for plotting purposes
      x_values <- group_data |> 
        pull(input$group_x)
      x_min <- min(x_values)
      x_max <- max(x_values)
      
      ####Having trouble getting multiple lines with different colors with aes_string
      g <- ggplot(sample_group$group_data, aes_string(x = isolate(input$group_x), y = isolate(input$group_y))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
      
      g
      
      
    }
  if (input$groups_comp=="lang"){
      #data and user values for line
      group_data <- sample_group$group_data %>% group_by(HHLfac)
      
      #values for plotting purposes
      x_values <- group_data |> 
        pull(input$group_x)
      x_min <- min(x_values)
      x_max <- max(x_values)
      
      ####Having trouble getting multiple lines with different colors with aes_string
      g <- ggplot(sample_group$group_data, aes_string(x = isolate(input$group_x), y = isolate(input$group_y))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
      
      g
      
  }
})

}



