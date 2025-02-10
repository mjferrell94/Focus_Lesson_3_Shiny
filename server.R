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

#get rid of scientific notation...
options(scipen =6)

my_sample <- readRDS("my_sample_temp.rds")

#function to map df variable names to descriptive ones
get_descriptive_name <- function(var) {
  names(numeric_vars)[which(var == numeric_vars)]
}

function(input, output, session) {
  
  
#################################################
##Correlation tab
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  #update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices2 <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices2)
    } else {
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices[-which(choices == corr_y)],
                           selected = corr_x
                           )
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices[-which(choices == corr_x)],
                           selected = corr_y
      )
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
      geom_point() +
      xlab(get_descriptive_name(isolate(input$corr_x))) +
      ylab(get_descriptive_name(isolate(input$corr_y)))
    #Format some labels
    if(input$corr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
      g <- g + 
        scale_x_continuous(labels = scales::label_dollar())
    }
    if(input$corr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
      g <- g + 
        scale_y_continuous(labels = scales::label_dollar())
    }
    
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

    
########################################################
##SLR stuff
    sample_slr <- reactiveValues(slr_data = NULL, slr_ls = NULL, slr_user = NULL, variable_issue = FALSE)

    #update input boxes so they can't choose the same variable
    observeEvent(c(input$slr_x, input$slr_y), {
      slr_x <- input$slr_x
      slr_y <- input$slr_y
      choices <- numeric_vars
      if (slr_x == slr_y){
        choices2 <- choices[-which(choices == slr_x)]
        updateSelectizeInput(session,
                             "slr_y",
                             choices = choices2)
      } else {
        updateSelectizeInput(session,
                             "slr_x",
                             choices = choices[-which(choices == slr_y)],
                             selected = slr_x
        )
        updateSelectizeInput(session,
                             "slr_y",
                             choices = choices[-which(choices == slr_x)],
                             selected = slr_y
        )
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
        
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
        
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
        
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
        
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
          scale_color_manual(values = colors) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab(get_descriptive_name(isolate(input$slr_y)))
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
        if(input$slr_y %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_y_continuous(labels = scales::label_dollar())
        }
        
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
                            round(sum(ls_resids^2), 2), 
                            round(sum(ls_resids^2)/(isolate(input$slr_n)-2), 2),
                            round(sqrt(sum(ls_resids^2)/(isolate(input$slr_n)-2)), 2))
        }
        results
      }
    }, digits = 2)
    
    
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
      }, digits = 4)


    
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
          geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0) +
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab("Predicted")
        #Format some labels
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
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
          xlab(get_descriptive_name(isolate(input$slr_x))) +
          ylab("Predicted") +
          facet_grid(~line)
        if(input$slr_x %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
          g <- g + 
            scale_x_continuous(labels = scales::label_dollar())
        }
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
          stat_qq_line() + 
          
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
    
#################################################
##Group SLR Stuff
  sample_group <- reactiveValues(group_data = NULL, r2 = NULL, r2full = NULL, fit_full = NULL)
  
  #update input boxes so they can't choose the same variable
  observeEvent(c(input$group_x, input$group_y), {
    group_x <- input$group_x
    group_y <- input$group_y
    choices <- numeric_vars
    if (group_x == group_y){
      choices2 <- choices[-which(choices == group_x)]
      updateSelectizeInput(session,
                           "group_y",
                           choices = choices2)
    } else {
      updateSelectizeInput(session,
                           "group_x",
                           choices = choices[-which(choices == group_y)],
                           selected = group_x
      )
      updateSelectizeInput(session,
                           "group_y",
                           choices = choices[-which(choices == group_x)],
                           selected = group_y
      )
    }
    if (((input$group_x == "GRPIP") & (input$group_y %in% c("TAXAMT", "VALP"))) | ((input$group_y == "GRPIP") & (input$group_x %in% c("TAXAMT", "VALP")))){
      shinyalert(title = "Oh no!", "Those with Property taxes and/or Property Values usually don't have a rent payment. Please select a different combination of variables.", type = "error")
      updateSelectizeInput(session,
                           "group_x",
                           choices = choices[-2],
                           selected = choices[1]
      )
      updateSelectizeInput(session,
                           "group_y",
                           choices = choices[-1],
                           selected = choices[2]
      )
    }
  })
  
  
  #make sure two variables are selected 
  observeEvent(input$group_sample, {
    
    if(is.null(input$groups_comp)){
      shinyalert(title = "Grouping Needed!", "Please use the radio buttons to select a group first.", type = "error")
    } else {
      groups_comp <- input$groups_comp
      if (groups_comp == "lang") {
        group_variable <- "HHLfac"
      } else if(groups_comp == "snap"){
        group_variable <- "FSfac"
      } else if(groups_comp == "school"){
        group_variable <- "SCHLfac"
      }
      group_vars <- c(input$group_x, input$group_y)
      
      subsetted_data <- my_sample |>
        mutate(SCHLfac = ifelse(SCHLfac %in% SCHLvals[as.character(20:24)],"College","No College")) %>%
        #filter(#cat vars first
        #  HHLfac %in% c("English Only","Spanish"),
        #  FSfac %in% c("Yes","No")
        #) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
        {if("WKHP" %in% group_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% group_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% group_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% group_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% group_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% group_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% group_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% group_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% group_vars) filter(., !is.na(JWMNP)) else .} 
        
      #if lang selected, subset data appropriately
      if (groups_comp == "lang") {#filter to have only english and spanish
        subsetted_data <- subsetted_data |>
          filter(HHLfac %in% c("English Only", "Spanish"))
      }
      
      #make sure we get at least four observations per group so we can fit our line with some reasonable df
      four_values <- FALSE
      while(!four_values){
        index <- sample(1:nrow(subsetted_data), 
                        size = input$group_n, 
                        replace = TRUE, 
                        prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
        temp <- sample_group$group_data <- subsetted_data[index, ]
        temp_counts <- temp |>
          group_by(get(group_variable)) |>
          summarize(count = n())
        temp_counts
        if(length(temp_counts$count) > 1){
          if(temp_counts$count[1] >4 & temp_counts$count[2] > 4){
            four_values <- TRUE
          }
        }
      }
      sample_group$group_data <- temp
      #fit linear models
      #just slr
      # sample_group$group_joint_lm <- lm(get(input$group_y) ~ get(input$group_x), data = sample_group$group_data)
      # #on just 1st group
      # sample_group$sub1 <- sample_group$group_data %>%
      #   {if(groups_comp == "lang") filter(., HHLfac == "English Only") else .} %>%
      #   {if(groups_comp == "school") filter(., SCHLfac == "College") else .} %>%
      #   {if(groups_comp == "snap") filter(., FSfac == "Yes") else .}
      # sample_group$group_1_lm <- lm(get(input$group_y) ~ get(input$group_x), data = sample_group$sub1)
      # #on just 2nd group
      # sample_group$sub2 <- sample_group$group_data %>%
      #   {if(groups_comp == "lang") filter(., HHLfac == "Spanish") else .} %>%
      #   {if(groups_comp == "school") filter(., SCHLfac == "No College") else .} %>%
      #   {if(groups_comp == "snap") filter(., FSfac == "No") else .}
      # sample_group$group_2_lm <- lm(get(input$group_y) ~ get(input$group_x), data = sample_group$sub2)
      # #with different intercepts
      # if(groups_comp == "lang") {
      #   int_formula <- formula(get(input$group_y) ~ get(input$group_x) + HHLfac)
      # } else if (groups_comp == "school") {
      #   int_formula <- formula(get(input$group_y) ~ get(input$group_x) + SCHLfac)
      # } else {
      #   int_formula <- formula(get(input$group_y) ~ get(input$group_x) + FSfac)
      # }
      # sample_group$group_intercepts_lm <- lm(int_formula, data = sample_group$group_data)
      # #with different intercepts and slopes
      # if(groups_comp == "lang") {
      #   full_formula <- formula(get(input$group_y) ~ get(input$group_x)*HHLfac)
      # } else if (groups_comp == "school") {
      #   full_formula <- formula(get(input$group_y) ~ get(input$group_x)*SCHLfac)
      # } else {
      #   full_formula <- formula(get(input$group_y) ~ get(input$group_x)*FSfac)
      # }
      # sample_group$group_full_lm <- lm(full_formula, data = sample_group$group_data)
    }
    })
  
  
  output$group_scatter <- renderPlotly({

    groups_comp <- isolate(input$groups_comp)
    
    validate(
      need(!is.null(groups_comp) , "Please select a group"),
      need(!is.null(sample_group$group_data), "Please click the 'Get a Sample!' button")
    )

    
    x_variable <- isolate(input$group_x)
    y_variable <- isolate(input$group_y)
    
    if(groups_comp == "snap"){
        #data and user values for line
        group_data <- sample_group$group_data 
        
        g <- ggplot(group_data, aes_string(x = x_variable, y = y_variable)) +
          geom_point(aes(col = FSfac)) +
          scale_color_discrete(name = "SNAP Benefit?")
        if(input$groups_fit == "Separate SLR Fits") { 
          g <- g +
            geom_smooth(method = "lm", aes(col = FSfac), se = FALSE) 
        } else if(input$groups_fit == "SLR Fit") {
          g <- g +
            geom_smooth(method = "lm", se = FALSE)
        }
      } else if (groups_comp=="school"){
        #data and user values for line
        group_data <- sample_group$group_data 
        
        g <- ggplot(group_data, aes_string(x = x_variable, y = y_variable)) +
          geom_point(aes(col = SCHLfac)) +
          scale_color_discrete(name = "Education")
        if(input$groups_fit == "Separate SLR Fits") { 
          g <- g +
            geom_smooth(method = "lm", aes(col = SCHLfac), se = FALSE) 
        } else if(input$groups_fit == "SLR Fit") {
          g <- g +
            geom_smooth(method = "lm", se = FALSE)
        }
      } else if (groups_comp =="lang"){
        #data and user values for line
        group_data <- sample_group$group_data 
        
        g <- ggplot(group_data, aes_string(x = x_variable, y = y_variable)) +
          geom_point(aes(col = HHLfac)) +
          scale_color_discrete(name = "Household Language")
        if(input$groups_fit == "Separate SLR Fits") { 
          g <- g +
            geom_smooth(method = "lm", aes(col = HHLfac), se = FALSE) 
        } else if(input$groups_fit == "SLR Fit") {
          g <- g +
            geom_smooth(method = "lm", se = FALSE)
        }
      }
    g <- g +
      xlab(get_descriptive_name(x_variable)) +
      ylab(get_descriptive_name(y_variable))
    if(x_variable %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
      g <- g + 
        scale_x_continuous(labels = scales::label_dollar())
    }
    if(y_variable %in% c("PINCP", "WATP", "ELEP", "GASP", "GRPIP", "TAXAMT", "VALP")){
      g <- g + 
        scale_y_continuous(labels = scales::label_dollar())
    }
    tooltip <- c("x", "y", "color")
    ggplotly(g, tooltip = tooltip)
  })

  #produce coef table
  output$groups_info <- renderTable({
    if(input$groups_fit == "None"){
      NULL
    } else if(input$groups_fit == "Separate SLR Fits"){
      #fit SLR model 
      x_variable <- isolate(input$group_x)
      y_variable <- isolate(input$group_y)
      group <- isolate(input$groups_comp)
      if (group == "lang") {
        group_variable <- "HHLfac"
        form <- formula(get(y_variable) ~ get(x_variable)*get(group_variable))
        fit <- lm(form, data = sample_group$group_data)
        #late add to just grab R2 as a reactive value
        sample_group$r2full <- summary(fit)$r.squared
        sample_group$fit_full <- fit
        results <- summary(fit)$coefficients
        dimnames(results)[[1]] <- c("Intercept", "Slope", "Difference in Intercept for Spanish", "Difference in Slope for Spanish")
        round(results, 5)
      } else if(group == "snap"){
        group_variable <- "FSfac"
        form <- formula(get(y_variable) ~ get(x_variable)*get(group_variable))
        fit <- lm(form, data = sample_group$group_data)
        #late add to just grab R2 as a reactive value
        sample_group$r2full <- summary(fit)$r.squared
        sample_group$fit_full <- fit
        
        results <- summary(fit)$coefficients
        dimnames(results)[[1]] <- c("Intercept", "Slope", "Difference in Intercept for No SNAP", "Modification to Slope for No SNAP")
        round(results, 5)
      } else if(group == "school"){
        group_variable <- "SCHLfac"
        form <- formula(get(y_variable) ~ get(x_variable)*get(group_variable))
        fit <- lm(form, data = sample_group$group_data)
        #late add to just grab R2 as a reactive value
        sample_group$r2full <- summary(fit)$r.squared
        sample_group$fit_full <- fit
        
        results <- summary(fit)$coefficients
        dimnames(results)[[1]] <- c("Intercept", "Slope", "Difference in Intercept for No College", "Difference in Slope for No College")
        round(results, 5)
      }
    } else if(input$groups_fit == "SLR Fit"){
      x_variable <- isolate(input$group_x)
      y_variable <- isolate(input$group_y)
      form <- formula(get(y_variable) ~ get(x_variable))
      fit <- lm(form, data = sample_group$group_data)
      #late add to just grab R2 as a reactive value
      sample_group$r2 <- summary(fit)$r.squared
      results <- summary(fit)$coefficients
      dimnames(results)[[1]] <- c("Intercept", "Slope")
      round(results, 5)
    }
  }, digits = 4, rownames = TRUE)
  
  #produce R^2...
  output$r2 <- renderTable({
    if(input$groups_fit == "SLR Fit"){
      data.frame("R Squared" = sample_group$r2)
    } else if(input$groups_fit == "Separate SLR Fits"){
      #obtain R^2 for basic model
      x_variable <- isolate(input$group_x)
      y_variable <- isolate(input$group_y)
      form <- formula(get(y_variable) ~ get(x_variable))
      fit <- lm(form, data = sample_group$group_data)
      #late add to just grab R2 as a reactive value
      sample_group$r2 <- summary(fit)$r.squared
      data.frame("Model" = c("Basic SLR", "Separate SLR"),
                 "R Squared" = c(sample_group$r2, sample_group$r2full))
    }
  }, digits = 4)
  
  
  #give equations for line(s)
  output$groups_fits <- renderUI({
    if(input$groups_fit == "None"){
      NULL
    } else if(input$groups_fit == "Separate SLR Fits"){
      #fit SLR model 
      x_variable <- isolate(input$group_x)
      y_variable <- isolate(input$group_y)
      group <- isolate(input$groups_comp)
      if (group == "lang") {
        group_variable <- "HHLfac"
      } else if(group == "snap"){
        group_variable <- "FSfac"
      } else if(group == "school"){
        group_variable <- "SCHLfac"
      }
      form <- formula(get(y_variable) ~ get(x_variable)*get(group_variable))
      fit <- lm(form, data = sample_group$group_data)
      coefs <- coef(fit)
      if (group == "lang") {
        x_name <- names(numeric_vars)[which(numeric_vars == x_variable)]
        withMathJax(paste0("Fitted equation for 'English Only':$$\\hat{y} \\approx ", round(coefs[1], 5), " + ", round(coefs[2], 5), "*(\\mbox{", x_name, "})$$Fitted equation for 'Spanish':$$\\hat{y} \\approx ", round(coefs[1]+coefs[3], 5), " + ", round(coefs[2]+coefs[4], 5), "*(\\mbox{", x_name, "})$$"))
      } else if(group == "snap"){
        x_name <- names(numeric_vars)[which(numeric_vars == x_variable)]
        withMathJax(paste0("Fitted equation for 'SNAP':$$\\hat{y} \\approx ", round(coefs[1], 5), " + ", round(coefs[2], 5), "*(\\mbox{", x_name, "})$$Fitted equation for 'No SNAP':$$\\hat{y} \\approx ", round(coefs[1]+coefs[3], 5), " + ", round(coefs[2]+coefs[4], 5), "*(\\mbox{", x_name, "})$$"))
      } else if(group == "school"){
        x_name <- names(numeric_vars)[which(numeric_vars == x_variable)]
        withMathJax(paste0("Fitted equation for 'College':$$\\hat{y} \\approx ", round(coefs[1], 5), " + ", round(coefs[2], 5), "*(\\mbox{", x_name, "})$$Fitted equation for 'No College':$$\\hat{y} \\approx ", round(coefs[1]+coefs[3], 5), " + ", round(coefs[2]+coefs[4], 5), "*(\\mbox{", x_name, "})$$"))
      }
    } else if(input$groups_fit == "SLR Fit"){
      x_variable <- isolate(input$group_x)
      y_variable <- isolate(input$group_y)
      form <- formula(get(y_variable) ~ get(x_variable))
      fit <- lm(form, data = sample_group$group_data)
      coefs <- coef(fit)
      x_name <- names(numeric_vars)[which(numeric_vars == x_variable)]
      withMathJax(paste0("Fitted equation:$$\\hat{y} \\approx ", round(coefs[1], 4), " + ", round(coefs[2], 5), "*(\\mbox{", x_name, "})$$"))
    }
  })
  
  #prediction part
  #slider
  output$group_preds <- renderUI({
    g_data <- sample_group$group_data
    x_variable <- isolate(input$group_x)
    y_variable <- isolate(input$group_y)
    
    sliderInput("pred_x", paste0("Value of ", names(numeric_vars)[which(numeric_vars == x_variable)], " to predict for:"), min = min(g_data[[x_variable]]), max = max(g_data[[x_variable]]), value = mean(g_data[[x_variable]]))
    })
  
  #preds
  output$pred_out <- renderTable({
    x_variable <- isolate(input$group_x)
    y_variable <- isolate(input$group_y)
    if(input$groups_fit == "Separate SLR Fits"){
      fit <- sample_group$fit_full
      if(!is.null(input$pred_x)){
        reference_pred <- fit$coefficients[1] + fit$coefficients[2]*input$pred_x
        non_reference_pred <- fit$coefficients[1] + fit$coefficients[3] + fit$coefficients[2]*input$pred_x + fit$coefficients[4]*input$pred_x
        #group levels
        group <- isolate(input$groups_comp)
        if (group == "lang") {
          data.frame("Group" = c("Spanish", "English"), 
                     "Prediction" = c(reference_pred, non_reference_pred))
        } else if(group == "snap"){
          data.frame("Group" = c("Snap", "No Snap"), 
                     "Prediction" = c(reference_pred, non_reference_pred))
        } else if(group == "school"){
          data.frame("Group" = c("College", "No College"), 
                     "Prediction" = c(reference_pred, non_reference_pred))
        }
      } else {
        NULL
      }
    } else {
      form <- formula(get(y_variable) ~ get(x_variable))
      fit <- lm(form, data = sample_group$group_data)
      #prediction setup
      if(!is.null(input$pred_x)){
        prediction <- fit$coefficients[1] + fit$coefficients[2]*input$pred_x
        names(prediction) <- names(numeric_vars)[which(numeric_vars == y_variable)]
        data.frame("Prediction" = prediction)
      }else {
        NULL
      }
    }
  }, rownames = TRUE)
  
}



