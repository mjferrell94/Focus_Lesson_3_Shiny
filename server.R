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
                                    select(all_of(corr_vars)))[1,2]
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
    
    #update graphs
    observeEvent(input$add_to_plot, {
        if(!("Show Least Squares Line" %in% isolate(input$add_to_plot))){
          updateCheckboxGroupInput(session, "add_to_plot", choices = c("Show User Line Residuals", "Show Least Squares Line"), inline = TRUE, selected = isolate(input$add_to_plot))
        }
        if("Show Least Squares Line" %in% isolate(input$add_to_plot)){
          updateCheckboxGroupInput(session, "add_to_plot", choices = c("Show User Line Residuals", "Show Least Squares Line", "Show Least Squares Line Residuals"), selected = isolate(input$add_to_plot), inline = TRUE)
        }
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
      colors <- c("User Line" = "green", "Least Squares Line" = "blue")
      #values for plotting purposes
      x_values <- slr_data |> 
        pull(input$slr_x)
      x_min <- min(x_values)
      x_max <- max(x_values)
      y_min <- min(c(user_line(x_min), user_line(x_max)))
      y_max <- max(c(user_line(x_min), user_line(x_max)))
      g <- ggplot(sample_slr$slr_data, aes_string(x = isolate(input$slr_x), y = isolate(input$slr_y))) +
        geom_point() +
        geom_line(data = data.frame(x = c(x_min, x_max), y = c(user_line(x_min), user_line(x_max))), aes(x = x, y = y, color = "User Line"))
      if("Show User Line Residuals" %in% input$add_to_plot){
        #find the deviations from the user line to each data point
        user_y <- user_line(x_values)
        true_y <- slr_data |>
          pull(input$slr_y)
        user_resids <- true_y - user_y
        user_resid_df <- data.frame(x = x_values, true_y = true_y, user_y = user_y)
        g <- g +
          geom_segment(data = user_resid_df, aes(x = x_values, xend = x_values, y = true_y, yend = user_y, color = "User Line"), lty = "dashed")
      }
      if("Show Least Squares Line" %in% input$add_to_plot){
        g <- g +
          geom_smooth(method = "lm", se = FALSE, aes(color = "Least Squares Line"))
        if("Show Least Squares Line Residuals" %in% input$add_to_plot){
          #find the deviations from the LS line to each data point
          fit <- sample_slr$slr_ls
          coefs <- coef(fit)
          ls_y <- coefs[1] + coefs[2]*x_values
          true_y <- slr_data |>
            pull(input$slr_y)
          ls_resids <- true_y - ls_y
          ls_resid_df <- data.frame(x = x_values, true_y = true_y, ls_y = ls_y)
          g <- g +
            geom_segment(data = ls_resid_df, aes(x = x_values, xend = x_values, y = true_y, yend = ls_y, color = "Least Squares Line"))
        }
      }
      g + 
        labs(color = "Legend") + 
        scale_color_manual(values = colors)
        
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
          pull(input$slr_x)
        user_y <- user_line(x_values)
        true_y <- slr_data |>
          pull(input$slr_y)
        user_resids <- true_y - user_y
        results <- data.frame("Line" = "User Line", "SSE" = round(sum(user_resids^2), 2), "MSE" = round(sum(user_resids^2)/(input$slr_n-2), 2), "RMSE" = round(sqrt(sum(user_resids^2)/(input$slr_n-2)), 2))
        if((("Show Least Squares Line" %in% input$add_to_plot) & (input$tabset1 == "Scatter Plot with Line(s)")) | ((input$tabset1 == "Residual Plot(s)") & input$plot_slr_resid)){
          fit <- sample_slr$slr_ls
          coefs <- coef(fit)
          ls_y <- coefs[1] + coefs[2]*x_values
          true_y <- slr_data |>
            pull(input$slr_y)
          ls_resids <- true_y - ls_y
          results[2, ] <- c("Least Squares Line", 
                            round(sum(ls_resids^2), 4), 
                            round(sum(ls_resids^2)/(input$slr_n-2), 4),
                            round(sqrt(sum(ls_resids^2)/(input$slr_n-2)), 2))
          print(summary(fit)$sigma)
        }
        results
      }
    })
    
    #Create graph
    output$slr_residual <- renderPlot({
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
      user_y <- user_line(x_values)
      true_y <- slr_data |>
        pull(input$slr_y)
      user_resids <- true_y - user_y
      
      if(!input$plot_slr_resid){
        #create one resid plot
        resid_df <- data.frame(x = x_values, y = user_resids)
        ggplot(resid_df, aes(x = x, y = y)) +
          geom_point() +
          geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0)
      } else {
        fit <- sample_slr$slr_ls
        coefs <- coef(fit)
        ls_y <- coefs[1] + coefs[2]*x_values
        true_y <- slr_data |>
          pull(input$slr_y)
        ls_resids <- true_y - ls_y
        
        resid_df <- data.frame(x = rep(x_values, 2), y = c(user_resids, ls_resids), line = factor(c(rep("User", length(x_values)), rep("Least Squares", length(x_values))), levels = c("User", "Least Squares")))
        ggplot(resid_df, aes(x = x, y = y)) +
          geom_point() +
          geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0) +
          facet_grid(~line)
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

#make sure two variables are selected #Error here for some reason
observeEvent(input$group_sample, {
  
  group_vars <- c(input$group_x, input$group_y)
  
  subsetted_data <- my_sample |>
    mutate(SCHLvals = ifelse(SCHLvals %in% SCHLvals[as.character(20:24)],"College","No College")) |>
    filter(#cat vars first
      HHLvals %in% c("English Only","Spanish"),
      FSvals %in% c("Yes","No")
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

}

