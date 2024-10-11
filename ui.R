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
library(shinyalert)
library(shinyjs)
library(tidyverse)
library(plotly)

source("helpers.R")

#my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"

# # #get big sample for the first part that is common to everyone
#switching to loop due to error. (turns out it was a Puerto Rico issue - has a different API)
#oregon (41) returns a curl_fecth_memory error...
# my_sample <- list()
# for (i in 1:length(state_codes)){
#   if(i == 41){
#     my_sample[[i]] <- "Handle"
#     next
#   }
#   my_sample[[i]] <- get_sample(var = names(variable_info), state = names(state_codes)[i], year = 2022)
#   print(i/length(state_codes))
# }
#temp <- get_sample(var = names(variable_info), state = names(state_codes)[41], year = 2022)
#my_sample[[41]] <- temp
#saveRDS(my_sample, file = 'full_list.rds')

#combine list elements
#my_sample_all <- do.call(rbind, my_sample)

# #convert everything to factors & handle special cases
# my_sample2 <- my_sample %>%
#   mutate(HHLfac = factor(as.character(HHL), labels = HHLvals, levels = names(HHLvals)),
#           HHLANPfac = factor(as.character(HHLANP), labels = HHLANPvals, levels = names(HHLANPvals)),
#           LANPfac = factor(as.character(LANP), labels = LANPvals, levels = names(LANPvals)),
#           LANXfac = factor(as.character(LANX), labels = LANXvals, levels = names(LANXvals)),
#           FSfac = factor(as.character(FS), labels = FSvals, levels = names(FSvals)),
#           WAOBfac = factor(as.character(WAOB), labels = WAOBvals, levels = names(WAOBvals)),
#           FERfac = factor(as.character(FER), labels = FERvals, levels = names(FERvals)),
#           SCHLfac = factor(as.character(SCHL), labels = SCHLvals, levels = names(SCHLvals)),
#           SCHfac = factor(as.character(SCH), labels = SCHvals, levels = names(SCHvals)),
#           RAC1Pfac = factor(as.character(RAC1P), labels = RAC1Pvals, levels = names(RAC1Pvals)),
#           STfac = factor(as.character(ST), labels = state_names, levels = names(state_names)),
#          POWSPfac = factor(as.character(POWSP), labels = POWSPvals, levels = names(POWSPvals)),
#           AGEP = as.numeric(AGEP),
#            JWMNP = ifelse(JWMNP == 0 & POWSPfac != "N", NA, JWMNP), #only people that work have a numeric value
#            PINCP = ifelse(PINCP == -19999, 0, PINCP), #-19999 indicates less than 15 years old, otherwise valid values
#            WATP = ifelse(WATP == 2, 0, WATP), #2 indicates included in rent or NA/no charge
#            ELEP = ifelse(ELEP == 2, 0, ELEP), #2 indicates included in rent or NA/no charge
#            GASP = ifelse(GASP == 3, 0, GASP), #3 indicates included in rent or NA/no chart/not used
#            #GRPIP, 0 indicates NA (owned, being occupied without rent/no household income), 101 means more than 100%
#            TAXAMT = ifelse(TAXAMT == -1, NA, TAXAMT), #-1 NA/not owned/vacant
#            VALP = ifelse(VALP == 0, NA, VALP) #0 Vacant, no occupied/being bought
#            #WKHP 0 can be NA or didn't work
#           )

#saveRDS(my_sample_all, file = 'my_sample.rds')


dashboardPage(
  dashboardHeader(title="Regression Activity"),
  
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Correlation Exploration", tabName = "correlation", icon = icon("archive")),
      menuItem("Simple Linear Regression", tabName = "slr", icon = icon("laptop")),
      menuItem("Comparing Models", tabName = "compare", icon = icon("house"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "correlation",
              titlePanel("Correlation Exploration"),
              sidebarLayout(
                sidebarPanel(
                  h2("Select Variables to Find Correlation:"),
                  selectizeInput("corr_x",
                                 "x Variable",
                                 choices = numeric_vars[-1], 
                                 selected = numeric_vars[2]),
                  selectizeInput("corr_y",
                                 "y Variable",
                                 choices = numeric_vars[-2],
                                 selected = numeric_vars[1]),
                  h2("Choose a subset of the data:"),
                  radioButtons("hhl_corr",
                               "Household Language",
                               choiceValues = c("all", 
                                                "english",
                                                "spanish",
                                                "other"
                                                ),
                               choiceNames = c("All",
                                               "English only",
                                               "Spanish",
                                               "Other"
                                               )
                            ),
                  radioButtons("fs_corr",
                               "SNAP Recipient",
                               choiceValues = c("all", 
                                                "yes",
                                                "no"
                               ),
                               choiceNames = c("All",
                                               "Yes",
                                               "No"
                               )
                  ),
                  radioButtons("schl_corr",
                               "Educational attainment",
                               choiceValues = c("all", 
                                                "no_hs",
                                                "hs",
                                                "college"
                               ),
                               choiceNames = c("All",
                                               "High School not Completed",
                                               "High School or GED",
                                               "College Degree"
                               )
                  ),
                  h2("Select a Sample Size"),
                  sliderInput("corr_n", "", min = 20, max = 500, value = 20),
                  actionButton("corr_sample","Get a Sample!")
                ),
                mainPanel(
                  box(title = "Scatter Plot", 
                      status = "primary",
                      width = 12,
                      plotlyOutput("corr_scatter")
                  ),
                  conditionalPanel("input.corr_sample",
                                   box(title = "Guess the correlation!",
                                       width = 6,
                                       numericInput("corr_guess",
                                                    "",
                                                    value = 0,
                                                    min = -1, 
                                                    max = 1
                                                    )
                                          ),
                                   box(
                                     width = 4,
                                     actionButton("corr_submit", "Check Your Guess!")
                                     )
                  )
                )
              )
      ),
      tabItem(tabName = "slr",
              titlePanel("Simple Linear Regression"),
              sidebarLayout(
                sidebarPanel(
                  h2("Choose Your Variables:"),
                  selectizeInput("slr_x",
                                 "Explanatory (x) Variable",
                                 choices = numeric_vars[-1], 
                                 selected = numeric_vars[2]),
                  selectizeInput("slr_y",
                                 "Reponse (y) Variable",
                                 choices = numeric_vars[-2],
                                 selected = numeric_vars[1]),
                  h2("Choose a subset of the data:"),
                  radioButtons("hhl_slr",
                               "Household Language",
                               choiceValues = c("all", 
                                                "english",
                                                "spanish",
                                                "other"
                               ),
                               choiceNames = c("All",
                                               "English only",
                                               "Spanish",
                                               "Other"
                               )
                  ),
                  radioButtons("fs_slr",
                               "SNAP Recipient",
                               choiceValues = c("all", 
                                                "yes",
                                                "no"
                               ),
                               choiceNames = c("All",
                                               "Yes",
                                               "No"
                               )
                  ),
                  radioButtons("schl_slr",
                               "Educational attainment",
                               choiceValues = c("all", 
                                                "no_hs",
                                                "hs",
                                                "college"
                               ),
                               choiceNames = c("All",
                                               "High School not Completed",
                                               "High School or GED",
                                               "College Degree"
                               )
                  ),
                  h2("Select a Sample Size"),
                  sliderInput("slr_n", "", min = 20, max = 500, value = 20),
                  actionButton("slr_sample","Get a New Sample!")
                ),
                mainPanel(
                  fluidRow(
                    h3("Create your own line!")
                  ), 
                  fluidRow(
                    box(
                      width = 6,
                      sliderInput("slr_int", 
                                  "Intercept",
                                  min = -50,
                                  max = 50,
                                  value = 0)
                    ),
                    box(
                      width = 6,
                      sliderInput("slr_slope", 
                                  "Slope",
                                  min = -50,
                                  max = 50,
                                  value = 0)
                    )
                  ),
                  fluidRow(
                    tabBox(
                      id = "tabset1",
                      width = 12,
                      tabPanel("Scatter Plot with Line(s)", 
                               plotlyOutput("slr_scatter"),
                               conditionalPanel("input.slr_sample",
                                                checkboxGroupInput("add_to_plot", "", choices = c("Show User Line Residuals", "Show Least Squares Line"), inline = TRUE)
                                                )
                               ),
                      tabPanel("Residual Plot(s)", 
                               plotlyOutput("slr_residual"),
                               checkboxInput("plot_slr_resid", "Show Least Squares Line Residual Plot")
                               )
                      )
                    ),
                  fluidRow(
                    conditionalPanel("input.slr_sample",
                      box(
                        width = 9,
                        tableOutput("slr_info"),
                        conditionalPanel("input.add_to_plot.includes('Show Least Squares Line')",
                                         h4("Simple Linear Regression Fit"),
                                         tableOutput("slr_ls_info")
                                         )
                        )
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "compare",
              titlePanel("Comparing Models"),
              sidebarLayout(
                sidebarPanel(
                  h2("Choose Your Variables:"),
                  selectizeInput("group_x",
                                 "Explanatory (x) Variable",
                                 choices = numeric_vars[-1], 
                                 selected = numeric_vars[2]),
                  selectizeInput("group_y",
                                 "Reponse (y) Variable",
                                 choices = numeric_vars[-2],
                                 selected = numeric_vars[1]),
                  h2("Choose which groups to compare"),
                  radioButtons("groups_comp",
                               "Groups",
                               choiceValues = c("snap", 
                                                "school",
                                                "lang"
                          ),
                               choiceNames = c("SNAP vs no SNAP",
                                               "College vs no College",
                                               "English vs Spanish"
                               ), selected = character(0)
                  ),
                  h2("Select a Sample Size"),
                  sliderInput("group_n", "", min = 20, max = 500, value = 20),
                  actionButton("group_sample","Get a New Sample!")
                
                ),
                mainPanel(
                  fluidRow(
                    plotOutput("group_scatter")
                  )
                )
              )
      )
    )
  )
)

