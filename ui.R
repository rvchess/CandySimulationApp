#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    
    # This is the UI page for the Candy Simulation Model. None of the Logic is present on this page.
    # This page generates input/output boxes, plots, and event buttons. New buttons can be added
    # to either the main panel or the side panel. In the future more panels could be added to clean up
    # the UI.
    #
    #
    
    
    # Application title
    titlePanel("Candy Simulation Model"),
    sidebarLayout(
        sidebarPanel(
            numericInput("candy", "Number of Candies: ", value = 0, min = 1, max = NA, step = NA),
            
            numericInput("min_log", "Min log reduction: ", value = 0.5, min = NA, max = NA, step = NA),
            
            numericInput("max_log", "Max log reduction: ", value = 1.5, min = NA, max = NA, step = NA),
            
            numericInput("log_red_week", "Log Reduction per Week: ", value = 0, min = NA, max = NA, step = NA),
            
            numericInput("stor_week", "Number of Weeks in Storage: ", value = 0, min = 0, max = NA, step = NA),
            
            numericInput("candy_serving", "Candy Servings in Grams: ", value = 20, min = 0, max = NA, step = NA),
            
            numericInput("frac_fondant", "Fraction Fondant: ", value = 0.75, min = 0, max = 1, step = NA),
            
            numericInput("frac_peanut", "Fraction Peanut Paste in Fondant: ", value = 0.24, min = 0, max = 1, step = NA),
            
            numericInput("cfus", "CFUs: ", value = 0, min = 0, max = NA, step = NA),
            
            numericInput("pos_test", "Number of Positive Tests: ", value = 0, min = 0, max = NA, step = NA),
            
            numericInput("neg_test", "Number of Negative Tests: ", value = 0, min = 0, max = NA, step = NA),
            
            #actionButton("iterate", "Iterate Once"),
            #actionButton("iterate_many", "Generate Plot for 100 Iterations"),
            
            #textOutput("Initial_Conditions"),
            #actionButton("baseline_bad", "Baseline Bad Food Safety System"),
            #actionButton("onepos", "1 Positive Test"),
            #actionButton("onelog", "1 Log Reduction, Cooking"),
            #actionButton("twolog", "2 Log Reduction, Cooking"),
            #actionButton("fivelog", "5 Log Reduction, Cooking"),
            #actionButton("onewklog", "Log Reduction in 1 week shelf life"),
            #actionButton("fourwklog", "Log Reduction in 4 week shelf life"),
            #actionButton("baseline_real", "Baseline Realistic Contamination"),
            #actionButton("rec_much", "Rec. Much Sampling"),
            #actionButton("rec_twolog", "Rec. 2-log Cook Step and Storage"),
            #actionButton("rec_fivelog", "Rec. 5-log Cook Step")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            actionButton("iterate", "Iterate Once"),
            actionButton("iterate_many50", "Generate Plot for 50 Iterations"),
            actionButton("iterate_many", "Generate Plot for 100 Iterations"),
            textOutput("Number_Sick"),
            plotOutput("plotting50_output"),
            plotOutput("plotting_output"),
            sliderInput("probpositive", label = "Number of Negative Tests: ", min = 1, max = 100, value = 10, step = 1),
            plotOutput("probability_positive"),
            numericInput("parameter_1", label = "Parameter 1: ", value = 0.13, min = 0, max = 10, step = 0.01),
            numericInput("parameter_2", label = "Parameter 2: ", value = 51.45, min = 0, max = 100, step = 0.01),
            plotOutput("salmonella_dose")
        )
    )

    # Sidebar with a slider input for number of bins


        # Show a plot of the generated distribution
        
    )
)
