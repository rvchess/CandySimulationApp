#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#number_sick <- function(candy_on_market, min_log_red = 0.5, max_log_red = 1.5, log_reduction_per_week_storage, storage_weeks, candy_servings_grams = 20, fraction_fondant = 0.75, fraction_peanut_paste_fondant = 0.24, cells_per_gram_peanut_paste, positive_tests, negative_tests) {
#    B6 <- candy_servings_grams * fraction_fondant
#    B8 <- B6 * fraction_peanut_paste_fondant
#    C12 <- B8
#    C11 <- cells_per_gram_peanut_paste
#    C13 <- C11 * C12
#    C20 <- log_reduction_per_week_storage * storage_weeks
#    C17 <- runif(1, min_log_red, max_log_red)
#    C14 <- log10(C13)
    #C29 <- negative_tests + 1
    #C28 <- positive_tests + 1
    #C30 <- rbeta(1, C28, C29)
#    C30 <- rbeta(1, positive_tests + 1, negative_tests + 1)
#    C21 <- C14 - C17 - C20
#    C22 <- 10^(C21)
#    C35 <- rbinom(1, 1, C30)
#    C37 <- C35 * C22
#    C40 <- 0.13 #constant from literature
#    C41 <- 51.45 #constant from literature
#    C42 <- 1 - (1 + (C37/C41))^(-C40)
#    toReturn <- rbinom(candy_on_market, 1, C42)
#    return(toReturn)
#}

#THE FUNCTION ABOVE IS THE OLD MODEL. New model cuts run time by 50%.

number_sick <- function(candy_on_market, min_log_red = 0.5, max_log_red = 1.5, log_reduction_per_week_storage, storage_weeks, candy_servings_grams = 20, fraction_fondant = 0.75, fraction_peanut_paste_fondant = 0.24, cells_per_gram_peanut_paste, positive_tests, negative_tests) {
  B6 <- candy_servings_grams * fraction_fondant
  #browser()
  B8 <- B6 * fraction_peanut_paste_fondant
  C12 <- B8
  C11 <- cells_per_gram_peanut_paste
  C13 <- C11 * C12
  C20 <- log_reduction_per_week_storage * storage_weeks
  C17 <- runif(n = candy_on_market/7 , min = min_log_red, max = max_log_red)
  C14 <- log10(C13)
  #C29 <- negative_tests + 1
  #C28 <- positive_tests + 1
  #C30 <- rbeta(1, C28, C29)
  C30 <- rbeta(candy_on_market/7, positive_tests + 1, negative_tests + 1)
  C21 <- C14 - C17 - C20
  C22 <- 10^(C21)
  C35 <- rbinom(candy_on_market/7, 1, C30)
  C37 <- C35 * C22
  C40 <- 0.13 #constant from literature
  C41 <- 51.45 #constant from literature
  C42 <- 1 - (1 + (C37/C41))^(-C40)
  toReturn <- sapply(C42 , rbinom, size = 1, n = 7)
  #toReturn <- rbinom(candy_on_market, 1, C42)
  return(toReturn)
}


iteration <- function() {
  sum(
    replicate(n = 210000/7, expr = {
      number_sick(candy_on_market = 7, min_log_red = 0.5, max_log_red = 1.5, 
                  log_reduction_per_week_storage = 0, storage_weeks = 4, 
                  candy_servings_grams = candy_serving, fraction_fondant = fraction_fondant, 
                  fraction_peanut_paste_fondant = fraction_peanut_paste_fondant, 
                  cells_per_gram_peanut_paste = 150, positive_tests = 0, 
                  negative_tests = 10-0)
    })
  )
}

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #hello <- eventReactive(input$iterate, 
            #number_sick(input$candy, input$min_log, input$max_log, input$log_red_week, input$stor_week, input$candy_serving, input$frac_fondant, input$frac_peanut, input$cfus, input$pos_test, input$neg_test)
    #          toReturn <- sum(number_sick(candy_on_market = input$candy, min_log_red = input$min_log, max_log_red = input$max_log ,log_reduction_per_week_storage = input$log_red_week , storage_weeks = input$stor_week , candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant, fraction_peanut_paste_fondant = input$frac_peanut, cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test, negative_tests = input$neg_test))
    #)
  text_reactive <- 
    eventReactive( input$iterate, {
    input$candy
    input$min_log
    input$max_log
    input$log_red_week
    input$stor_week
    input$candy_serving
    input$frac_fondant
    input$frac_peanut
    input$cfus
    input$pos_test
    input$neg_test
    
    sum(number_sick(candy_on_market = input$candy, min_log_red = input$min_log, max_log_red = input$max_log,
                                     log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
                                     candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
                                     fraction_peanut_paste_fondant = input$frac_peanut,
                                     cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
                                     negative_tests = input$neg_test))
    #(number_sick(candy_on_market = input$candy, min_log_red = input$min_log, max_log_red = input$max_log ,log_reduction_per_week_storage = input$log_red_week , storage_weeks = input$stor_week , candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant, fraction_peanut_paste_fondant = input$frac_peanut, cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test, negative_tests = input$neg_test))
  
   #sum(
   #     replicate(n = input$candy / 7, expr = {
   #       number_sick(candy_on_market = 7, min_log_red = input$min_log, max_log_red = input$max_log,
   #                 log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
   #                 candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
   #                 fraction_peanut_paste_fondant = input$frac_peanut,
   #                 cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
   #                 negative_tests = input$neg_test)
   #   })
   # )
  })
  
  
  #output$Number_Sick <- renderText(text_reactive())
  output$Number_Sick <- renderText(paste("Number of People Sick for 1 Iteration: ", text_reactive(), sep = " "))
  
  output$Initial_Conditions <- renderText(paste("Initial Conditions: ", " ", sep = "\n" ))
  
  plot_reactive <- eventReactive(input$iterate_many, {
    input$candy
    input$min_log
    input$max_log
    input$log_red_week
    input$stor_week
    input$candy_serving
    input$frac_fondant
    input$frac_peanut
    input$cfus
    input$pos_test
    input$neg_test
    #hist(xlab = "Number of People Sick",main = "Over 100 Iterations",replicate(n = 100, expr = {sum(
    #  replicate(n = input$candy / 7, expr = {
    #    number_sick(candy_on_market = 7, min_log_red = input$min_log, max_log_red = input$max_log,
    #                log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
    #                candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
    #                fraction_peanut_paste_fondant = input$frac_peanut,
    #                cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
    #                negative_tests = input$neg_test)
   #   })
   # )}))
    
    
    
    hist(xlab = "Number of People Sick",main = "Over 100 Iterations",replicate(n = 100, expr = {sum(
      number_sick(candy_on_market = input$candy, min_log_red = input$min_log, max_log_red = input$max_log,
                  log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
                  candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
                  fraction_peanut_paste_fondant = input$frac_peanut,
                  cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
                  negative_tests = input$neg_test)
       )}))
    
  })
  
  output$plotting_output <- renderPlot(plot_reactive())
  
  plot_reactive2 <- eventReactive(input$iterate_many50, {
    input$candy
    input$min_log
    input$max_log
    input$log_red_week
    input$stor_week
    input$candy_serving
    input$frac_fondant
    input$frac_peanut
    input$cfus
    input$pos_test
    input$neg_test
    #hist(xlab = "Number of People Sick",main = "Over 100 Iterations",replicate(n = 100, expr = {sum(
    #  replicate(n = input$candy / 7, expr = {
    #    number_sick(candy_on_market = 7, min_log_red = input$min_log, max_log_red = input$max_log,
    #                log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
    #                candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
    #                fraction_peanut_paste_fondant = input$frac_peanut,
    #                cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
    #                negative_tests = input$neg_test)
    #   })
    # )}))
    
    
    
    hist(xlab = "Number of People Sick",main = "Over 50 Iterations",replicate(n = 50, expr = {sum(
      number_sick(candy_on_market = input$candy, min_log_red = input$min_log, max_log_red = input$max_log,
                  log_reduction_per_week_storage = input$log_red_week, storage_weeks = input$stor_week,
                  candy_servings_grams = input$candy_serving, fraction_fondant = input$frac_fondant,
                  fraction_peanut_paste_fondant = input$frac_peanut,
                  cells_per_gram_peanut_paste = input$cfus, positive_tests = input$pos_test,
                  negative_tests = input$neg_test)
    )}))
    
  })
  
  output$plotting50_output <- renderPlot(plot_reactive2())
  
  #This block plots the Probability of a positive given sampling results.
  
  output$probability_positive <- renderPlot({
    x <- seq(0, 1, by = 0.01)
    y <- dbinom(0, input$probpositive, x)
    plot(x, y, xlab = "Chance of Positive", ylab = "Probability", main = "Probability of Positive Given Sampling Results")
  })
  
  #This chunk of code plots the Salmonella Dose Response Model.
  
    output$salmonella_dose <- renderPlot({
    x <- seq(0, 100000000, by = 100)
    y <- 1-(1+x/input$parameter_2)^-input$parameter_1
    ##plot(x, y,log = 'x' ,xlab = "Chance of Positive", ylab = "Probability", main = "Probability of Positive Given Sampling Results")
    #axis(2, at = 10^(c(2, 4, 6)))
    curve(1-(1+x/input$parameter_2)^-input$parameter_1, from=1, to=100000000, n=300, xlab="Dose [(cells/serving)]", ylab="Response [Prob(Sick)]", 
            col="blue",log = "x", main="Salmonella Dose Response Model"  )
    })
  
  
  #INITIAL Condition Buttons
  
  #BASELINE
  observeEvent(input$baseline_bad,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #1 Positive Test
  observeEvent(input$onepos ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 1)
    updateNumericInput(session, inputId = "neg_test",
                       value = 9)
  })
  
  #1 Log Reduction, Cooking
  observeEvent(input$onelog ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 1)
    updateNumericInput(session, inputId = "max_log",
                       value = 1)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #2 Log Reduction, Cooking
  observeEvent(input$twolog ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 2)
    updateNumericInput(session, inputId = "max_log",
                       value = 2)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #5 Log Reduction, Cooking
  observeEvent(input$fivelog ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 5)
    updateNumericInput(session, inputId = "max_log",
                       value = 5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #Log Reduction in 1 wk Shelf Life
  observeEvent(input$onewklog ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0.33)
    updateNumericInput(session, inputId = "stor_week",
                       value = 1)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #Log Reduction in 4 wk Shelf Life
  observeEvent(input$fourwklog ,{
    updateNumericInput(session, inputId = "candy",
                       value = 210000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0.33)
    updateNumericInput(session, inputId = "stor_week",
                       value = 4)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 150)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 10)
  })
  
  #Realistic Condition Baseline
  observeEvent(input$baseline_real ,{
    updateNumericInput(session, inputId = "candy",
                       value = 1500000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 0)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 1.5)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 150)
  })
  
  #Realistic Condition Rec Much Sampling
    observeEvent(input$rec_much ,{
    updateNumericInput(session, inputId = "candy",
                       value = 1500000)
    updateNumericInput(session, inputId = "min_log",
                       value = 0.5)
    updateNumericInput(session, inputId = "max_log",
                       value = 1.5)
    updateNumericInput(session, inputId = "log_red_week",
                       value = 0)
    updateNumericInput(session, inputId = "stor_week",
                       value = 0)
    updateNumericInput(session, inputId = "candy_serving",
                       value = 20)
    updateNumericInput(session, inputId = "frac_fondant",
                       value = 0.75)
    updateNumericInput(session, inputId = "frac_peanut",
                       value = 0.24)
    updateNumericInput(session, inputId = "cfus",
                       value = 1.5)
    updateNumericInput(session, inputId = "pos_test",
                       value = 0)
    updateNumericInput(session, inputId = "neg_test",
                       value = 1000)
  })
    
    #Realistic Condition Rec 2-log cook step
    observeEvent(input$rec_twolog ,{
      updateNumericInput(session, inputId = "candy",
                         value = 1500000)
      updateNumericInput(session, inputId = "min_log",
                         value = 2)
      updateNumericInput(session, inputId = "max_log",
                         value = 2)
      updateNumericInput(session, inputId = "log_red_week",
                         value = 0.33)
      updateNumericInput(session, inputId = "stor_week",
                         value = 4)
      updateNumericInput(session, inputId = "candy_serving",
                         value = 20)
      updateNumericInput(session, inputId = "frac_fondant",
                         value = 0.75)
      updateNumericInput(session, inputId = "frac_peanut",
                         value = 0.24)
      updateNumericInput(session, inputId = "cfus",
                         value = 1.5)
      updateNumericInput(session, inputId = "pos_test",
                         value = 0)
      updateNumericInput(session, inputId = "neg_test",
                         value = 150)
    })
    
    #Realistic Condition Rec 5-log cook step
    observeEvent(input$rec_fivelog ,{
      updateNumericInput(session, inputId = "candy",
                         value = 1500000)
      updateNumericInput(session, inputId = "min_log",
                         value = 5)
      updateNumericInput(session, inputId = "max_log",
                         value = 5)
      updateNumericInput(session, inputId = "log_red_week",
                         value = 0)
      updateNumericInput(session, inputId = "stor_week",
                         value = 4)
      updateNumericInput(session, inputId = "candy_serving",
                         value = 20)
      updateNumericInput(session, inputId = "frac_fondant",
                         value = 0.75)
      updateNumericInput(session, inputId = "frac_peanut",
                         value = 0.24)
      updateNumericInput(session, inputId = "cfus",
                         value = 1.5)
      updateNumericInput(session, inputId = "pos_test",
                         value = 0)
      updateNumericInput(session, inputId = "neg_test",
                         value = 150)
    })
    
  
  
})




