# server for coffee shiny app

library(shiny)
library(tidyverse)
library(lubridate)
library(gt)

source("coffee_functions.R")

shinyServer(function(input, output, session) {
        
        observeEvent(input$brew_table, {
                output$table <-
                if( input$lcornot == "A La Colombe coffee that CJ has made before"){
                        render_gt(give_me_coffee(coffee = input$coffee,
                                target_volume = input$volume,
                                brew_method = input$method,
                                coffee_data),
                          width = px(400))
                } else {
                        render_gt(give_me_custom_coffee(
                                customcoffee = input$customcoffee,
                                grindsize = input$grindsize,
                                coffeeamt = input$coffeeamt,
                                temperature = input$temperature,
                                target_volume = input$volume,
                                brew_method = input$method),
                                width = px(400))
                }
        })
        
        # shoutout to stackoverflow user "florian"!
        # Initialize the timer, not active.
          timer <- reactiveVal(0)
          active <- reactiveVal(FALSE)
          update_interval = 1 # How many seconds between timer updates?
        
          # Output the time left.
          output$stopwatch_time <- renderText({
                  temp_time <- seconds_to_period(timer())
                  nice_time <- sprintf('%02d:%02d', minute(temp_time), second(temp_time))
            paste("Current Brew Time: ", nice_time)
          })
        
          # observer that invalidates every second. If timer is active, increase by one.
          observe({
            invalidateLater(1000, session)
            isolate({
              if(active())
              {
                timer(round(timer()+update_interval,2))
              }
            })
          })
        
          # observers for actionbuttons
          observeEvent(input$start, {active(TRUE)})
          observeEvent(input$stop, {active(FALSE)})
          observeEvent(input$reset, {timer(0)})
        
})