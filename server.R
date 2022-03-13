# server for coffee shiny app

library(shiny)
library(tidyverse)
library(gt)

source("coffee_functions.R")

shinyServer(function(input, output) {
        
        output$table <-
                render_gt(give_me_coffee(coffee = input$coffee,
                                target_volume = input$volume,
                                brew_method = input$method,
                                coffee_data),
                          height = px(600),
                          width = px(600))
})