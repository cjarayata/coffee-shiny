# UI for coffee shiny app

library(shiny)
library(tidyverse)
library(gt)
library(glue)

coffee_data <- read_csv("coffee_data.csv", show_col_types = F)

shinyUI(fluidPage(
        
        titlePanel("CJ's Pourover Coffee Brewing Guide"),
        
        sidebarLayout(
                sidebarPanel(
                        selectInput("coffee", "Which coffee are you brewing?",
                                    choices = coffee_data$coffee_brand),
                        selectInput("method", "Which brewing method are you using?",
                                    list("Hoffman V60" = "hoffman")),
                        sliderInput("volume", HTML("How much coffee are you making?<br>
                                                   250g = 1 coffee mug<br>
                                                   600g = coffee to share on a weekday<br>
                                                   800g = a full carafe for weekends"),
                                    value = 600, min = 250, max = 800, step = 50),
                        submitButton("Let's Brew Coffee!")
                ),
                
                mainPanel(
                        gt_output(outputId = "table")
                )
        )
        
        
))