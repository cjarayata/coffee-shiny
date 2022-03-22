# UI for coffee shiny app

library(shiny)
library(tidyverse)
library(gt)
library(glue)

coffee_data <- read_csv("coffee_data.csv", show_col_types = F)

shinyUI(fluidPage(
        
        titlePanel("CJ's Coffee Brewing Guide"),
        
        
        sidebarLayout(
                sidebarPanel(
                        selectInput("coffee", "Which coffee are you brewing?",
                                    choices = coffee_data$coffee_brand),
                        selectInput("method", "Which brewing method are you using?",
                                    list("Hoffman V60" = "hoffmann v60",
                                         "Hoffman French Press" = "hoffmann french press")),
                        sliderInput("volume", "How much coffee are you making?",
                                    value = 600, min = 250, max = 800, step = 50),
                        HTML("For reference:<br>
                              250g = 1 coffee mug<br>
                              600g = coffee to share on a weekday<br>
                              800g = a full carafe for weekends<br>"),
                        br(),
                        HTML("<b>Ready to grind?</b>"),
                        br(),
                        actionButton('brew_table', "Generate Brew Guide"),
                        br(),
                        br(),
                        HTML("<b>Ready to start pouring?</b>"),
                        br(),
                        actionButton('start','Start Brewing!'),
                        br(),
                        br(),
                        actionButton('stop','Stop Brewing'),
                        br(),
                        br(),
                        actionButton('reset','Reset Timer'),
                        tags$hr(),
                        br(),
                        HTML("<p>Check out the underlying code on <a href='https://github.com/cjarayata/coffee-shiny'>GitHub!</a>")
                ),
                
                mainPanel(
                        gt_output(outputId = "table"),
                        br(),
                        h1(textOutput('stopwatch_time'), align = "center")
                )
        )
        
        
))