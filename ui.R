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
                        selectInput("lcornot", "What coffee are you brewing?",
                                    choices = c("An Elixr or La Colombe coffee that CJ has made before", "Literally any other kind of coffee")),
                        conditionalPanel(
                                condition = "input.lcornot == 'An Elixr or La Colombe coffee that CJ has made before'",
                                selectInput("coffee", "Which coffee are you brewing?",
                                    choices = coffee_data$coffee_brand)
                        ),
                        conditionalPanel(
                                condition = "input.lcornot == 'Literally any other kind of coffee'",
                                textInput("customcoffee", "What coffee?"),
                                numericInput("grindsize", "What grind size are you using?", value = 14),
                                numericInput("temperature", "What temperature of water are you using?", value = 205),
                                numericInput("coffeeamt", "How many grams of coffee are you using?", value = 36)
                                ),
                        selectInput("method", "Which brewing method are you using?",
                                    list("Hoffmann V60" = "hoffmann v60",
                                         "Hoffmann French Press" = "hoffmann french press",
                                         "Traditional French Press" = "french press")),
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