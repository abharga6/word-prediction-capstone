suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

shinyUI(navbarPage("Data Science Capstone Project", 
                   
                   theme = shinytheme("spacelab"),
                   
                   ############################### ~~~~~~~~1~~~~~~~~ ##############################  
                   ## Tab 1 - Prediction
                   
                   tabPanel("Next Word Prediction",
                            
                            
                            fluidRow(
                              
                              column(3),
                              column(6,
                                     tags$div(textInput("text", 
                                                        label = h3("Enter your text here:"),
                                                        value = ),
                                              tags$span(style="color:brown",("Only English words are supported.")),
                                              br(),
                                              tags$hr(),
                                              h4("The next-word predicted is:"),
                                              tags$span(style="color:brown",
                                                        tags$strong(tags$h3(textOutput("predictedWord")))),
                                              br(),
                                              tags$hr(),
                                              h4("Words entered by the user (you):"),
                                              tags$em(tags$h4(textOutput("enteredWords"))),
                                              align="center")
                              ),
                              column(3)
                            )
                   ),
                   
                   ############################### ~~~~~~~~F~~~~~~~~ ##############################
                   
                   ## Footer
                   
                   tags$hr(),
                   
                   tags$br(),
                   
                   tags$span(style="color:grey", 
                             tags$footer(("© 2017 - "), 
                                         tags$a(
                                           target="_blank",
                                           "AYUSH BHARGAVA, MS INDUSTRIAL ENGINEERING"), 
                                         tags$br(),
                                         ("Built with"), tags$a(
                                           href="http://www.r-project.org/",
                                           target="_blank",
                                           "R"),
                                         ("&"), tags$a(
                                           href="http://shiny.rstudio.com",
                                           target="_blank",
                                           "Shiny."),
                                         #                       ("&"), tags$a(
                                         #                               href="http://www.rstudio.com/products/shiny/shiny-server",
                                         #                               target="_blank",
                                         #                               "Shiny Server."),
                                         #                       ("Hosted on"), tags$a(
                                         #                               href="https://www.digitalocean.com/?refcode=f34ade566630",
                                         #                               target="_blank",
                                         #                               "DigitalOcean."),
                                         
                                         align = "center"),
                             
                             tags$br()
                   )
)
)