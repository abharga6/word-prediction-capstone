suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

source("./PredictionFunction.R")
y <- readRDS(file="./fourgramTable.RData")
z <- readRDS(file="./threegramTable.RData")
k <- readRDS(file="./twogramTable.RData")

shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$text
    wordPrediction <- prediction_model(text,y,z,k)})
  
  output$predictedWord <- renderPrint(wordPrediction())
  output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})