library(shiny)
source("wordPredict.R")

#Shiny server
shinyServer(function(input, output){
  
  output$text1 <- renderText({
    input$textInput
  })
  predictedWords <- reactive({predictThis(input$textInput)})
  
  output$result1 <- renderText({
    predictedWords()[1]
  })
  
  output$result2 <- renderText({
    predictedWords()[-1]
  })
})