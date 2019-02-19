library(shiny)

shinyUI(fluidPage(
  navbarPage(
    h3("Coursera Data Science Specialization SwiftKey Capstone"),
    inverse = TRUE,
    tabPanel(
      h5("Next Word Prediction"),
      
      #Application title
      titlePanel("Next Word Prediction"),
      helpText("This application will read in a phrase and predict the next word.
               It will provide a list of the most probable words that might follow
               the phrase."),
      
      #Sidebar
      sidebarPanel(
        textInput("textInput", label = "Enter the phrase for which you want to predict
                  the next word", "New York City")
      ),
      
      #Main Panel
      mainPanel(
        h3("Input"),
        textOutput("text1"),
        br(),
        h3("Prediction Model Output"),
        h5("Most probable word"),
        textOutput("result1"),
        br(),
        h5("Next most probable words"),
        textOutput("result2")
      )
    ),
    tabPanel(
      h5("About"),
      helpText("Smart word prediction is one improvement to mobile user experience,
               which makes it easier for people to type on their mobile devices."),
      br(),
      helpText("The objective of this app is to predict the next word of a phrase using the
               predictive text models created from a large collection of English documents
               from three different sources: blog articles, news, and Twitter."),
      br(),
      helpText("This app is created for the Coursera Data Science Specialization
               SwiftKey Capstone offered by John Hopkins.")
    )
  )
))