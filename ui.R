# load shiny package
library(shiny)
# begin shiny UI
shinyUI(pageWithSidebar(
        headerPanel("Prediction technology for easier mobile typing"),
        sidebarPanel(
                textInput(inputId="text1", label = "Please enter some words"),
                numericInput('n', 'Define the number of words to show (top n words)', 5, min = 1, max = 10, step = 1),
                actionButton("goButton", "Run!")
        ),
        mainPanel(
                h3('Prediction result'),
                p("The top n most likely words are shown below, and the top 1 would be the prediction result of the next word"),
                h4('Show the top n most likely words'),
                verbatimTextOutput("topn"),
                h4('Prediction result of the next word'),
                verbatimTextOutput("top1")
                
        )
))
