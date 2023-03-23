# Load packages
library(shiny)
library(shinythemes)


# Define UI 
shinyUI(fluidPage(theme=shinytheme("lumen"),
    titlePanel("Text Prediction App"),

    sidebarLayout(
        sidebarPanel(
            h5("Doris Chen"),
            h5("03/23/2023"),
            br(),
            p("This is the Johns Hopkins-SwiftKey Capstone Project from Coursera Data Science Specialization."),
            br(),
            p("Training by text data from blogs, news and twitter, the App predicts the text which you are likely to enter next. It only supports English so far."),
            br(),
            textInput(inputId="text", label="Please enter the text:"),
            submitButton("Submit")
        ),

        mainPanel(
            p(strong("Top predicted words:")),
            textOutput(outputId="predict"),
            br(),
            p(strong("More predicted words:")),
            plotOutput(outputId="wordcloud")
        )
    )
)
)