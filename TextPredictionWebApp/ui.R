#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
library(shinythemes)
library(here)

shinyUI(
    fluidPage(
        theme = shinytheme("flatly"),
        tabsetPanel(
            tabPanel("Prediction Model",  
                titlePanel("Text Prediction Model"),
                align = "center",
                fluidRow(
                    textAreaInput("text", label = '', value = "john hopkins"),
                    hr()
                ),
          
                fluidRow(
                    em(h4("Predicted next word:")),
                    h3(textOutput("predictText"), style = "color:red"),
                    textOutput("time"),
                    hr()
                ),
          
                fluidRow(
                    em(h4("In a sentence:")),
                    h3(textOutput("sentence"), style = "color:orange"),
                    hr()
                ),
          
                fluidRow(
                    a(href = "https://github.com/PieterVeere/Capstone-Project", "github,"),
                    a(href = "http://rpubs.com/NLNightmare/431169", "slides about the model"),
                    br(),
                    column(imageOutput("jhuLogo"), width = 4),
                    column(imageOutput("courseraJhu"), width = 4),
                    column(imageOutput("swiftKeyLogo"), width = 4)
                )
            ),
            tabPanel("About Model",
                hr(),
                "This model is largely based off the Katz's back-off model(1). The Katz's back-off model states
                that for n-grams ('n' sized word combination) with a frequency below a certain
                threshold, smaller n-grams should be applied to look for better matches. The 
                threshold in my model where choosen based on the amount of n-grams needed to 
                cover 50% of total n-gram use. N-grams of size six to two were used in my model." ,
                br(),
                br(),

                "The main dataset on which the model is based, was provided by SwiftKey. They supplied us with extracts from
                twitter, blog and American newspapers. Only a small part of the supplied data
                was used due to limited processing power and RAM on my end."  ,
                br(),
                br(),
                
                "To increase to accuracy of my model I added all English idioms on Wikipedia to the 
                matching dataset.(2) As there were only roughly 8000 idioms on Wikipedia at the
                time of creation this did not slow down the prediction speed."  ,
                hr(),

                a(href ="https://en.wikipedia.org/wiki/Katz%27s_back-off_model", 
                  "(1) https://en.wikipedia.org/wiki/Katz%27s_back-off_model")  ,
                br(),
                a(href ="https://en.wiktionary.org/wiki/Category:English_idioms",
                  "(2) https://en.wiktionary.org/wiki/Category:English_idioms"),
                br(),
                a(href ='https://github.com/PieterVeere/Capstone-Project',
                  'Github: https://github.com/PieterVeere/Capstone-Project'),
                br(),
                a(href ='https://swiftkey.com',
                  'Swiftkey: https://swiftkey.com'),
                br(),
                a(href ='https://www.coursera.org/specializations/jhu-data-science',
                  'Coursera Data Science Specialisation: https://www.coursera.org/specializations/jhu-data-science')
                )
)))
