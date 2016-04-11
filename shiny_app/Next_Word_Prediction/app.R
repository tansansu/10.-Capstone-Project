# START: 2016.03.31

library(shiny)
source("./functions.R")
load(file = "./grams_app.RData")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    headerPanel(
        div(align = "center", img(src = "logo.png"))
    ),
    fluidRow(
        column(width = 12, align = "center", 
               style = "text-align:center;", 
               h5("Type any sentence in english, please..", 
                  style = "line-height: 1;text-align: center;color: gray;")
        ),
        column(width = 12, align = "center", 
               textInput(inputId = "word_input", label = "", width = "50%")
        ),
        column(width = 6, align = "right", 
               h5(textOutput("colname1"), style = "color: darkred"), 
               h2(strong(textOutput("word1"))), 
               h3(strong(textOutput("word2")), style = "color: gray;"),
               h4(strong(textOutput("word3")), style = "color: silver;")
        ),
        column(width = 6, align = "left", 
               h5(textOutput("colname2"), style = "color: darkred"), 
               h2(em(textOutput("prob1"))), 
               h3(em(textOutput("prob2")), style = "color: gray;"),
               h4(em(textOutput("prob3")), style = "color: silver;")
        ),
        br(),
        br(),
        hr(),
        column(width = 6, align = "right", 
               tags$button("About")
        ),
        column(width = 6, align = "left", 
               a(href = " ", tags$button("Source Codes"))
        )
    )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    result <- NULL
    cap1 <- reactive({
        if (input$word_input == "") NULL
        else return("The next word might be")
    })
    cap2 <- reactive({
        if (input$word_input == "") NULL
        else return("Probability")
    })
    output$word1 <- renderText(
        if (input$word_input == "") NULL
        else {
            result <<- pred_word(input$word_input)
            output$colname1 <- renderText(cap1())
            output$colname2 <- renderText(cap2())
            return(result[1, 1])
        }
    )
    output$prob1 <- renderText(
        if (input$word_input == "") NULL
        else {
            return(paste0(round(result[1, 2], 4) * 100, "%"))
        }
    )
    output$word2 <- renderText(
        if (input$word_input == "") NULL
        else {
            if (!is.na(result[2, 1])) return(result[2, 1])
        }
    )
    output$prob2 <- renderText(
        if (input$word_input == "") NULL
        else {
            if (!is.na(result[2, 1])) return(paste0(round(result[2, 2], 4) * 100, "%"))
        }
    )
    output$word3 <- renderText(
        if (input$word_input == "") NULL
        else {
            if (!is.na(result[3, 1])) return(result[3, 1])
        }
    )
    output$prob3 <- renderText(
        if (input$word_input == "") NULL
        else {
            if (!is.na(result[3, 1])) return(paste0(round(result[3, 2], 4) * 100, "%"))
        }
    )
})

# Run the application 
shinyApp(ui = ui, server = server)

