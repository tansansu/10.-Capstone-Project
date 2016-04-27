### Source Codes
---
#### *All source codes and data of whole project are in [my Github](https://github.com/tansansu/10.-Capstone-Project)*

#### Shiny app.R

```{R}
library(shiny)
library(markdown)
source("functions.R")
load(file = "grams_app.RData", envir = .GlobalEnv)
load(file = "gram_1.RData", envir = .GlobalEnv)

# Define UI for application
ui <- shinyUI(fluidPage(
    tags$head(
        tags$title("Words You May Type")
    ),
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
        hr(),
        column(width = 6, align = "right", 
               actionButton(inputId = "about", label = "About", width = 140)
        ), 
        column(width = 6, align = "left", 
               actionButton(inputId = "source", label = "Source Codes", width = 140)
        ), 
        column(width = 12, align = "center", 
               h5("Above two buttons is executed by pressing twice. :-)", 
                  style = "color:red;")
        ),
        column(width = 10, offset = 1, 
               conditionalPanel(condition = "input.about > input.source", 
                                includeMarkdown("about.md")), 
               conditionalPanel(condition = "input.source > input.about", 
                                includeMarkdown("source.md"))
        )
    )
))

# Define server logic
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

```

#### function.R (For predicting next words)

```{R}
library(dplyr)
library(tidyr)
library(stringr)

pred_word <- function(x) {
    recom <- NULL
    word_temp <- str_split(clean_word(x), pattern = " ") %>% unlist() %>% .[. != ""]
    word_count <- word_temp %>% length()
    if (word_count >= 3) {
        word_temp <- paste(word_temp[word_count - 2], word_temp[word_count - 1], word_temp[word_count])
        recom <- pred_gram(word_temp, .5)
        # Searching the matched 4-gram words when the number of results is less than 3
        word_temp <- word_temp %>% str_split(pattern = " ") %>% unlist()
        word_temp <- paste(word_temp[2], word_temp[3])
        recom <- rbind(recom, pred_gram(word_temp, .3))
        # Searching the matched 3-gram words when the number of results is less than 2
        word_temp <- word_temp %>% str_split(pattern = " ") %>% unlist()
        word_temp <- word_temp[2]
        recom <- rbind(recom, pred_gram(word_temp, .2))
    }
    else if (word_count == 2) {
        recom <- pred_gram(word_temp, .7)
        # Searching the matched 2-gram words when the number of results is less than 2 
        word_temp <- str_split(x, pattern = " ") %>% unlist()
        word_temp <- word_temp[2]
        recom <- rbind(recom, pred_gram(word_temp, .3))
    }
    else {
        recom <- pred_gram(word_temp, 1)
    }
    recom <- recom %>% group_by(Recommend) %>% 
        summarise(Prob = sum(Prob)) %>% 
        # Adding the probability of the Recommended words
        merge(x = ., y = gram_1[, c(".", "Prob")], by.x = "Recommend", by.y = ".", all.x = T) %>% 
        # Weight settting : 4/3/2 grams(0.95) + 1gram(0.05)
        mutate(Prob = (.95 * Prob.x) + (.05 * Prob.y)) %>% 
        select(Recommend, Prob) %>% 
        arrange(desc(Prob))
    names(recom) <- c("Word", "Probability")
    # If the word for recommendation is none, 
    # I'm going to recommend the most frequently used three words(the, to, and)
    if (nrow(recom) < 1) {
        top_1_words <- data.frame(Word = c("the", "to", "and"), 
                                  Probability = c(0.05069748, 0.02818105, 0.02580177), 
                                  stringsAsFactors = F)
        recom <- rbind(recom, top_1_words)    
    }
    print(head(recom, 3))
    return(head(recom, 3))
}

pred_gram <- function(x, weight) {
    return(grams_app %>% filter(x == Lookup) %>% 
               mutate(Prob = weight * Freq / sum(Freq)) %>% 
               select(Recommend, Prob))
}

clean_word <- function(x) {
    x %>% str_to_lower() %>% 
        str_replace_all(pattern = "http[[:alnum:]]*", "") %>% 
        str_replace_all(pattern = "[:punct:]", "") %>% 
        str_replace_all(pattern = "[:cntrl:]", "") %>% 
        str_replace_all(pattern = "[0-9]", "") %>% 
        .[. != ""]
}
```