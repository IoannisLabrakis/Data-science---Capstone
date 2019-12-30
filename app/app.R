library(shiny)

#source code
source("prediction_algorithm.R")

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Text Prediction model"),
   p("This app uses text input, for predicting the next four words"),
   
   sidebarLayout(
      sidebarPanel(
        h2("How to use:"),
        h6("1. Enter a word/phrase in the box"),
        h6("2. The prediction with the highest frequency is returned first"),
        h6("3. The predictions with lower frequency than the first is returned (max 19)"),
        h2("Notes:"),
        h6("1. Only English language is supported"),
        h6("2. Non ASCII characters can not be evaluated"),
        h6("3. The input text is filtered from some common abbreviations, upper case letters, symbols, whitespace and hashtags"),
        h6("4. With no input, top 5 frequency words are returned"),
        h6("5. If no match is found, or the input can not be evaluated, the message appears 'no match found / wrong input'"),
        br(),
        a("Code of the prediction model",href = "https://github.com/IoannisLabrakis/Data-science---Capstone")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         p(textInput(inputId = "phrase",label= "Write text below", value = ""),
         h3("Top prediction:"),
         verbatimTextOutput("prediction"),
         h3("Other predictions:"),
         verbatimTextOutput("predictions")
         )
      )
   )
)

# Define Server for application 
server <- function(input, output) {
    
    output$prediction <- renderText({
        nextWord(input$phrase)[1]
    })
    
    output$predictions <- renderText({
        nextWord(input$phrase)[-1]
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

