library(shiny)
library(tm)
library(RColorBrewer)

ui <- fluidPage(
  title = "Product Review Anaylsis App",
  h1("Product Review Analysis App"),
  navbarPage(title = "PRA Menu",
             tabPanel("Home"),
             tabPanel("Plots"),
             tabPanel("Wordcloud"),
             tabPanel("Ratings"),
             tabPanel("Grammar checks")
  ),
  sidebarLayout(
    sidebarPanel(
      
      fileInput(inputId ="num" ,label="Upload texfile for word cloud", multiple = FALSE, accept = NULL, width = NULL,
                buttonLabel = "Browse...", placeholder = "No file selected"),
      hr(),
      actionButton(inputId = "click", "create wordcloud")
      
      
    ),
    mainPanel(
      plotOutput("word"),
      verbatimTextOutput("reviews")
    )
    )
)

  
  

server <- function(input, output) {
  dk <- eventReactive(input$click, {
    mydata <- input$num
    #creating the corpus object
    
    wc <- VectorSource(mydata)
    wc <- Corpus(wc)
    
    #pre-processing data
    
    wc <- tm_map(wc, removePunctuation)
    wc <- tm_map(wc, content_transformer(tolower))
    wc <- tm_map(wc, removeWords, stopwords("english"))
    wc <- tm_map(wc, stripWhitespace)
    
  })
  output$word <- renderPlot({
    wordcloud(wc, scale=c(4,0.5),
              min.freq = input$freq, max.words=input$max,
              colors=brewer.pal(8, "Dark2"))
  })

}

shinyApp(ui = ui, server = server)