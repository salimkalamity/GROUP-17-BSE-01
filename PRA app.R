library(shiny)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(utils)
library(textclean)
library(hunspell)
library(ggplot2)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)

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
      
      fileInput("num" ,label="Upload texfile for word cloud", multiple = TRUE, accept = c(".csv","text/csv"), width = NULL,
                buttonLabel = "Browse...", placeholder = "No file selected"),
      uiOutput("selectfile"),
      # Slider input for frequency change
      sliderInput("slider1", "Minimum Frequency:",
                  min = 1, max = 50, value = 5),
      # Slider input for number of words change
      sliderInput("slider2", "Max words:",
                  min = 10, max = 1000, value = 100),
      
      actionButton(inputId = "click", "create wordcloud"),
      actionButton(inputId = "sent", "plot sentiment"),
      actionButton("rate","Ratings"),
      actionButton("word","Word Distribution"),
      actionButton("ward","Word Preview")
      
      
      
      
    ),
    mainPanel(
      plotOutput("word"),
      verbatimTextOutput("reviews"),
      plotOutput("bar"),
      plotOutput("bar1"),
      plotOutput("bar2"),
      plotOutput("preview")
      
    )
    )
)

  
  
options(shiny.maxRequestSize = 100*1024^2)
server <- function(input, output) {
data <-reactive({
  if (is.null(input$num)){
    return()
  } 
  mydata <- read.csv(input$num$datapath, header=TRUE, sep= ",") 
  return(mydata$reviews.text) 
})
data1 <-reactive({
  if (is.null(input$num)){
    return()
  } 
  mydata <- read.csv(input$num$datapath, header=TRUE, sep= ",") 
  return(mydata) 
})
data2 <- reactive({
  if (is.null(input$num)){
    return()
  } 
  mydata <- read.csv(input$num$datapath, header=TRUE, sep= ",",colClasses = "character") 
  return(mydata) 
}
)

 observeEvent(input$click, {
    mydata <- data()
    #creating the corpus object
    
    wc <- VectorSource(mydata)
    wc <- Corpus(wc)
    
    #pre-processing data
    
    wc <- tm_map(wc, removePunctuation)
    wc <- tm_map(wc, content_transformer(tolower))
    wc <- tm_map(wc, removeWords, stopwords("english"))
    wc <- tm_map(wc, stripWhitespace)
    
    output$word <- renderPlot({
      wordcloud(wc, scale=c(4,0.5),
                min.freq = input$slider1, max.words=input$slider2,
                colors=brewer.pal(8, "Dark2"))
    })
    
  })
  
 
 observeEvent(input$sent,{
   mydata <- data()
   get_data<-iconv(mydata)
   s<-get_nrc_sentiment(get_data)
   head(s)
   output$bar1 <- renderPlot({
     barplot(colSums(s),
             las=2, 
             col = rainbow(10),
             ylab = "count",
             main = "Sentiment scores for Product Reviews")
     
     
   })
   
 })
 observeEvent(input$rate,{
   mydata <-data1()
   output$bar <-renderPlot({
     hist(mydata$reviews.rating, main = "Distribution of Product Rating", xlab = "Rating", col = "Yellow")
   })
   
 })
 observeEvent(input$word,{
   mydata <- data2()
   output$bar2 <- renderPlot({
     # Split words
     words_list = strsplit(mydata$reviews.text, " ")
     # Words per review 
     words_per_review = sapply(words_list, length)
     barplot(table(words_per_review), border=NA, main="Distribution of words per review", cex.main=1)
     
     
   })
 })
 data3 <- reactive(
   {
     if (is.null(input$num)){
       return()
     } 
     mydata <- read.csv(input$num$datapath, header=TRUE, sep= ",",colClasses = "character") 
     return(mydata)  
     
   }
 )
 
 observeEvent(input$ward,{
   
   mydata <- data3()
   
   output$preview <- renderPlot({
     # Split words
     words_list = strsplit(mydata$reviews.text, " ")
     # Length of words per review
     word_length = sapply(words_list, function(x) mean(nchar(x)))
     barplot(table(round(word_length)), border=NA,xlab = "Word length in number of characters", main="Distribution of words length per review", cex.main=1)
     
   })
 })

 
  }
  
 

shinyApp(ui = ui, server = server)