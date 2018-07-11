library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(corpus)
options(shiny.maxRequestSize = 96*1024^2)

shinyServer(
  
  function(input, output){
   # renderTable prints a table in R
   # renderPrint preserves the structure of the given data.  Use verbatinTextOutput
   # renderPlot prints the data visualisations.
    
   # output$name <- renderText(input$studentname)
    
   # output$reg <- renderText(input$registrationnumber)
    
   # output$gender <- renderText(input$gender)
    
   # output$slider <- renderText(paste("You selected:",input$slider))
    
   # output$select <- renderText(input$select)
    
   # output$grammar <- renderText(
    #  paste("Grammar is about spelling, punt...")
   # )
   # output$pra <- renderText(
   #   paste("Product Review Analysis system is a web based system that will analyse customer reviews from over 1000 products provided by different online markets.
    #        PRAS will be used to assess how writing quality impacts positive and negative online product reviews such as analyzing reviews for grammatical errors, frequency of particular words, and product ratings. ")
    #)
    #x <- reactive({
     # iris[,as.numeric(input$val1)] 
      #     })
    #y <- reactive({
     # iris[,as.numeric(input$val2)] 
    #})
    #output$plot <- renderPlot({
      
    #  plot(x(),y())
      
     # })
    
    #output$downloadplot <- downloadHandler(
      #filename = function(){
       # paste("iris",input$select, sep = ".")
      #},
     # 
    #  content = function(){
   #     png(file)
  #      dev.off
 #     }
  #  )
    
    data <- reactive({
      uploadedfile <- input$file
      
        readuploadedfile <- readLines(uploadedfile$datapath)

        mydatasetcorpus <- Corpus(VectorSource(readuploadedfile$reviews.text))
        #inspect(mydatasetcorpus[1:10])
        
        #corpus cleaning
        cleancorpus <- tm_map(mydatasetcorpus, stripWhitespace)
        #inspect(cleancorpus[1:10])
        
        cleancorpus <- tm_map(mydatasetcorpus, removePunctuation)
        #inspect(cleancorpus[1:10])
        
        cleancorpus <- tm_map(mydatasetcorpus, PlainTextDocument)
        
        cleancorpus <- tm_map(mydatasetcorpus, removeNumbers)
        
        cleancorpus <- tm_map(mydatasetcorpus, removeWords, stopwords("english"))
        
        cleancorpus <- tm_map(mydatasetcorpus, content_transformer(tolower))
        #inspect(cleancorpus[1:10])
        
    })
        
        output$wordcloud <- renderPlot({
          wcdata <- data()
          
        wordcloud(wcdata, min.freq = 10, max.freq = Inf,colors = brewer.pal(8,"Set2"), random.order = F)

         })
    
    
  }
)