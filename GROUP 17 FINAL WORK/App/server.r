library(shiny)
library(scales)
library(tm)
library(wordcloud)
library(SnowballC)
library(dplyr)
library(reshape2)
library(tidytext)
library(syuzhet)
library(pander)
library(xlsx)
library(ggplot2)
library(partykit)
library(DT)
library(shinydashboard)
library(hunspell)
library(textclean)

options(shiny.maxRequestSize=100*1024^2)
server <- function(input, output, session) {
  
  myfile <- reactive({
    
    if (is.null(input$file)){
      return()
    } 
    mydata <- read.csv(input$file$datapath, header=TRUE, sep= ",") 
    return(mydata) 
  })
  
  output$data <- renderDataTable({
    myfile()
  })
    
    output$nomisp <- renderText({
      count <- which_are()
      no. <-count$misspelled(textdata)
      paste("The number of reviews with mispelled words are", length(no.))
    })
    
    output$punc <- renderText({
      textdata = gsub("[[:punct:]]", "", myfile()$reviews.text)
      textdata = gsub("[[:punct:]]", "", textdata)
      textdata = gsub("[[:digit:]]", "", textdata)
      textdata = gsub("http\\w+", "", textdata)
      textdata = gsub("[ \t]{2,}", "", textdata)
      textdata = gsub("^\\s+|\\s+$", "", textdata)
      count <- which_are()
      no. <-count$no_endmark(myfile()$reviews.text)
      paste("The number of reviews that don't end sentences with punctuation are", length(no.))
    })
    
    output$puncplot <- renderPlot({
      validate(
        need(!is.null(input$file),
             "Upload a csv file"
        )
      )
      Reviews_with_no_end_punctuation <- has_endmark(myfile()$reviews.text, endmarks = c("?", ".", "!"))
      data <- cbind(myfile(), Reviews_with_no_end_punctuation)
      ggplot(data=data,aes(x=Reviews_with_no_end_punctuation))+ geom_histogram( bins = 40, stat = "count") +labs(title="A histogram showing number of reviews that don't end sentences with punctuation.",
                                                                                                         x="Reviews with no end punctuation",
                                                                                                                 y="Number of product Reviews"
      )
    })
  
    
    datainput <- reactive({
      
      # Outputs a helpful message when neither text is entered nor
      # the text file uploaded.
      validate(
        need((!is.null(input$file)),
             "Upload your CSV file."
        )
      )
      mydatasetcorpus <- Corpus(VectorSource(myfile()$reviews.text))
      
      #corpus cleaning
      cleancorpus <- tm_map(mydatasetcorpus, stripWhitespace)
      
      cleancorpus <- tm_map(mydatasetcorpus, removePunctuation)
      
      cleancorpus <- tm_map(mydatasetcorpus, PlainTextDocument)
      
      cleancorpus <- tm_map(mydatasetcorpus, removeNumbers)
      
      cleancorpus <- tm_map(mydatasetcorpus, removeWords, stopwords("english"))
      
      cleancorpus <- tm_map(mydatasetcorpus, content_transformer(tolower))
      
      cleancorpus <- tm_map(mydatasetcorpus, removeWords, c("this", "part","the","they","and","was","for","there"))
      
  })
    
    observeEvent(input$click, {
      output$wc <- renderPlot({
        wordcloud(datainput(),
                      scale=c(5, 0.5),
                      min.freq=input$slider1,
                      max.words=input$slider2,
                      random.order=FALSE,
                      use.r.layout=FALSE,
                      colors=brewer.pal(8, "Dark2"))
      })
      })
    observeEvent(input$missamplebutton, {
      output$misword <- renderPrint({
        misspelled_words <- hunspell_find(input$missample, format = "latex")
        paste("Misspelled word(s):",misspelled_words)
      })
    })
    
    observeEvent(input$sentimentbutton, {
      output$sentimentsummary <- renderPrint({
        mysentiment <- get_nrc_sentiment(input$sentiment)
        mysentiment
      })
    })
    
  
  output$rategg <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    
    ggplot(data=myfile(),aes(x=reviews.rating))+
      geom_histogram(bins=40)+
      labs(title="Distribution of product ratings",
           x="Review ratings",
           y="Number of products"
      )
  })
  
  
  observeEvent(input$sentimentcsv, {
  output$sentiment <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    #Cleaning
    textdata = gsub("[[:punct:]]", "", myfile()$reviews.text)
    textdata = gsub("[[:punct:]]", "", textdata)
    textdata = gsub("[[:digit:]]", "", textdata)
    textdata = gsub("http\\w+", "", textdata)
    textdata = gsub("[ \t]{2,}", "", textdata)
    textdata = gsub("^\\s+|\\s+$", "", textdata)
    
    sentianalysis <- get_nrc_sentiment(textdata[1:input$senttext])
    sentimentscores <- data.frame(colSums(sentianalysis[1:8]))
    names(sentimentscores) <- "Score"
    sentimentscores <- cbind("sentiment" = rownames(sentimentscores), sentimentscores)
    rownames(sentimentscores) <- NULL
    ggplot(sentimentscores, aes(x = sentiment, y = Score)) +
      geom_bar(aes(fill = sentiment), stat = "identity") + 
      theme(legend.position = "none") +
      xlab("Emotions") +
      ylab("Sentiment Score") + 
      ggtitle("Total Sentiment score based on Online Product Reviews")
  })
  
  })
  observeEvent(input$pol, {
  output$polarity <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    #Cleaning
    textdata = gsub("[[:punct:]]", "", myfile()$reviews.text)
    textdata = gsub("[[:punct:]]", "", textdata)
    textdata = gsub("[[:digit:]]", "", textdata)
    textdata = gsub("http\\w+", "", textdata)
    textdata = gsub("[ \t]{2,}", "", textdata)
    textdata = gsub("^\\s+|\\s+$", "", textdata)
    
    sentianalysis <- get_nrc_sentiment(textdata[1:input$Poltext])
    sentimentscores <- data.frame(colSums(sentianalysis[,9:10]))
    names(sentimentscores) <- "Score"
    sentimentscores <- cbind("sentiment" = rownames(sentimentscores), sentimentscores)
    rownames(sentimentscores) <- NULL
    ggplot(sentimentscores, aes(x = sentiment, y = Score)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Polarity") +
      ylab("Sentiment Score") +
      ggtitle("Total Sentiment score based on Online Product Reviews")

  })
  
  })
  
  output$freq <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    dtm <- TermDocumentMatrix(datainput()[1:input$freqmax], control = list(removeNumbers = T, removePunctuation = T, stripWhitespace = T, tolower = T, stopwords = T, stemming = T))
    termFrequency <- rowSums(as.matrix(dtm))
    termFrequency <- subset(termFrequency, termFrequency >=input$freqmin)
    barplot(termFrequency, las = 2, col = rainbow(20))
    
  })
  
  
  output$mistable <- renderPrint({
    misspelled_words <- hunspell_find(as.character(myfile()$reviews.text[1:input$freqmaxmisp]), format = "latex")
    misspelled_words
    
  })
  
  output$freqmisp <- renderPrint({
    misspelled_words <- hunspell_find(as.character(myfile()$reviews.text[1:input$freqmaxmisp]), format = "latex")
    mydatasetcorpus <- Corpus(VectorSource(misspelled_words))
    dtm <- TermDocumentMatrix(mydatasetcorpus)
    termFrequency <- rowSums(as.matrix(dtm))
    as.matrix(termFrequency)
  })
  
  output$freqtable <- renderPrint({
    mydatasetcorpus <- Corpus(VectorSource(myfile()$reviews.text))
    dtm <- TermDocumentMatrix(mydatasetcorpus[1:input$freqmax], control = list(removeNumbers = T, removePunctuation = T, stripWhitespace = T, tolower = T, stopwords = T, stemming = T))
    termFrequency <- rowSums(as.matrix(dtm))
    termFrequency <- subset(termFrequency, termFrequency >=input$freqmin)
    as.matrix(termFrequency)
  })
  
  output$charactersinwordsplot <- renderPlot({
    
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    word_length = sapply(as.character(myfile()$reviews.text), function(x) mean(nchar(x)))
    
    barplot(table(round(word_length)), border=NA,xlab = "Word length in number of characters", main="Distribution of words length per review", cex.main=1)
  })
  
  output$wordfreq <- renderPrint({
    words_list = strsplit(as.character(myfile()$reviews.text), " ")
    # Words per review 
    words_per_review = sapply(words_list, length)
    
    words_per_review[1:50]
    
    
  })
  
  output$wordfreqplot <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    words_list = strsplit(as.character(myfile()$reviews.text), " ")
    # Words per review 
    words_per_review = sapply(words_list, length)
    
    barplot(table(words_per_review), border=NA, main="Distribution of words per review", cex.main=1, xlab = "Number of Words", ylab = "Number of Reviews")
    
  })
  
  output$ratingvswordlength <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    words_list = strsplit(as.character(myfile()$reviews.text), " ")
    # Words per review 
    words_per_review = sapply(words_list, length)
    #	How does review length differ by rating?
    data <- cbind(words_per_review, myfile())
    ggplot(data=data,aes(x=words_per_review, fill = reviews.rating))+
      geom_histogram(bins=40)+ 
      labs(title="Distribution of product ratings",
           x="Number of words per review",
           y="Frequency"
      )
  })
  
  output$spellvsrating <- renderPlot({
    
    validate(
      need((!is.null(input$file)),
           "Upload your CSV file."
      )
    )
    
    #punctuation errors
    spellng_error <- has_endmark(myfile()$reviews.text, endmarks = c("?",".","!"))
    
    data <- cbind(myfile(), spellng_error)
    ggplot(data = data,aes(x =reviews.rating ,fill = spellng_error))+
      geom_bar(position = "fill")
  })
}
