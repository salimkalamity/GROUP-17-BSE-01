library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

#loadind data

mydata <- read.csv(file.choose(),header = T,",")

#creating the corpus object

wc <- VectorSource(mydata$reviews.text)
wc <- Corpus(wc)

#pre-processing data

wc <- tm_map(wc, removePunctuation)
wc <- tm_map(wc, content_transformer(tolower))
wc <- tm_map(wc, removeWords, stopwords("english"))
wc <- tm_map(wc, stripWhitespace)

#creating the word cloud

wordcloud(words = wc, width = 1000, height = 1000,  min.freq = 4, max.words=Inf ,colors=brewer.pal(8, "Dark2"))
