library(wordcloud)
library(SnowballC)
library(tm)
library(sentiment)
library(ggplot2)
library(plyr)
library(RColorBrewer)


mydata <- read.csv(file.choose(),header = T,",")

df <- data.frame(mydata)

textdata <- df[df$mydata, ]

#creating corpus

mycorpus <- Corpus(VectorSource(mydata))

#creating plain text

mycorpus <- tm_map(mycorpus, PlainTextDocument)

#removing punctuations

mycorpus <- tm_map(mycorpus, removePunctuation)
#removing stopwords

mycorpus <-tm_map(mycorpus, removeWords,stopwords(kind="en"))

#do steaming

mycorpus <- tm_map(mycorpus, stemDocument)

try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL

#performing sentiment analysis

classify_emotion <-function(textdata, algorithm="bayes", prior=1.0){
  emotion <- class_emo[,7]
  emotion[is.na(emotion)] = "unknown" 
  
  # data frame with results
  
  sent_df <- data.frame(text=textdata, emotion=emotion,
                        polarity=polarity, stringsAsFactors=FALSE)
  #sort the data frame
  
  sent_df <- within(sent_df,
                    emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
 
}

classify_polarity <-function(textdata, algorithm="bayes"){
  polarity <- class_pol[,4]
}




#plotting the emotions

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")
