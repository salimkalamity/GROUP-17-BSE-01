library(SentimentAnalysis)
library(ggplot2)

#import the csv file

mydata <- read.csv(file.choose(),header = T,",")



doc <- data.frame(mydata)
sentiment <- analyzeSentiment(doc)$SentimentQDAP
convertToBinaryResponse(sentiment)

  