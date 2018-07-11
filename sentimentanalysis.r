library(tm)
library(wordcloud)
library(SnowballC)
library(syuzhet)
library(stringr)
library(plyr)
library(ggplot2)
library(dplyr)

#Upload dataset
mydataset <-read.csv(file.choose(),T,",")

#Length of the reviews
length.reviews <- length(mydataset$reviews.text)
length.reviews

#Cleaning
mydataset1 <- gsub("http[^[:blank:]]+","",mydataset$reviews.text)
mydataset2 <- gsub("[^[:alnum:]]","",mydataset1)

#Sentiment Analysis
mydatasetsentiment <- get_nrc_sentiment(mydataset2)
sentimentmarks <- data.frame(colSums(mydatasetsentiment[,]))
names(sentimentmarks) <- "Mark"
sentimentmarks <- cbind("Sentiment" = rownames(sentimentmarks),sentimentmarks)
rownames(sentimentmarks) <- NULL
ggplot(data = sentimentmarks, xlab = "Sentiment", ylab = "Number of Reviews", ggtitle = "Total sentiment score based on Product Reviews")

get_nrc_sentiment("Am vey happy and  i want to dance")

