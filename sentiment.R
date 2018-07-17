library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
mydata <- read.csv(file.choose(),header = T,",")
get_data<-iconv(mydata$reviews.text)
s<-get_nrc_sentiment(get_data)
head(s)

barplot(colSums(s),
        las=2, 
        col = rainbow(10),
        ylab = "count",
        main = "Sentiment scores for Product Reviews")

