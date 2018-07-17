library(hunspell)
library(textclean)
library(utils)
library(ggplot2)

#import the dataset 

mydata <- read.csv(file.choose(),header = T,",")

#wrong words

miss_spelled_words <- hunspell_check(as.character(mydata$reviews.text),dict = dictionary("en_US"))

#proportion of words with errors

length(miss_spelled_words)

data2 <- cbind(mydata,miss_spelled_words)
data2
#plotting mispelled words
ggplot(data = data2,aes(x = reviews.text, fill = miss_spelled_words))+
  geom_bar(position = "fill") 