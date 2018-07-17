library(hunspell)
library(textclean)
library(utils)
library(ggplot2)
mydata <- read.csv(file.choose(),header = T,",")
#wrong words
miss_spelled_words <- hunspell_find(as.character(mydata$reviews.text),format = "latex")

#punctuation errors
 punc <- has_endmark(mydata$reviews.text, endmarks = c("?",".","!"))

 data1 <- cbind(mydata, punc)
 ggplot(data = data1,aes(x =reviews.rating ,fill = punc))+
   geom_bar(position = "fill")
