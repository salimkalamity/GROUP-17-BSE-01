library(hunspell)
library(textclean)
library(utils)
library(ggplot2)

#import the dataset 

mydata <- read.csv(file.choose(),header = T,",")

#punctuation errors

punc <- has_endmark(mydata$reviews.text, endmarks = c("?",".","!"))

#number of sentences that dont end with punctuation

length(punc)
