library(ggplot2)
#import the dataset 
data2<-read.csv(file.choose(),header=TRUE)
#plotting the graph
ggplot(data=data2,aes(x=reviews.rating))+
  geom_histogram(bins=40)+
  labs(title="Distribution of product ratings",
       x="Review ratings",
       y="Number of products"
       )