
file <- read.csv(file.choose(), T, ",")
hist(file$reviews.rating, main = "Distribution of Product Rating", xlab = "Rating", col = "Yellow")
