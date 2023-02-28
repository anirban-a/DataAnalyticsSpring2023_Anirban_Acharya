library(ggplot2) # we will use ggplot2 to visualize the data.
head(iris) # first 6 rows of the
str(iris) # take a look at the structure of the iris data using str() function in R.

summary(iris) # summary statistics of all the 4 variables Sepal.Length,Sepal.Width,
# Petal.Length and Petal.Width

sapply(iris[,-5], var)
summary(iris)
# plot Sepal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()

# kmeans clustering

set.seed(300)
k.max <- 12

# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.

?kmeans
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)