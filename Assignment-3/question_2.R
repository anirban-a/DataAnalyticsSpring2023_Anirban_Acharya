library(ggplot2)

nyt1 <- read.csv('../data/nytimes-assignment-3/nyt10.csv')
nyt2 <- read.csv('../data/nytimes-assignment-3/nyt11.csv')
nyt3 <- read.csv('../data/nytimes-assignment-3/nyt12.csv')
nyt4 <- read.csv('../data/nytimes-assignment-3/nyt13.csv')




# Shapiro-Wilk Test:

# get a sample of lesser size:
s_nyt1 <- nyt1[sample(nrow(nyt1), 5000, replace = F),]
s_nyt2 <- nyt1[sample(nrow(nyt2), 5000, replace = F),]
s_nyt3 <- nyt1[sample(nrow(nyt3), 5000, replace = F),]
s_nyt4 <- nyt1[sample(nrow(nyt4), 5000, replace = F),]


shapiro.test(s_nyt1$Gender)
shapiro.test(s_nyt2$Gender)
shapiro.test(s_nyt3$Gender)
shapiro.test(s_nyt4$Gender)



shapiro.test(s_nyt1$Impressions)
shapiro.test(s_nyt2$Impressions)
shapiro.test(s_nyt3$Impressions)
shapiro.test(s_nyt4$Impressions)


# Histograms
ggplot(nyt1, aes(Gender))+
  geom_histogram()

ggplot(nyt1, aes(Impressions))+
  geom_histogram()

ggplot(nyt2, aes(Gender))+
  geom_histogram()

ggplot(nyt2, aes(Impressions))+
  geom_histogram()

ggplot(nyt3, aes(Gender))+
  geom_histogram()

ggplot(nyt3, aes(Impressions))+
  geom_histogram()

ggplot(nyt4, aes(Gender))+
  geom_histogram()

ggplot(nyt4, aes(Impressions))+
  geom_histogram()



# ECDF
plot(ecdf(nyt1$Gender), do.points=FALSE, verticals = T)
plot(ecdf(nyt1$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt2$Gender), do.points=FALSE, verticals = T)
plot(ecdf(nyt2$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt3$Gender), do.points=FALSE, verticals = T)
plot(ecdf(nyt3$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt4$Gender), do.points=FALSE, verticals = T)
plot(ecdf(nyt4$Impressions), do.points=FALSE, verticals = T)


# QQ plot for normal distribution:
qqnorm(nyt1$Gender)
qqline(nyt1$Gender, distribution = qnorm)

qqnorm(nyt1$Impressions)
qqline(nyt1$Impressions, distribution = qnorm)


qqnorm(nyt2$Gender)
qqline(nyt2$Gender, distribution = qnorm)

qqnorm(nyt2$Impressions)
qqline(nyt2$Impressions, distribution = qnorm)

qqnorm(nyt3$Gender)
qqline(nyt3$Gender, distribution = qnorm)

qqnorm(nyt3$Impressions)
qqline(nyt3$Impressions, distribution = qnorm)

qqnorm(nyt4$Gender)
qqline(nyt4$Gender, distribution = qnorm)

qqnorm(nyt4$Impressions)
qqline(nyt4$Impressions, distribution = qnorm)


# These QQ plots clearly show that the distributions are not normal. 
# For the Impressions, there are lesser data points to the left and more data points to the right if we
# assumed a normal distribution.


# Significance test
wilcox.test(nyt1$Gender, nyt1$Impressions)
wilcox.test(nyt2$Gender, nyt2$Impressions)
wilcox.test(nyt3$Gender, nyt3$Impressions)
wilcox.test(nyt4$Gender, nyt4$Impressions)
