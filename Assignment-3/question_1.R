library(ggplot2)

nyt1 <- read.csv('../data/nytimes-assignment-3/nyt10.csv')
nyt2 <- read.csv('../data/nytimes-assignment-3/nyt11.csv')
nyt3 <- read.csv('../data/nytimes-assignment-3/nyt12.csv')
nyt4 <- read.csv('../data/nytimes-assignment-3/nyt13.csv')
nyt5 <- read.csv('../data/nytimes-assignment-3/nyt14.csv')
nyt6<- read.csv('../data/nytimes-assignment-3/nyt15.csv')
nyt7<- read.csv('../data/nytimes-assignment-3/nyt16.csv')
# verify if there's any NA value in the dataset.
sum(is.na(nyt1))

summary(nyt1)
# 1.a:

# Age & Impressions

boxplot(nyt1$Age, nyt1$Impressions)
boxplot(nyt2$Age, nyt2$Impressions)
boxplot(nyt3$Age, nyt3$Impressions)
boxplot(nyt4$Age, nyt4$Impressions)
boxplot(nyt5$Age, nyt5$Impressions)
boxplot(nyt6$Age, nyt6$Impressions)
boxplot(nyt7$Age, nyt7$Impressions)

# Summary statistics:

summary(nyt1)
summary(nyt2)
summary(nyt3)
summary(nyt4)
summary(nyt5)
summary(nyt6)
summary(nyt7)

# 1.b:

# Shapiro-Wilk Test:

# get a sample of lesser size:
s_nyt1 <- nyt1[sample(nrow(nyt1), 5000, replace = F),]
s_nyt2 <- nyt1[sample(nrow(nyt2), 5000, replace = F),]
s_nyt3 <- nyt1[sample(nrow(nyt3), 5000, replace = F),]
s_nyt4 <- nyt1[sample(nrow(nyt4), 5000, replace = F),]
s_nyt5 <- nyt1[sample(nrow(nyt5), 5000, replace = F),]
s_nyt6 <- nyt1[sample(nrow(nyt6), 5000, replace = F),]
s_nyt7 <- nyt1[sample(nrow(nyt7), 5000, replace = F),]

shapiro.test(s_nyt1$Age)
shapiro.test(s_nyt2$Age)
shapiro.test(s_nyt3$Age)
shapiro.test(s_nyt4$Age)
shapiro.test(s_nyt5$Age)
shapiro.test(s_nyt6$Age)
shapiro.test(s_nyt7$Age)


shapiro.test(s_nyt1$Impressions)
shapiro.test(s_nyt2$Impressions)
shapiro.test(s_nyt3$Impressions)
shapiro.test(s_nyt4$Impressions)
shapiro.test(s_nyt5$Impressions)
shapiro.test(s_nyt6$Impressions)
shapiro.test(s_nyt7$Impressions)

# Histograms
ggplot(nyt1, aes(Age))+
  geom_histogram()

ggplot(nyt1, aes(Impressions))+
  geom_histogram()

ggplot(nyt2, aes(Age))+
  geom_histogram()

ggplot(nyt2, aes(Impressions))+
  geom_histogram()

ggplot(nyt3, aes(Age))+
  geom_histogram()

ggplot(nyt3, aes(Impressions))+
  geom_histogram()

ggplot(nyt4, aes(Age))+
  geom_histogram()

ggplot(nyt4, aes(Impressions))+
  geom_histogram()

ggplot(nyt5, aes(Age))+
  geom_histogram()

ggplot(nyt5, aes(Impressions))+
  geom_histogram()


ggplot(nyt6, aes(Age))+
  geom_histogram()

ggplot(nyt6, aes(Impressions))+
  geom_histogram()


ggplot(nyt7, aes(Age))+
  geom_histogram()

ggplot(nyt7, aes(Impressions))+
  geom_histogram()

# ECDF
plot(ecdf(nyt1$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt1$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt2$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt2$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt3$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt3$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt4$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt4$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt5$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt5$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt6$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt6$Impressions), do.points=FALSE, verticals = T)

plot(ecdf(nyt7$Age), do.points=FALSE, verticals = T)
plot(ecdf(nyt7$Impressions), do.points=FALSE, verticals = T)

# QQ plot for normal distribution:
qqnorm(nyt1$Age)
qqline(nyt1$Age, distribution = qnorm)

qqnorm(nyt1$Impressions)
qqline(nyt1$Impressions, distribution = qnorm)


qqnorm(nyt2$Age)
qqline(nyt2$Age, distribution = qnorm)

qqnorm(nyt2$Impressions)
qqline(nyt2$Impressions, distribution = qnorm)

qqnorm(nyt3$Age)
qqline(nyt3$Age, distribution = qnorm)

qqnorm(nyt3$Impressions)
qqline(nyt3$Impressions, distribution = qnorm)

qqnorm(nyt4$Age)
qqline(nyt4$Age, distribution = qnorm)

qqnorm(nyt4$Impressions)
qqline(nyt4$Impressions, distribution = qnorm)

qqnorm(nyt5$Age)
qqline(nyt5$Age, distribution = qnorm)

qqnorm(nyt5$Impressions)
qqline(nyt5$Impressions, distribution = qnorm)

qqnorm(nyt6$Age)
qqline(nyt6$Age, distribution = qnorm)

qqnorm(nyt6$Impressions)
qqline(nyt6$Impressions, distribution = qnorm)


qqnorm(nyt7$Age)
qqline(nyt7$Age, distribution = qnorm)

qqnorm(nyt7$Impressions)
qqline(nyt7$Impressions, distribution = qnorm)

# These QQ plots clearly show that the distributions are not normal. 
# For the Impressions, there are lesser data points to the left and more data points to the right if we
# assumed a normal distribution.


# Significance test
wilcox.test(nyt1$Age, nyt1$Impressions)
wilcox.test(nyt2$Age, nyt2$Impressions)
wilcox.test(nyt3$Age, nyt3$Impressions)
wilcox.test(nyt4$Age, nyt4$Impressions)
wilcox.test(nyt5$Age, nyt5$Impressions)
wilcox.test(nyt6$Age, nyt6$Impressions)
wilcox.test(nyt7$Age, nyt7$Impressions)

