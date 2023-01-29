setwd("~/Documents/Codes/DataAnalyticsSpring2023_Anirban_Acharya/Lab1-b/")

library("tidyverse")

epi_data <- read.csv("EPI2010_data.csv")

colnames(epi_data)<-as.character(epi_data[1,])
epi_data<-epi_data[-1,]
epi_data$EPI<-as.numeric(epi_data$EPI)

plot(ecdf(epi_data$EPI), do.points=F, verticals = T)
plot(ecdf(epi_data$EPI), do.points=T, verticals = T)

ggplot(data=epi_data)+
  geom_density(aes(EPI))

qqline(epi_data$EPI)



epi_data$WATER_H<-as.numeric(epi_data$WATER_H)
epi_data$DALY<-as.numeric(epi_data$DALY)

plot(ecdf(epi_data$WATER_H), do.points=F, verticals = T)
plot(ecdf(epi_data$DALY), do.points=F, verticals = T)

boxplot(epi_data$EPI, epi_data$DALY, epi_data$WATER_H)


# Scatter plot for Water_H against EPI
ggplot(data=epi_data)+
  geom_point(mapping = aes(x=WATER_H, y=EPI))
