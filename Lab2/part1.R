library("tidyverse")
library(r2r)
library(ggplot2)

path<-"../data/EPI2010_data.csv"
raw_epi_data<-read.csv(path)
colnames(raw_epi_data)<-raw_epi_data[1,]

# make the first row of the CSV file as header
epi_data<-raw_epi_data[-1,]
epi_data$EPI<-as.numeric(epi_data$EPI)

# Part 1(a)
# Measures of Central Tendencies.

epi_data$NOX_pt<-as.numeric(epi_data$NOX_pt)
epi_data$SO2_pt<-as.numeric(epi_data$SO2_pt)

mode_ <- function(series){
  freq_t <- table(series)
  max_count <- max(as.vector(freq_t))
  # print(max_count)
  if(max_count<=1){
    # if max_count is 1 or 0 then either all elements are unique or there isn't any element.
    return (NULL)
  }
  m<-hashmap(default = 0)
  for(i in series){
    m[[i]]<-m[[i]]+1
  }
  modes<-c()
  for(i in keys(m)){
    if(m[[i]]==max_count){
      modes<-append(modes, i)
    }
  }
  modes
}

# Mean of NOX_pt
mean(epi_data$NOX_pt, na.rm = T)

# Median of NOX_pt
median(epi_data$NOX_pt, na.rm = T)

# Mode of NOX_pt
mode_(epi_data$NOX_pt)

# Mean of SO2_pt
mean(epi_data$SO2_pt, na.rm = T)

# Median of SO2_pt
median(epi_data$SO2_pt, na.rm = T)

# Mode of SO2_pt
mode_(epi_data$SO2_pt)

# Box plot for OZONE_pt
epi_data$OZONE_pt <- as.numeric(epi_data$OZONE_pt)

ggplot(epi_data)+
  geom_boxplot(mapping = aes(EPI_regions, OZONE_pt, color=EPI_regions), na.rm=T)+
  labs(x="EPI regions")

# NOTE: Ask prof -> Why is the plot coming this way.
ggplot(epi_data)+
  geom_boxplot(mapping = aes(EPI_regions, WQI_pt), na.rm=T)


epi_data$CLIMATE<-as.numeric(epi_data$CLIMATE)
epi_data$AGRICULTURE<-as.numeric(epi_data$AGRICULTURE)

# Central Tendency values for CLIMATE
mean(epi_data$CLIMATE, na.rm = T)
median(epi_data$CLIMATE, na.rm = T)
mode_(epi_data$CLIMATE) # multi-modal variable.

# Central Tendency values for AGRICULTURE
mean(epi_data$AGRICULTURE, na.rm = T)
median(epi_data$AGRICULTURE, na.rm = T)
mode_(epi_data$AGRICULTURE) # multi-modal variable.

# Boxplots for FISHERIES
epi_data$FISHERIES<-as.numeric(epi_data$FISHERIES, na.rm=T)

ggplot(epi_data)+
  geom_boxplot(mapping = aes(EPI_regions, FISHERIES, color=EPI_regions))

#  Boxplots for NMVOC_pt
epi_data$NMVOC_pt<-as.numeric(epi_data$NMVOC_pt)

ggplot(epi_data)+
  geom_boxplot(mapping = aes(EPI_regions, NMVOC_pt, color=EPI_regions))

epi_data$ENVHEALTH<-as.numeric(epi_data$ENVHEALTH)
epi_data$ECOSYSTEM<-as.numeric(epi_data$ECOSYSTEM)

# boxplot(ENVHEALTH,ECOSYSTEM)
boxplot(epi_data$ENVHEALTH, epi_data$ECOSYSTEM)

qqplot(epi_data$ENVHEALTH, epi_data$ECOSYSTEM, xlab = "ENVHEALTH", ylab = "ECOSYSTEM")

# Part 1b

ggplot(epi_data)+
  geom_boxplot(aes(EPI_regions, EPI, color=EPI_regions))

# box plot
ggplot(epi_data)+
  geom_boxplot(aes(GEO_subregion, EPI, color=EPI_regions))

# inference: Western Europe has highest EPI, Western Africa has the lowest EPI
colnames(epi_data)

w_eu_epi_data<-filter(epi_data, GEO_subregion=='Western Europe')
w_af_epi_data<-filter(epi_data, GEO_subregion=='Western Africa')
w_eu_epi_data$PopulationDensity07<-as.numeric(w_eu_epi_data$PopulationDensity07)
w_af_epi_data$PopulationDensity07<-as.numeric(w_af_epi_data$PopulationDensity07)

w_af_epi_data$AIR_H<-as.numeric(w_af_epi_data$AIR_H)
w_eu_epi_data$AIR_H<-as.numeric(w_eu_epi_data$AIR_H)

boxplot(w_af_epi_data$PopulationDensity07, w_eu_epi_data$PopulationDensity07)
boxplot(w_af_epi_data$AIR_H, w_eu_epi_data$AIR_H)
# AIR_H of W. Europe > W. Africa


boxplot(w_eu_epi_data$ENVHEALTH, w_af_epi_data$ENVHEALTH)
# ENVHEALTH of W. Europe > W. Africa

w_eu_epi_data$DALY<-as.numeric(w_eu_epi_data$DALY)
w_af_epi_data$DALY<-as.numeric(w_af_epi_data$DALY)

boxplot(w_eu_epi_data$DALY, w_af_epi_data$DALY)
# DALY of W. Europe > W. Africa

w_eu_epi_data$WATER_H<-as.numeric(w_eu_epi_data$WATER_H)
boxplot(w_eu_epi_data$DALY, w_eu_epi_data$ENVHEALTH, w_eu_epi_data$AIR_H, w_eu_epi_data$WATER_H)

# WATER_H seems like the an important factor for improving EPI.


lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H, w_eu_epi_data)

summary(lmENVH)

cENVH <- coefficients(lmENVH)

DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))

NEW<-data.frame(DALY=DALYNEW,AIR_H=AIR_HNEW,WATER_H=WATER_HNEW)

pENV<-predict(lmENVH,NEW,interval="prediction")

cENV<-predict(lmENVH,NEW,interval="confidence")

air_e_mod <- lm(AIR_E~DALY+AIR_H+WATER_H, w_eu_epi_data)

pred_air_e <- predict(air_e_mod, NEW, interval = "prediction")
conf_air_e <- predict(air_e_mod, NEW, interval = "confidence")

climate_mod <- lm(CLIMATE~DALY+AIR_H+WATER_H, w_eu_epi_data)

pred_climate <- predict(climate_mod, NEW, interval = "prediction")
conf_climate <- predict(climate_mod, NEW, interval = "confidence")

# Part 1 c Shapiro-Wilk Test
shapiro.test(epi_data$ENVHEALTH)
# p-value = 8.17e-08 <<< 0.05. This implies that ENVHEALTH is non-normal.
# Plotting a Q-Q plot to verify graphically:
qqnorm(epi_data$ENVHEALTH)
qqline(epi_data$ENVHEALTH)
# The plot indicates: there are less observations at extreme ends
hist(epi_data$ENVHEALTH, probability = T, breaks = 30)
lines(density(epi_data$ENVHEALTH, na.rm = T), col="blue")

shapiro.test(epi_data$ECOSYSTEM)

epi_data$DALY<-as.numeric(epi_data$DALY)
shapiro.test(epi_data$DALY)

epi_data$AIR_H<-as.numeric(epi_data$AIR_H)
shapiro.test(epi_data$AIR_H)

epi_data$AIR_H<-as.numeric(epi_data$AIR_H)
shapiro.test(epi_data$AIR_H)

# Repeat the same for EPI_Data.csv

df <- read.csv('../data/EPI_Data.csv')

shapiro.test(df$ENVHEALTH)
# p-value is ~ 1e-8 <<< 0.05 hence rejecting.
qqnorm(df$ENVHEALTH)
qqline(df$ENVHEALTH)

ggplot(data=df)+
  geom_density(mapping=aes(ENVHEALTH))

ggplot(data = df)+
  geom_freqpoly(mapping = aes(ENVHEALTH))

shapiro.test(df$DALY)

shapiro.test(df$AIR_H)

shapiro.test(df$WATER_H)
