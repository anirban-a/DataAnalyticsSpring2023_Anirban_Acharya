install.packages("dplyr")
install.packages("purr")

library(dplyr)
library(purrr)

epi_data <- read.csv("EPI2010_data.csv")

epi.treated <- epi_data

# calculate means for each numeric columns
epi.means<-1:ncol(epi.treated) |> map(function(col_idx){
  if(is.numeric(epi.treated[1,col_idx])){
    mean(epi.treated[,col_idx], na.rm=TRUE)
  }
})

geo.subregions<-as.vector(unique(epi.treated$GEO_subregion))
gdp_means_by_subregions<-geo.subregions |>
  map(function(subregion) mean(epi.treated$GDPCAP07[epi.treated$GEO_subregion==subregion], na.rm = TRUE))

mean_gdp_by_subregion<-NULL
for(i in 1:length(geo.subregions)){
  mean_gdp_by_subregion[geo.subregions[i]]<-gdp_means_by_subregions[i]
}

# population density = Population/Landarea
# GDPCAP = mean of GDPCAP by sub-region for missing value
for(i in 1:nrow(epi.treated)){
  for(j in 1:ncol(epi.treated)){
    if(is.na(epi.treated[i,j]) && is.numeric(epi.treated[i,j])){
      if(colnames(epi.treated[j])=="PopulationDensity"){
        epi.treated$PopulationDensity[i]<-epi.treated$Population07[i]/epi.treated$Landarea[i]
      }else if(colnames(epi.treated[j])=="GDPCAP07"){
        epi.treated$GDPCAP07[i]<-mean_gdp_by_subregion[[epi.treated$GEO_subregion[i]]]
      }
      else{
        
        # This is not a good approximation for several features like EPI, Population, etc., need to figure out a better way to do replace NA.
        epi.treated[i,j]<-epi.means[j] 
      }
    }
  }
}

# EPI stats before treatment
summary(epi_data$EPI)
# EPI stats after treatment
summary(epi.treated$EPI)

# Equivalent to EPI stats above but without the NAs.
fivenum(epi_data$EPI, na.rm = TRUE)

stem(epi_data$EPI)
stem(epi_data$EPI, scale = 2.5)

sort(unique(epi_data$EPI, na.rm=TRUE))

hist(epi_data$EPI)
hist(epi.treated$EPI)

# Histogram for values in range [30, 95] with probability densities.
hist(epi_data$EPI, seq(30., 95., 1.0), prob=TRUE)

lines(density(epi_data$EPI, na.rm = TRUE, bw = "SJ"))

rug(epi_data$EPI, col = "RED")

plot(ecdf(epi_data$EPI), do.points=FALSE, verticals = T)

par(pty="s")

qqnorm(epi_data$EPI)