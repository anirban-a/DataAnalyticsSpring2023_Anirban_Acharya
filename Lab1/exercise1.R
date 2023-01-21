install.packages("dplyr")
install.packages("purr")

library(dplyr)
library(purrr)

epi_data <- read.csv("EPI2010_data.csv")

epi.treated <- epi_data

# calculate means for each numeric columns
epi.means<-1:ncol(epi.treated) %>% map(function(col_idx){
  if(is.numeric(epi.treated[1,col_idx])){
    mean(epi.treated[,col_idx], na.rm=TRUE)
  }
})

# population density = Population/Landarea
for(i in 1:nrow(epi.treated)){
  for(j in 1:ncol(epi.treated)){
    if(is.na(epi.treated[i,j]) && is.numeric(epi.treated[i,j])){
      if(colnames(epi.treated[j])=="PopulationDensity"){
        epi.treated$PopulationDensity[i]<-epi.treated$Population07[i]/epi.treated$Landarea[i]
      }else{
        
        # This is not a good approximation for several features like GDP, Population, etc., need to figure out a better way to do replace NA.
        epi.treated[i,j]<-epi.means[j] 
      }
    }
  }
}