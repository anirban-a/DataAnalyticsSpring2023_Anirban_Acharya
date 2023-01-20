install.packages("dplyr")
install.packages("purr")

library(dplyr)
library(purrr)

epi_data <- read.csv("EPI2010_data.csv")

epi.treated <- epi_data

epi.means<-1:ncol(epi.treated) %>% map(function(col_idx){
  if(is.numeric(epi.treated[1,col_idx])){
    mean(epi.treated[,col_idx], na.rm=TRUE)
  }
})

for(i in 1:nrow(epi.treated)){
  for(j in 1:ncol(epi.treated)){
    if(is.na(epi.treated[i,j]) && is.numeric(epi.treated[i,j])){
      epi.treated[i,j]<-epi.means[j]
    }
  }
}