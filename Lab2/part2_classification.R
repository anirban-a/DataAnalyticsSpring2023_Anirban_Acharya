# Exercise 2: Classification
abalone_df <- read.csv('../data/abalone.csv')

# transform "Sex" variable from character to integer
Sex<-c()
for(i in 1:nrow(abalone_df)){
  sex<-abalone_df[i,"Sex"]
  if(sex=="M")Sex<-append(Sex, 1)
  else if(sex=="F")Sex<-append(Sex, 2)
  else Sex<-append(Sex, 3)
}
abalone_df<-abalone_df[,2:ncol(abalone_df)]
abalone_df<-cbind(abalone_df, Sex)


# Need to predict the ring count.
train_row_cnt <- floor(0.8*nrow(abalone_df))

abalone_train <- abalone_df[1:train_row_cnt, c("Length","Diameter","Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Sex")]
abalone_train_cl <- as.numeric(abalone_df[1:train_row_cnt, "Rings"])

min(which(is.na(abalone_df[train_row_cnt+1:nrow(abalone_df),])))
# NOTE: all values are NA after ~ (train_row_cnt+836) row
test_abalone <- abalone_df[(train_row_cnt+1):(train_row_cnt+836), c("Length","Diameter","Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Sex")]



expected_abalone_rings <- abalone_df[(train_row_cnt+1):(train_row_cnt+836), "Rings"]

score<- function(a,b){
  c<-a-b
  c<-c*c
  length(c[c==0])/length(c)
}

knn_res<-as.numeric(knn3Train(abalone_train, test_abalone, abalone_train_cl, k=7, prob = FALSE))

# Accuracy Score:
score(knn_res, expected_abalone_rings)

# the score is too low. Let's check which features are having stronger correlations with the ring count.

# Rings vs Length
rings_by_avg_length <- dplyr::group_by(abalone_df, Rings)|>dplyr::summarise(Avg_Length=mean(Length))

ggplot(rings_by_avg_length)+
  geom_point(mapping=aes(x=Avg_Length, y=Rings))

# Rings vs Diameter

rings_by_avg_diam <- dplyr::group_by(abalone_df, Rings)|>dplyr::summarise(Avg_Diameter=mean(Diameter))

ggplot(rings_by_avg_diam)+
  geom_point(mapping=aes(y=Avg_Diameter, x=Rings))

# Rings vs Height

rings_by_avg_height <- abalone_df|>dplyr::group_by(Rings)|>dplyr::summarise(Avg_Height=mean(Height))

ggplot(rings_by_avg_height)+
  geom_point(mapping=aes(y=Avg_Height, x=Rings))

# Rings vs Whole.weight

rings_vs_wwt <- abalone_df|>dplyr::group_by(Rings)|>dplyr::summarise(Avg_Whole_Wt=mean(Whole.weight))

ggplot(rings_vs_wwt)+
  geom_point(mapping = aes(x=Rings,y=Avg_Whole_Wt))

# Rings vs Shucked.weight

rings_vs_shwt <- abalone_df|>dplyr::group_by(Rings)|>dplyr::summarise(Avg_Shucked_Wt=mean(Shucked.weight))

ggplot(rings_vs_shwt)+
  geom_point(mapping = aes(x=Rings,y=Avg_Shucked_Wt))

ggplot(abalone_df)+
  geom_point(aes(x=Shucked.weight, y=Whole.weight, colour=factor(Rings)))

ggplot(abalone_df)+
  geom_density(mapping=aes(Whole.weight))

mean_ww <- mean(abalone_df$Whole.weight)
hist(abalone_df$Whole.weight, probability = TRUE)

range_scale <- function(data){
  min_v <- min(data)
  max_v <- max(data)
  return((data-min_v)/(max_v-min_v))
}

# create a copy of the original data-set
abalone <- data.frame(abalone_df)

# Log scaling the height.
abalone$Height <- log(abalone$Height)

z_score_scaling <- function(data){
  d<-sd(data)
  m<-mean(data)
  return((data-m)/d)
}

# Range scaling length
abalone_df$Length <- range_scale(abalone_df$Length)

hist(range_scale(abalone_df$Whole.weight), probability = TRUE)
hist(range_scale(abalone_df$Shucked.weight), probability = TRUE)

# Range scaling weights
abalone_df[2:7]<-lapply(abalone_df[2:7], range_scale)


boxplot(abalone$Diameter)
hist(range_scale(abalone$Diameter), probability = T)

# Need to predict the ring count.
train_row_cnt <- floor(0.8*nrow(abalone_df))

abalone_train <- abalone_df[1:train_row_cnt, c("Length","Diameter","Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Sex")]
abalone_train_cl <- as.numeric(abalone_df[1:train_row_cnt, "Rings"])

# NOTE: all values are NA after ~ (train_row_cnt+836) row
test_abalone <- abalone_df[(train_row_cnt+1):(train_row_cnt+836), c("Length","Diameter","Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Sex")]

expected_abalone_rings <- abalone_df[(train_row_cnt+1):(train_row_cnt+836), "Rings"]


library(class)
knn_pred <- knn(train = abalone_train, test = test_abalone, cl = abalone_train_cl, k=57)

# 
# knn_res<-as.numeric(knn3Train(abalone_train, test_abalone, abalone_train_cl, k=7, prob = FALSE))
# 
# # Accuracy Score:
score(as.numeric(knn_pred), expected_abalone_rings)
