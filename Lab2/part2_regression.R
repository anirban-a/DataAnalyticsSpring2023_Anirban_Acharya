df <- read.csv('../data/dataset_multipleRegression.csv')

library(plotly)
library(dplyr)
options(max.print=1000000)

# Exercise 1: Regression

# Using the unemployment rate (UNEM) and number of spring high school graduates
# (HGRAD), predict the fall enrollment (ROLL) for this year by knowing that UNEM=7% and HGRAD=90,000


# Let's plot the existing relation between the dependent and independent variables:

fig<-plot_ly(df, x=df$HGRAD, y=df$UNEM, z=df$ROLL, color=df$ROLL, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers
fig <- fig %>% layout(scene=list(
  xaxis=list(title='High School Graduates'),
  yaxis=list(title='Unemployment Rate'),
  zaxis=list(title='Fall Enrollment')
  ))

# display the 3-D scatter plot.
fig

# build model
enrollment_model <- lm(ROLL~UNEM+HGRAD, df)

# create new data frame to pass to the model
new_UNEM <- data.frame(UNEM=c(7), HGRAD=c(90000))

# get prediction
p_enrollment <- predict(enrollment_model, new_UNEM)%>%floor

# Prepare a new data frame to plot the values along with the predicted outcome.
new_UNEM_df <- cbind(new_UNEM, ROLL=c(p_enrollment))

new_df <- rbind(df[,c("UNEM", "HGRAD","ROLL")], new_UNEM_df[1,])

fig2<-plot_ly(new_df, x=new_df$HGRAD, y=new_df$UNEM, z=new_df$ROLL, color=new_df$ROLL, colors = c('#65e73f', '#e06666','#990000'))
fig2 <- fig2 %>% add_markers
fig2 <- fig2 %>% layout(scene=list(
  xaxis=list(title='High School Graduates', range=c(8000, 90000)),
  yaxis=list(title='Unemployment Rate'),
  zaxis=list(title='Fall Enrollment')
))

fig2

# Repeat the same including INC=$25,000
enrollment_model <- lm(ROLL~UNEM+HGRAD+INC, df)

# create new data frame to pass to the model
new_UNEM <- data.frame(UNEM=c(7), HGRAD=c(90000), INC=c(25000))

# get prediction
p_enrollment <- predict(enrollment_model, new_UNEM)%>%floor





