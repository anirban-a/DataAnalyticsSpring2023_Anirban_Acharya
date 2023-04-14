library(MASS)
library(tree)
set.seed(1)
head(Boston)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)

# Regression Tree
tree(formula = medv ~. , data = Boston, subset = train)
# We now plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)
# The variable "lstat" measure the percentage of the individuals with lower socioeconomics status.
# The tree indicates that the lower values of lstat corresponds to more expensive house.
# Now we use the cv.tree() function to see whether pruning the tree will
# improve performance.
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,typ ='b')


## Prunning

# Regression Tree
tree(formula = medv ~. , data = Boston, subset = train)
# We now plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)
# The variable "lstat" measure the percentage of the individuals with lower socioeconomics status.
# The tree indicates that the lower values of lstat corresponds to more expensive house.
# Now we use the cv.tree() function to see whether pruning the tree will
# improve performance.
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,typ ='b')


yhat=predict(tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
# adding the abline()
abline(0,1)
mean((yhat-boston.test)^2)


# Bagging and Random Forest Example
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data=Boston, subset = train, mtry=13, importance= TRUE)
bag.boston
# The argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree—in other words, that bagging should be done.
# How well does this bagged model perform on the test set?
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
plot(yhat.bag, boston.test)
# adding the abline()
abline(0,1)
mean((yhat.bag-boston.test)^2)


bad.boston =
  bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,ntree=25)
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
mean((yhat.bag-boston.test)^2)


set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)


importance (rf.boston)
# Two measures of variable importance are reported.
#The former is based upon the mean decrease of accuracy in predictions on
# the out of bag samples when a given variable is excluded from the model.
#The latter is a measure of the total decrease in node impurity that results
# from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS,
# and for classification trees by the deviance.
# Plots of these importance measures can be produced using the varImpPlot() function.
varImpPlot (rf.boston)