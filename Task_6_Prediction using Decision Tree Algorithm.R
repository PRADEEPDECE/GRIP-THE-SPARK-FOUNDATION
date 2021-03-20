#Author: PRADEEP K 
#Prediction using Decision Tree Algorithm
#Create the Decision Tree classifier and visualize it graphically.

library(rpart)
library(rpart.plot)
library(e1071)
head(iris)
iris
decision_tree_model<-rpart(Species ~ ., data = iris, method = "class")
decision_tree_model
rpart.plot(decision_tree_model)

iris$Species_Predicted<-predict(decision_tree_model,newdata=iris,type="class")


table(iris$Species,iris$Species_Predicted)

# The model Creation
library(caTools)
set.seed(123) 
split <- sample.split(iris, SplitRatio = 0.7)
split
train <- subset(iris, split== "TRUE")
test <- subset(iris, split== "FALSE")
train
test

# Train model on train data
decision_tree_model<-rpart(Species ~ ., data = train, method = "class")

#Detailed information about the each node
summary(decision_tree_model)

#Plot the decision tree using plot fucntion
plot(decision_tree_model, uniform = TRUE, branch = 0.6, margin = 0.1)
text(decision_tree_model, all = TRUE, use.n = TRUE)

#Plot the decision tree using rpart.plot fucntion
rpart.plot(decision_tree_model)


# Predict Species on test data
test$Species_Predicted<-predict(decision_tree_model,newdata=test,type="class")
table(test$Species,test$Species_Predicted)

library(caret)
confusionMatrix(table(test$Species,test$Species_Predicted))

####################################################################################
#Tree Pruning

printcp(decision_tree_model) 
?printcp

#Minimum error occurs when the tree size is = 3
plotcp(decision_tree_model)
?plotcp
#Find the value of CP for which cross validation error is minimum
min(decision_tree_model$cptable[,"xerror"])
which.min(decision_tree_model$cptable[,"xerror"])
cpmin <- decision_tree_model$cptable[3, "CP"]

#Prune the tree by setting the CP parameter as =  cpmin
decision_tree_pruned = prune(decision_tree_model, cp = cpmin)
rpart.plot(decision_tree_pruned)

# Predict Species on test data
test$Species_Predicted<-predict(decision_tree_pruned,newdata=test,type="class")
table(test$Species,test$Species_Predicted)
confusionMatrix(table(test$Species,test$Species_Predicted))
