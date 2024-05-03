#8.machine learning - 6.1 ensembles comp check

#Q1 Use the training set to build a model with several of the models available from 
#the caret package. We will test out seven of the most common machine learning models 
#in this exercise:
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

# Apply all of these models using train() with all the default parameters

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


#Q2 use sapply() to create a matrix of predictions for the test set
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

#Q3 compute accuracy for each model on the test set
#report mean accuracy across all models

acc <- colMeans(pred == mnist_27$test$y) #colMeans creates means of columns
acc
mean(acc)

#Q4 Next, build an ensemble prediction by majority vote and compute the accuracy
#of the ensemble. Vote 7 if more than 50% of the models are predicting a 7, and 
# 2 otherwise.
# What is the accuracy of the ensemble?

vote <- rowMeans(pred == "7")
y_hat <- ifelse(vote > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5 in Q3 we computed the accuracy of each method on the test set and noticed that
# individual accuracies varied.  How many of the individual methods do better than
# the ensemble?
#which methods perform better than the ensemble?
acc

#OR
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#Q6 it is tempting to remove the methods that do not perform well and re-do the 
# emsemble.  The problem with this approach is that we are using the test data to
#make a decision. However, we could use the minimum accuracy estimates obtained 
#from cross validation with the training data for each model from fit$results$Accuracy. 
#Obtain these estimates and save them in an object. 
#What is the mean of these training set accuracy estimates?

acc_hat <-sapply(fits, function(fit) 
  min(fit$results$Accuracy))
mean(acc_hat)

#Q7 Now let's only consider the methods with a minimum accuracy estimate of greater
# than or equal to 0.8 when constructing the ensemble.  Vote 7 if 50% or more of 
# those models are predicting a 7, and 2 otherwise.

#what is the accuracy of the ensemble now?

acc_8 <- acc_hat >= 0.8
vote <- rowMeans(pred[,acc_8] == "7")
y_hat <- ifelse(vote >= 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
