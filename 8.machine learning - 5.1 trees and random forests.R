#8.machine learning - 5.1 trees and random forests
library(dslabs)
data("polls_2008")
head(polls_2008)
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin~., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex=0.75)

#final estimate
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#set cp=0 and minsplit = 2
fit <- rpart(margin~., data = polls_2008,
             control = rpart.control(cp=0, minsplit = 2))
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

plot(fit)

#use cross validation to choose cp
library(caret)
train_rpart <- train(margin~., 
                     method = "rpart",
                     tuneGrid = data.frame(cp=seq(0,0.05,len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

#view the resulting tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#because we only have 1 predictor, can plot f_hat(x)
polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

pruned_fit <- prune(fit, cp=0.01)

pruned_fit

plot(pruned_fit)
text(pruned_fit)

#CLassification trees for categorical outcomes
train_rpart <- train(y~., 
                     method = "rpart",
                     tuneGrid = data.frame(cp=seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart) #accuracy ~ 0.805 @ cp = 0.1

#kernal method = better accuracy
y_hat <- predict(train_rpart, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)

#random forest
library(randomForest)
fit <- randomForest(margin~., data = polls_2008)
rafalib::mypar()
plot(fit)

#resulting estimate for this random forest
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)

#change the parameter that controls the minimum number of data points in the nodes of the tree
#optimize accuracy vs node size
nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(y~., method = "rf", data = mnist_27$train,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})

qplot(nodesize, acc)

nodesize[which.max(acc)]

#fit the random forest with the optimized minimum nodesize to the training data and eval performance vs test data
train_rf_2 <- randomForest(y~., data=mnist_27$train, 
                           nodesize = nodesize[which.max(acc)])
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)

# use cross validation to choose parameter
train_rf_3 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_3, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
