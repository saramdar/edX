<<<<<<< HEAD
#8.machine learning - 5.1 caret and loess packages
=======
#8.machine learning - caret and loess packages
>>>>>>> ca00004279b08a0e20d3bb04e96f1757bfb5bdde
#caret
library(tidyverse)
library(dslabs)
data("mnist_27")

#train() lets us train algorithms
library(caret)
train_glm <- train(y~., method = "glm", data = mnist_27$train)
train_knn <- train(y~., method = "knn", data = mnist_27$train)

#predict.train() lets us predict
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

#compare accuracy of algorithms
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#get information on models
getModelInfo("knn") #find out what parameters are optimized
modelLookup("knn") #quick lookup

#view result of cross validation using ggplot
#highlight = TRUE highlights the max
ggplot(train_knn, highlight = TRUE)

#use tune grid parameter to change k default parameters
set.seed(2008)
train_knn <- train(y~., method = "knn",
                   data = mnist_27$train, 
                   tuneGrid = data.frame(k=seq(9, 71, 2)))

ggplot(train_knn, highlight = TRUE)

#access parameter that maximizes the accuracy
train_knn$bestTune

#access best performing model
train_knn$finalModel

#predict() will use the best performing model
#accuracy for the best model when applied to the test set
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall[["Accuracy"]]

#change how we perform cross validation using trainControl() with 10 fold cross validation.  
control <- trainControl(method ="cv", number = 10, p=.9) #cross validation with 10 samples using 10% of the observations each
train_knn_cv <-train(y~., method = "knn",
                     data = mnist_27$train, 
                     tuneGrid = data.frame(k=seq(9, 71, 2)), 
                     trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
#results component of the train output contains several summary statistics related to the estimate
names(train_knn$results)

#view best fitting knn model
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

#loess
modelLookup("gamLoess")
# to try different values of span we need a column in the dable with the name degree.  define grid like this:
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1) 
#use default cross validation control parameters
train_loess <- train(y~., 
                     method = "gamLoess",
                     tuneGrid = grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)                    
confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]
#view estimate of conditional probability
p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1
