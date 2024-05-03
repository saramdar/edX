#8.machine learning - k nearest neighbors

# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Section 4: Cross-validation and k-Nearest Neighbors

## Section 4.1 Nearest Neighbors

### Video: Cross-validation

library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

#knn estimate
knn_fit <- knn3(y ~ ., data = mnist_27$train) #specify formula (y~X...) and dataframe, default k = 5
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  #determine probability for each class
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"] # view accuracy

#linear regression estimate
fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

#plot estimates vs true conditional probability
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

#view accuracy of train set vs test set = overtraining in train set
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

### Video: Overtraining and Oversmoothing

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall["Accuracy"]  #accuracy for training set

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]  #accuracy for test set

#plot estimates vs train and test data
p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

#use much larger value of k
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

#plot
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)
