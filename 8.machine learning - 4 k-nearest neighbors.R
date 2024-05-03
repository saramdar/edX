#8.machine learning - 4 k-nearest neighbors

#load data and show plot of predictors with outcome represented by color using test data
library(tidyverse)
library(dslabs)
data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) + geom_point()

#use k-nearest neighbor methodology to estimate the probability of 2 or 7
library(caret)
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#probability of number being a 7
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") %>% factor(levels = levels(test_set$sex))

#view accuracy using a confusion matrix
confusionMatrix(y_hat_knn, mnist_27$test$y)

confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

#use linear regression to generate an estimate for the same data set
#train data
fit_lm <-mnist_27$train %>%
  mutate(y=ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)
#make predictions based on training
p_hat_lm <- predict(fit_lm, mnist_27$test)
#using predictions, classify as 7 or 2
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
#view resuults
confusionMatrix(y_hat_lm, mnist_27$test$y)

#why does knn beat regression?
#plot estimates of p_hat compared to true conditional probability p
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
#the areas of blue within the kNN estimate happens due to overtraining. 
#View predictions based on train vs test data below:

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)

#overtrainng and oversmoothing
#fit a knn model with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)
#check test set accuracy
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)

#view the overtraining problem by plotting the estimates
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

#try k=401
knn_fit_401 <- knn3(y~., data = mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)

#compare graphs of regression model vs k=401 model
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)
