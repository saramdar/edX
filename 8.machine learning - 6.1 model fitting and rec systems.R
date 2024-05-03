#8.machine learning - 6.1 model fitting and rec systems

#preprocessing

library(tidyverse)
library(dslabs)
mnist <- read_mnist()

names(mnist) #components of data set
dim(mnist$train$images) # each component includes maxtrix with feathers in columns

class(mnist$train$labels) #class - integers
table(mnist$train$labels) # vector

# sample 10k rows from training set, 1k rows from test set
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#view variability of features
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

#remove features whose variance is near zero
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28)) #visual of columns recommended for removal

col_index <- setdiff(1:ncol(x), nzv)
length(col_index) # number of features we are keeping

#fit model
#add column names to features, required by caret 
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

#kNN model
#step 1 - optimize for k
#code below will take a long time:
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#instead, test a smaller subset of data using this code:
n <- 1000 #adjust to establish how it affects computing time
b <- 2 #adjust to establish how it affects computing time
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
#view optimum value of k
ggplot(train_knn)

#use optimum value of k to train algorithm (instead of trying all values of k)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

# view specificity and sensitivy by number
cm$byClass[,1:2] #8 is hardest to detect (lowest sensitivity), 7 is most common to be incorrectly predicted (lowest specificity)

#random forest algorithm
library(randomForest)
control <- trainControl(method = "cv", number = 5) 
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <- train(x[,col_index], y, 
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)

#fit model
fit_rf <- randomForest(x[,col_index], y, mtry = train_rf$bestTune$mtry)

#check if enough trees were run:
plot(fit_rf)

#view accuracy
y_hat_rf <- predict(fit_rf, x_test[,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall

#Variable importance
imp <- importance(fit_rf)
imp

#plot image
mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(imp, 28, 28))


p_max <- predict(fit_rf, x_test[,col_index], type = "prob") 
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)

ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

rafalib::mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#Ensembles
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
