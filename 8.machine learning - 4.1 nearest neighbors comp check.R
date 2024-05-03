#8.machine learning - 4.1 nearest neighbors comp check

#Q1
library(caret)
library(dslabs)
data(heights)
set.seed(1)

#partition heights data into train and test sets

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks <- seq(1,101,3)
F_1 <- sapply(ks, function(k){
  knn_fit <- knn3(sex ~ height, data = train_set, k=k) #specify formula (y~X...) and dataframe, default k = 5
  y_hat_knn <- predict(knn_fit, test_set, type = "class")  #determine probability for each class
  F_meas(data = y_hat_knn, reference = test_set$sex)
})

plot(ks, F_1) # plot k vs F-1 score
max(F_1) #what is the max F_1 score?
ks[which.max(F_1)] #at what value of k does it occur?

#Q2
data("tissue_gene_expression")

set.seed(1)
#split data into train and test sets
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# train set = -test_index
# test set = test_index

#create model
ks <- seq(1, 11, 2)
sapply(ks, function(k){
fit <- knn3(x[-test_index,], y[-test_index], k = k) # create model
y_hat <- predict(fit, newdata = x[test_index,], type = "class") # predict
mean(y_hat == y[test_index])  # determine the accuracy
})


