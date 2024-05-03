#8.machine learning - breast cancer project
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#Q1 dimensions and properties of the dataset
#how many samples are in the data set?  How many predictors are in the matrix?
head(brca)
str(brca)
#OR
dim(brca$x)
dim(brca$x)[1]
dim(brca$x)[2]

#what proportion of the samples are malignant?
mal <- brca$y == "M"
mean(mal)
#OR
mean(brca$y=="M")

#what column has the highest mean?
colMeans(brca$x) %>% which.max()

#what column has the lowest standard deviation?
colSds(brca$x) %>% which.min()

#Q2 Use sweep() two times to scale each column: subtract the column mean of brca$x, 
#then divide by the column standard deviations of brca$x
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
#after scaling, what is the standard deviation of the first column?
sd(x_scaled[,1])
#after scaling, what is the median value of the first column?
median(x_scaled[,1])

#Q3 perform a principal component analysis of the scaled matrix. 
pca <- prcomp(x_scaled)
#what proportion is explained by the first principal component?
#how many principal components are required to explain at least 90% of the variance?
summary(pca)$importance[,1:10]

#Q4 plot the first two principal components with color representing tumor type (benign/malignant)
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], type = (brca$y)) %>%
  ggplot(aes(PC1, PC2, color = type)) + geom_point()

#Q5 Make a boxplot of the first 10 PCs grouped by tumor type. Which PCs are significantly 
#different enough by tumor type that there is no overlap in the interquartile ranges
#(IQRs) for benign and malignant samples?

pc_scores <- pca$x[,1:10]
type <-brca$y

#combine into one dataframe
pc_data <- data.frame(type, pc_scores)
#reshape dataframe
pc_data_long <- reshape2::melt(pc_data, id.vars = "type", variable.name = "Principal Component")
#boxplot
ggplot(pc_data_long, aes(x = `Principal Component`, y = value, fill = type)) + geom_boxplot()


#OR
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

#PART 3
#create a data partition splitting brca$y and the scaled version of the brca$x matrix
#into a 20% test set and 80% train set using the following code:

set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#Q6 check that the training and test sets have similiar proportions of begign and malignant tumors
#what proportion of the training set is benign?
prop.table(table(train_y))
#OR
mean(train_y == "B")
#what proportion of the test set is benign?
prop.table(table(test_y))
#OR
mean(test_y == "B")

#Q7 fit a logistic regression model on the training set using all predictors.
#Make predictions on the test set.
#What is the accuracy of the logistic regression model on the test set?

set.seed(1) 
train_glm <- train(train_x, train_y, method = "glm") #train
glm_preds <- predict(train_glm, test_x) #predict
glm_results <- mean(glm_preds == test_y) #measure accuracy
glm_res <- data.frame(method = "glm", accuracy = glm_results)
glm_res

#Q8 fit a loess model on the training set. Use default tuning grid.
#generate predictions on the test set.
#What is the accuracy of the loess model on the test set?

set.seed(5) 
train_loess <- train(train_x, train_y, method = "gamLoess") #train
loess_preds <- predict(train_loess, test_x) #predict
loess_results <- mean(loess_preds == test_y) #measure accuracy
loess_res <- data.frame(method = "loess", accuracy = loess_results)
loess_res

#Q9 train a k-nearest neighbors model on the training set.
# Try odd values of K from 3 to 21. 
#what is the final value of K used in the model?
#what is the accuracy of the knn model on the test set?

set.seed(7)
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn$bestTune

knn_preds <- predict(train_knn, test_x)
knn_results <- mean(knn_preds == test_y)
knn_res <- data.frame(method = "knn", accuracy = knn_results)
knn_res

#Q10a
#train a random forest model ont he training set.
#test mtry values of 3, 5, 7, 9
#use argument importance = TRUE so that feature importance can be extracted.
#generate predictions on the test test.
set.seed(9)
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry = c(3, 5, 7, 9)), importance = TRUE)
train_rf$bestTune

rf_preds <- predict(train_rf, test_x)
rf_results <- mean(rf_preds == test_y)
rf_res <- data.frame(method = "rf", accuracy = rf_results)
rf_res

train_rf$importance #look for the variable with the highest mean decrease in accuracy - this is considered the most important variable
#OR
varImp(train_rf)

#Q10b COnsider the top 10 most important variables in the random forest model.
#which set of features os the most important for determining tumor type?

varImp(train_rf)
#6 of top 10 are "worst" values, including top 4

#Q11a Create an ensemble using the predictions from the 4 models created in the previous exercises.
# generate a majority prediction of the tumor type - if more than 50% of the models suggest 
# the tumor is malignant, predict malignant.
# What is the accuracy of the ensemble prediction?

ensemble_preds <- data.frame(glm_preds, loess_preds, knn_preds, rf_preds) #combine predictions into 1 dataframe

vote <- rowMeans(ensemble_preds == "M") # create a mean score for each row based on # of M vs total
vote_preds <- ifelse(vote > .50, "M", "B") #if mean is over .50, chose M, otherwise B
ens_results <- mean(vote_preds == test_y) #compare prediction to test data
ens_res <- data.frame(method = "ensemble", accuracy = ens_results)
ens_res

#11b Make a table of the accuracies of the 4 models and the accuracy of the ensemble
#which model has the highest accuracy?


all_results <- bind_rows(glm_res, loess_res, knn_res, rf_res, ens_res)
all_results %>% knitr::kable()
#review accuracies, knn has highest.