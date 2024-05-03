#8.machine learning - 5.2 titanic exercises
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Q1 set the seed to 42, then use the caret package to create a 20% data partition based 
# on the survived column.  Assign the 20% partition to test_set and the remaining 80% to 
# train_ set

set.seed(42)
y <- titanic_clean$Survived
test_index <- createDataPartition(y, times = 1, p = .20, list = FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]
#how many observations are in the training set?
nrow(train_set)
#how many observations are in the test set?
nrow(test_set)
#what proportion of individuals in the training set survived?
mean(train_set$Survived == "1")

#Q2 The simplest prediction method is randomly guessing the outcome without using 
# additional predictors. These methods will help us determine whether our machine 
# learning algorithm performs better than chance. How accurate are two methods of 
# guessing Titanic passenger survival?
#Set the seed to 3. For each individual in the test set, randomly guess whether 
#that person survived or not by sampling from the vector c(0,1) (Note: use the 
#default argument setting of prob from the sample function).

#What is the accuracy of this guessing method?
set.seed(3)
guess <- sample(c("0","1"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$Survived))
mean(guess == test_set$Survived)
#OR
set.seed(3)
# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

#Q3a Use the training set to determine whether members of a given sex were more 
#likely to survive or die.

#What proportion of training set females survived?
#What proportion of training set males survived?

train_set %>% group_by(Sex) %>% summarize(mean(Survived == "1"))

#Q3b Predict survival using sex on the test set: if the survival rate for a sex 
#is over 0.5, predict survival for all individuals of that sex, and predict 
#death if the survival rate for a sex is under 0.5.

sex_model <- ifelse(test_set$Sex == "female", "1", "0") %>%
  factor(levels = levels(test_set$Survived))

#What is the accuracy of this sex-based prediction method on the test set?
mean(test_set$Survived == sex_model)

#Q4a In the training set, which class(es) (Pclass) were passengers more likely to
#survive than die? Note that "more likely to survive than die" (probability > 50%)
#is distinct from "equally likely to survive or die" (probability = 50%).
#Select ALL that apply.

table(train_set$Pclass, train_set$Survived) 
  
#OR
train_set %>% group_by(Pclass) %>% summarize(survived = mean(Survived == 1))

#Q4b Predict survival using passenger class on the test set: predict survival
# if the survival rate for a class is over 0.5, otherwise predict death.

class_model <- ifelse(test_set$Pclass == "1", "1", "0") %>%
  factor(levels = levels(test_set$Survived))

#What is the accuracy of this sex-based prediction method on the test set?
mean(test_set$Survived == class_model )

#Q4c Use the training set to group passengers by both sex and passenger class
#which sex and class combinations were more likely to survive than die? (i.e. >50% survival?)

train_set %>% group_by(Sex, Pclass) %>% summarise(mean(Survived == 1))

#Q4d Predict survival using both sex and passenger class on the test set.  Predict
# survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass == 1 | test_set$Sex == "female" & test_set$Pclass == 2, 1, 0) 
#what is the accuracy of this sex and class based prediction method on the test set?
mean(test_set$Survived == sex_class_model)
#OR - a little simplier!
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0) %>%
  factor(levels = levels(test_set$Survived))

mean(sex_class_model == test_set$Survived)

#Q5 Use the confusionmatrix() function to create confusion matrices for the sex model,
# class model, and combined sex/class model.  You will need to concert predictions and survival statuses
# to factors to use this function.

confusionMatrix(sex_model, test_set$Survived)
confusionMatrix(class_model, test_set$Survived)
confusionMatrix(sex_class_model, test_set$Survived)
#OR
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived)) #do for all models

#Q6 use the F_meas() function to calculate F1 scores for the sex model, class model, and combined sex/class model.
#You will need to convert predictions to factors to use this function
#which model has the highest F1 score?
F_meas(data = sex_model, reference = factor(test_set$Survived))
F_meas(data = class_model, reference = factor(test_set$Survived))
F_meas(data = sex_class_model, reference = factor(test_set$Survived))

#Q7 set the seed to 1.  Train a model using loess with the caret gamLoess method using fare as the only predictor
#Note: when training models for this section, please use the S3 method for class formula rather than the default S3 method of caret train():
# S3 method for class 'formula'
#train(form, data, ..., weights, subset, na.action = na.fail, contrasts = NULL)
set.seed(1)
train_gamloess <-train(Survived ~ Fare, method = "gamLoess", data = train_set)

#what is the accuracy on the test set for the loess model?  
predict_gamloess <- predict(train_gamloess, test_set)
confusionMatrix(predict_gamloess, test_set$Survived)

#OR
set.seed(1) 
train_loess <- train(Survived ~ Fare, method = "gamLoess", data = train_set)
loess_preds <- predict(train_loess, test_set)
mean(loess_preds == test_set$Survived)

#Q8
#Set the seed to 1. Train a logistic regression model with the caret glm method using age as the only predictor.
#What is the accuracy of your model (using age as the only predictor) on the test set ?
set.seed(1)
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
loess_glm <- predict(train_glm, test_set)
mean(loess_glm == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the caret glm method using four predictors: sex, class, fare, and age.
#What is the accuracy of your model (using these four predictors) on the test set?
set.seed(1)
train_glm <- train(Survived ~ Age + Fare + Pclass + Sex, method = "glm", data = train_set)
loess_glm <- predict(train_glm, test_set)
mean(loess_glm == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the caret glm method using all predictors.
#What is the accuracy of your model (using these four predictors) on the test set?
set.seed(1)
train_glm <- train(Survived ~ ., method = "glm", data = train_set)
loess_glm <- predict(train_glm, test_set)
mean(loess_glm == test_set$Survived)

#Q9a Set the seed to 6.  Train a kNN model on the training set using the caret train().  Try tuning with x=seq(3,51,2)
set.seed(6)
train_knn <- train(Survived ~ ., 
                   method = "knn",
                   data = train_set, 
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

#Q9b Plot the kNN model to investigate the relationship between the number of neighbors and accuracy on the training set.
ggplot(train_knn)

#Q9c What is the accuracy of the kNN model on the test set?
y_hat_knn <- predict(train_knn, test_set)
confusionMatrix(y_hat_knn, test_set$Survived)

#OR
mean(y_hat_knn == test_set$Survived)

#Q10 set the seed to 8 and train a new kNN model.  Instead of the default training control, use 10-fold cross-validation 
#where each partition consists of 10% of the total. Try tuning with k = seq(3, 51, 2).

set.seed(8)
control <-trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Survived ~., 
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k=seq(3,51,2)),
                      trControl = control)

ggplot(train_knn_cv, highlight = TRUE)

#What is the optimal value of k using cross-validation?
train_knn_cv$bestTune

#what is the accuracy on the test set using the cross validated kNN model?
mean(predict(train_knn_cv, test_set) == test_set$Survived)

#Q11a set the seed to 10.  Use caret to train a decision tree with the rpart method.
#tune the complexity parameter with cp = seq(0, 0.05, 0.02)
set.seed(10)
train_rpart <- train(Survived ~., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
ggplot(train_rpart)

#what is the optimal value of the complexity parameter?
train_rpart$bestTune

#what is the accuracy of the decision tree model on the test set?
mean(predict(train_rpart, test_set) == test_set$Survived)

#Q11b Inspect the final model and decision tree.
#Which variables are used in the decision tree?
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)
#read the tree, ask the question, if yes go down to the left, if no go to the right.

#Q12 set the seed to 14.  Use the caret train() function with the rf method to train an random forest.
#test values of mtry = seq(1:7).  set ntree to 100.
set.seed(14)
train_rf <- train(Survived~., 
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(1:7)),
                  data = train_set)
ggplot(train_rf)

#what mtry value maximizes accuracy?
train_rf$bestTune

#What is hte accuracy of the random forest model on the test set?
mean(predict(train_rf, test_set) == test_set$Survived)
