#8.machine learning - 5.1 caret comp check

#Q1 Load the rpart package and then use the caret::train() function with method = "rpart" 
#to fit a classification tree to the tissue_gene_expression dataset. Try out cp values of 
#seq(0, 0.1, 0.01). Plot the accuracies to report the results of the best model. Set the seed to 1991.

#Which value of cp gives the highest accuracy?
library(caret)
library(dslabs)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
dat <- data.frame(x,y)
set.seed(1991)
fit <- train(y~., 
             method = "rpart",
             data = dat,
             tuneGrid = data.frame(cp=seq(0, 0.1, 0.01 )))


ggplot(fit)
confusionMatrix(fit)

#Q2 Note that there are only 6 placentas in the dataset. By default, rpart requires
#20 observations before splitting a node. That means that it is difficult to have a 
#node in which placentas are the majority. Rerun the analysis you did in Q1 with 
#caret::train(), but this time with method = "rpart" and allow it to split any node 
#by using the argument control = rpart.control(minsplit = 0). Look at the confusion
#matrix again to determine whether the accuracy increases. Again, set the seed to 1991.

dat %>% count(y)

set.seed(1991)
fit_rpart <- train(y~., 
             method = "rpart",
             data = dat,
             tuneGrid = data.frame(cp=seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0))


ggplot(fit_rpart)
confusionMatrix(fit_rpart)

#Q3 plot the tree from the best fitting model of the analysis in Q2
#which gene is at the first split?

plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel)

#Q4 We can see that with just seven genes, we are able to predict the tissue type.
# Now let's see if we can predict the tissue type with even fewer genes using a 
# Random Forest. Use the train() function and the rf method to train a Random Forest 
# model and save it to an object called fit. Try out values of mtry ranging from 
#seq(50, 200, 25) (you can also explore other values on your own). 
#What mtry value maximizes accuracy? To permit small nodesize to grow as we did with 
#the classification trees, use the following argument: nodesize = 1.

#Note: This exercise will take some time to run. If you want to test out your code 
#first, try using smaller values with ntree. Set the seed to 1991 again.

#What value of mtry maximizes accuracy?

set.seed(1991)  
fit <- train(y~., 
             method = "rf",
             data = dat,
             nodesize = 1,
             tuneGrid = data.frame(mtry=seq(50, 200, 25)))


ggplot(fit)


