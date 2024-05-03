#8.machine learning - cross validation comp check
#Q1
#generate a set of random predictors using the following code:
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
x_subset

#Because x and y are completely independent, you should not be able to 
#predict y using x with accuracy much greater than 0.5. Confirm this by 
#setting the seed to 1 and running cross-validation using logistic 
#regression to fit the model. Because we have so many predictors, we 
#selected a random sample x_subset. Use the subset when training the model.

#Which code correctly performs this cross-validation?

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

#Q2 Now, instead of using a random selection of predictors, we are going to
#search for those that are most predictive of the outcome. We can do this by 
#comparing the values for the  group to those in the  group, for each 
#predictor, using a t-test. We can perform this step like this:

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}
#Create an index ind with the column numbers of the predictors that were 
#"statistically significantly" associated with y. Use a p-value cutoff 
#of 0.01 to define "statistically significantly."How many predictors survive 
#this cutoff?

ind <- data.frame(pvals) %>% filter(pvals <= 0.01) %>% nrow()
ind

#OR
ind <- which(pvals<=0.01)
length(ind)

#Q3 Now set the seed to 1 and re-run the cross-validation after redefinining
#x_subset to be the subset of x defined by the columns showing "statistically
#significant" association with y. What is the accuracy now?

x_subset <- x[,ind]
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

#Q3 Set the seed to 1 and re-run the cross-validation again, but this time using
#kNN. Try out the following grid k = seq(101, 301, 25) of tuning parameters. Make
#a plot of the resulting accuracies.

set.seed(1)
fit <- train(x_subset, y, method = "knn",
                   tuneGrid = data.frame(k=seq(101, 301, 25)))

ggplot(fit, highlight = TRUE)
plot(fit)

