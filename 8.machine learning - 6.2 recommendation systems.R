#8.machine learning - 6.2 recommendation systems

library(dslabs)
library(tidyverse)

data("movielens")

#view data in tidy format
movielens %>% as_tibble() 

#view number of distinct users that provided ratings and distinct movies that were rated
movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))

#identify top 5 movieIDs with most ratings
keep <- movielens %>%
     dplyr::count(movieId) %>%
     top_n(5) %>%
     pull(movieId)
keep
#view ratings for top 5 movies for a group of users
tab <- movielens %>%
     filter(userId %in% c(13:20)) %>% 
     filter(movieId %in% keep) %>% 
     dplyr::select(userId, title, rating) %>% 
     pivot_wider(names_from="title", values_from="rating")
tab
#make into a nicer looking table
tab %>% knitr::kable()

#take subset of data - sample 100 users randomly
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()

#create matrix for random sample of 100 users and 100 movies to visually see user/movie combinations with ratings
movielens %>% filter(userId %in% users) %>% 
dplyr::select(userId, movieId, rating) %>%
mutate(rating = 1) %>%
pivot_wider(names_from = movieId, values_from = rating) %>% 
(\(mat) mat[, sample(ncol(mat), 100)])()%>%
as.matrix() %>% 
t() %>%
image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

#distribution of movies/number of ratings and users/number of ratings
library(gridExtra)
p1 <- movielens %>% 
     dplyr::count(movieId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "black") + 
     scale_x_log10() + 
     ggtitle("Movies")

p2 <- movielens %>% 
     dplyr::count(userId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "black") + 
     scale_x_log10() + 
     ggtitle("Users")
grid.arrange(p1, p2, ncol = 2)
#observe that some movies and some users have more ratings

#create a train and test set for predicting movie rating
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don't include users and movies in the test set that do not appear in the training set, 
#we removed these using the semi_join function, using this simple code.
test_set <- test_set %>% 
     semi_join(train_set, by = "movieId") %>%
     semi_join(train_set, by = "userId")

#equation for RMSE - use to compare different model performance to the test set
RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System
# A first model
mu_hat <- mean(train_set$rating) # average of all ratings in the train set
mu_hat

#predict unknown ratings using RMSE function from above, test_set$ratings for true ratings, and mu_hat for predicted ratings
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#try with fake rating
predictions <- rep(2.5, nrow(test_set)) #create set of fake 2.5 ratings
RMSE(test_set$rating, predictions) #view RMSE - it's higher

#results of first model
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results
# Modeling movie effects
#use least squares to estimate the bias of different movies being rated differently
#fit <- lm(rating ~ as.factor(userId), data = movielens)  ## this code may take a long time to run due to thousands 
#of bs, as each movie gets a b
mu <- mean(train_set$rating)   #determine average rating
movie_avgs <- train_set %>%   #determine difference from average
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))

#plot difference from average
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))
#large variation, need to add bias effect

#use bias in prediction
predicted_ratings <- mu + test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
predicted_ratings

#predict unknown ratings using RMSE function from above, test_set$ratings for true ratings, and predicted_ratings for 
#predicted ratings
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
model_1_rmse
#view in table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# Modeling user effects
#use following code to compute the average rating for user u for those that have rated 100 or more movies and plot histogram
train_set %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     filter(n()>=100) %>%
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 30, color = "black")
#large variation, need to add user specific effect

#fit <- lm(rating ~ as.factor(movieId) + as.factor(userId), data = movielens) ## this code may take a long time to run due to thousands of movies and users
#instead, compute an approximation by computing u and bi and estimating bu and the average of yui-u-bi
user_avgs <- train_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))

#create predictors and see how much the RMSE improves
predicted_ratings <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse

#view RMSEs in a table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

