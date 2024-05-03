#8.machine learning - 6.3 Regularization

#view top 10 largest prediction "mistakes"
test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, rating, residual) %>% slice(1:10)
#obscure movies, low ratings

#top 10 worst and best movies based on movie effect
movie_titles <- movielens %>%
  dplyr::select(movieId, title) %>%
  distinct()
head(movie_titles)

#ten best
movie_avgs %>% left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  dplyr::select(title, b_i) %>%
  slice(1:10)

#ten worst
movie_avgs %>% left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  dplyr::select(title, b_i) %>%
  slice(1:10)

#look at how often these movies are rated inour data set
#ten best
train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10)

#ten worst
train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  slice(1:10)

#use regularization to estimate the movie effect
#lambda is a tuning parameter, the larger it is, the more the effect is shrunk
#can select lambda using cross validation
lambda <- 3 #tuning parameter, the larger it is, the more we shrink
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
movie_reg_avgs

#view plot of regularized estimates vs least squares estimates (orginal)
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#use penalized estimate of b_i (including lambda) to view top 10 best and worst
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(title, b_i, n) %>% 
  slice(1:10)

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  dplyr::select(title, b_i, n) %>% 
  slice(1:10)

#predict results
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
#evaluate model
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)

#view all results of all models in a table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#use cross validation to find best lambda
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

#use regularization to estimate the user effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()
