#8.machine learning - 6.3 Regularization comp check

options(digits=7)
#simulate the number of students in 1000 schools:
set.seed(1986)
n <-round(2^rnorm(1000, 8, 1))
#assign true quality for each school independent of size (this is the parameter we will estimate in our analysis)
set.seed(1)
mu <- round(80+2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS", 1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#view top 10 schools using this code:
schools %>%
  top_n(10, quality) %>%
  arrange(desc(quality))

#simlulate students test scores
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})

schools <- schools %>% mutate(score = sapply(scores, mean))
schools

#Q1 what are the top schools based on the average score?  Show just the ID, size, and average score.
# Report the ID of the top school and average score of the 10th school.

schools %>% arrange(desc(score)) %>% top_n(10) %>% select(id, size, score)

#Q2 compare the median school size to the median school size of the top 10 schools based on the score

#what is the median size overall?
median(schools$size)

#what is the median school size of the top 10 schools based on the score?
top <- schools %>% top_n(10, score)
median(top$size)
#OR
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3 according to this analysis, it appears that small schools produce better test scores than large schools.
#we constructed the sim so that quality and size were independent.
#Repeat the exercise for hte bottom 10 schools based on score.
#what is the median school size of the bottom 10 schools based on the score?
schools %>% arrange((score)) %>% slice(1:10) %>% .$size %>% median()
#OR
schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4 
#plot average score vs school size
schools %>% ggplot(aes(size, score)) + geom_point(alpha = 0.5) + geom_point(data = filter(schools, rank <=10), col = "purple")

#Q5 NOW WE WILL USE REGULARIZATION TO PICK THE BEST SCHOOLS
#define overall average for all schools using the following code:
overall <- mean(sapply(scores, mean))
overall
#define, for each school, how it's score deviates from the average. use alpha = 25
alpha <- 25 #tuning parameter
score_reg <- sapply(scores, function(x){
  overall + (sum(x-overall)/(length(x)+alpha)) #(sum(actual score - overall average)/(number of scores + alpha)
})
#what is the ID of the top school with regularization? 
#what is the regularized score of the 10th school? 
schools %>% mutate(score_reg = score_reg) %>% top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6 find the value of alpha that minimizes RMSE *NOTE both alpha and school scores are variable.  Needs 2 functions
alphas <- seq(10, 250, 1)

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x){
    overall+sum(x-overall)/(length(x)+alpha)
  })
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmses)  
alphas[which.min(rmses)]

#Q7 use alpha from Q6 to determine new avg scores. rank them by top score.
alpha_min <- 135
score_reg_min <- sapply(scores, function(x){
  overall + sum(x-overall)/(length(x)+alpha_min)
})

schools %>% mutate(score_reg_min = score_reg_min) %>% top_n(10, score_reg_min) %>% arrange(desc(score_reg_min))

#Q8 a common mistake made when using regularization is shrinking values towards 0 that are not centered around 0. 
#If we don't subtract the overall overage before shrinking, we actually obtain an very similiar result. Confirm this
#by rerunning the code from Q6 but without removing the overall mean
#What value of alpha gives the minimum RMSE in this case?
alphas <- seq(10, 250, 1)

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x){
    sum(x)/(length(x)+alpha)
  })
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmses)  
alphas[which.min(rmses)]
