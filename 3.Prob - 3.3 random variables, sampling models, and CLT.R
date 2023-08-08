#3.3 Random Variables, sampling models, and Central Limit Thereom
#SAT testing
#-.025 penalty for incorrect answer, 1 point for correct answer, 44 questions with 5 answer choices

#1a What is the probabilty of guessing correcting for one question
p <- 1/5 # one correct choice of 5 options
p

#1b what is the expected value of points for guessing on one question?
a <- 1
b <- -.25
mu <- a*p+b*(1-p)
mu

#what is the expected score of guessing on all 44 questions?
n <- 44
n*mu

#what is the standard error of guessing on all 44 questions?
sigma <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma

#use CLT to determine the probability that a guessing student scores 8 points or higher on the test
1-pnorm(8, mu, sigma)

#run a monte carlo sim of 10,000 students guessing on the test.
set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44
p <- 0.2
tests <- replicate(B,{
  X <- sample(c(1, -.25), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
mean(tests >=8)

#2 SAT was changed to eliminate penalty for guessing and multiple choice options reduced to 4
#what ist he expected value of the score when guessing on the new test?
p <- 1/4
a <- 1
b <- 0
mu <- a*p+b*(1-p)
mu * n

#consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a 
#range of student skills.
#what is the lowest p such that the probabilty of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n*a*x+b*(1-x)
  sigma <- sqrt(n)*abs(b-a)*sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)})
min(p[which(exp_val > 0.8)])

#betting on roulette
#house special bet is a win if ball lands 5 pockets out of a total of 38
p <- 5/38
# bet pays out 6 to 1, meaning a losing bet yields $-1 and a successful bet yields $6
a <- 6
b <- -1
#what is the expected value of the payout for one bet?
 mu <- a*p + b*(1-p)
 mu
#what is the standard error of the payout for one bet?
 sigma <- abs(b-a)*sqrt(p*(1-p))
 sigma
#what is the expected balue of the average payout over 500 bets?
#there is a difference between expected balue of the average and expected value of the sum
 mu
#what is the standard error of the average payout over 500 bets?
 n <- 500
 sigma/sqrt(n)
#what is the expected vale of the sum of 500 bets?
 n*mu
 #standard error of 500 bets?
 sqrt(n)*sigma
 #whats the probability of losing money over 500 bets
 pnorm(0, n*mu, sigma*sqrt(n))
 