#3.Prob 1.3 addition rule and Monty Hall

#1 - what is the probability that the team will with the series?
n <- 6 #number of remaining games in series
outcomes <- c(0,1) #possible outcomes
l <- rep(list(outcomes), n) #list of all possible outcomes in remaining games
possibilities <- expand.grid(l) #data frame that contains all combinations of possible outcomes
results <- rowSums(possibilities) > 3 #vector that indicates whether each row in the data frame contains enough wins to win the series
mean(results) #proportion the team wins the series


#2 Monte Carlo of above problem
B <- 10000 #number of times we want to run the simulation
results <- replicate(B, { #replicate a simulated series for B iterations and determine if that series has enough wins to win the series
  simulated_series <- sample(c(0,1), 6, replace = TRUE)
  sum(simulated_series) > 3
})
mean(results) #proportion the team wins the series

#3 Teams A and B play a series
p <- seq(0.5, 0.95, 0.025) #probability that team A will win
prob_win <- function(p){ #given p, the probability of winning the series for Team B
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
  sum(b_win) >=4
  })
mean(result)}
Pr <- sapply(p, prob_win) #apply prob_win across vector of probabilities that A will win to determinw the probability that B will win
plot(p, Pr)

#repeat exercise above but keep probability for Team A to win = 0.75 and compute for different series lengths
prob_win <- function(N, p=0.75){ #probability of team B winning the series
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>= (N+1)/2
  })
mean(result)
}
N <- seq(1, 25, 2) #vector for series length.  options are 1-25 gaes, odd numbers only
Pr <- sapply(N, prob_win) # probability that team B will win across vector of series length
plot(N, Pr) #plot number of games vs Pr team B will win

#monty hall - stick to original door pick
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

#monty hall - switch from original door pick
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch)
