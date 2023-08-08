#generate a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number= numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

3/8

#probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

#chance that 2 people out of 50 have same birthday
n <- 50
bdays <- sample(1:365, n, replace = TRUE)
any(duplicated(bdays))

B <- 10000
bday_results <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})

compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n<-seq(1:60)

#compute exact probability that 2 people have the same birthday
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
1-prod(prob_unique)
}
eprob <-sapply(n, exact_prob)

#estimate a practical value of B
B<- 10^seq(1, 5, len = 100) #defines vector of many B values
compute_prob <- function(B, n=22){ # function to run Monte Carlo simulation with each B
  same_day <-replicate(B, {
    bdays <-sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
mean(same_day)
}
prob <- sapply(B, compute_prob) #apply compute_prob to many values of B
plot(log10(B), prob, type = "l")


#Monte Carlo simulation for Celtics winning a game
B <- 10000
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games %in% "win")
})
mean(celtic_wins)
