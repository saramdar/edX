#3Prob - 4.2 The Big short Insurance
#one year term life insurance policy pays $150,000 in the event of death within one one
#premium (assunal cost) is $1150 for 50 year old female. 
#company plans to 1,000 policies to this demographic

library(dslabs)
data("death_prob") #estimated probability of death within 1 year for different ages and sexes
head(death_prob)

#what is the death probability of a 50 year old female
p <- death_prob %>%
  filter(sex == "Female" & age == "50") %>%
  pull(prob)
p

#what is the expected value of the companys net prfit on one policy for a 50 year old female?
a <- -150000
b <- 1150
mu <- a*p+b*(1-p)
mu

#standard error on one policy for 50 yr old female?
sigma <- abs(b-a)*sqrt(p*(1-p))
sigma

#what is the expected value of the companys net profit on 1000 policies for 50 year old females?
n <- 1000
n*mu

#standard error of the sum of the expected value on 1000 policies for 50 yr old females?
sigma * sqrt(n)
t
#use CLT to calculate the probability that the insurance company loses money on this set of 1000 policies
pnorm(0,n*mu, sigma*sqrt(n))

#determine probabilty of death for 50 year old male
p_male <- death_prob %>%
  filter(sex == "Male" & age == "50") %>%
  pull(prob)
p_male

#company wants its expected profits from 1000 50 year old male policies to be $700000. What should they charge for the premium?
p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b

#using the new premium rate, calculate the standard error of the sum of 1,000 premiums
sigma_sum <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_sum

#what is the probabilty of losing money on 1,000 50 year old male policies?
pnorm(0,mu_sum, sigma_sum)
 
#a pandemic increases the probability of death within 1 year for 50 year old to 0.015
#company already sold 1000 $150,000 policies for $1150 each

#what is the expected value of companies profits over 1000 policies?
p <- 0.015 #probability of claim
a <- -150000 # loss per claim
b <- 1150 #premium - profit when no claim
n <- 1000
exp_val <- n*(a*p + b*(1-p))
exp_val

#standard error of the expecte value of the companies profits over 1000 policies?
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se

#what is the probability of the company losing money?
pnorm(0,exp_val, se)

#what is the probability of the company losing more than $1M?
pnorm(-1000000, exp_val, se)

#what is the lowest death probability for which the chance of losing money exceeds 90%?
p <- seq(0.01, 0.03, 0.001)
p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()

data.frame(p, p_lose_money)

#what is the lowest death probability for which the chance of losing over $1M exceeds 90%?
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()
data.frame(p, p_lose_million)

#create a sampling model for simulating the total profit over 1000 loans with variables defined below.
#what is the reported profit or less in millions?
set.seed(25, sample.kind = "Rounding")
p <- 0.015
loss <- -150000
profit <- 1150
n <- 1000
outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
sum(outcomes)/10^6

#set seed to 27 and run a monte carlo sim with 10,000 replicates
set.seed(27)
B <- 10000

profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes/10^6)
})
mean(profits)
mean(profits < -1)

#calculate premium required for a 5% change of losing money
n <- 1000
p <- 0.015
l <- -150000
z <- qnorm(0.05)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))
x

#what is the expect proft per policy at this rate?
ep <- l*p+x*(1-p)
ep

#expected profit over 1000 policies
mu <- n*ep
mu

#run a monte carlo sim 10000 times to determine the probability of losing money on 1000 policies given the new premium x.
set.seed(28)
B <- 10000
profit <- replicate(B, {
  draws <- sample(c(x, l), n,
                  prob=c(1-p, p), replace = TRUE)
  sum(draws)
})

mean(profit < 0)

#the company cannot predict if the pandemic death rate will stay stable. randomly change p by adding values from -0.01 to 0.01 and determine expected value over 1000 policies
set.seed(29)
B <- 10000

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, 
                  prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#probabiliy of losing money?
mean(profit < 0)

#probability of losing over $1M?
mean(profit < -1*10^6)
