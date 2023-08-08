library(gtools)
library(tidyverse)
options(digits = 3)

medals <- permutations(8,3)
nrow(medals)

medals_jamaica <- permutations(3,3)
nrow(medals_jamaica)

#Probability of all 3 medals are won by jamaica
nrow(medals_jamaica)/nrow(medals)

3/8 * 2/7 * 1/6

#monte carlo simulation to calculate probability that all medalists will be from Jamaica
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
set.seed(1)
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

#restaurant management
# meal includes 1 entree, 2 sides, and 1 drink.  6 entree options, 6 side options, and 2 drink options.
6 * nrow(combinations(6,2)) * 2 #generate possible meal combos - 6 entrees, 2 different sides, 2 drinks

6 * nrow(combinations(6,2)) * 3 #generate possible meal combos - 6 entrees, 2 different sides, 3 drinks

6 * nrow(combinations(6,3)) * 3 #generate possible meal combos - 6 entrees, 3 different sides, 3 drinks

#how many entree choices would restaurant need to offer to have a new option for each day of the year?
entree_choices <- function(x){
  x*nrow(combinations(6,2))*3
}
combos <- sapply(1:12, entree_choices)
                 
data.frame (entrees = 1:12,combos = combos) 

data.frame (entrees = 1:12,combos = combos) %>% filter(combos >365) %>% min(.$entrees)

#instead of entrees, how many sides would need to offer to have a new option for each day of the year?
side_choices <- function(x){
  6*nrow(combinations(x,2))*3
}
combos <- sapply(2:12, side_choices)

data.frame (sides = 2:12,combos = combos) 

data.frame (sides = 2:12,combos = combos) %>% filter(combos >365) %>% min(.$sides)

#esophageal cancer and alcohol/tobacco use
head(esoph)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

#4a probability that a subject in the highest alcohol consuption group is a cancer case
esoph %>% filter(alcgp == "120+") %>% 
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases/(ncases + ncontrols)) %>% pull(p_case)

#4b probability that a subject in the lowest alcohol consumption group is a cancer case
esoph %>% filter(alcgp == "0-39g/day") %>% 
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases/(ncases + ncontrols)) %>% pull(p_case)

#4c given that a person is a case, what is the probability that they smoke 10g or more a day 
tob_cases <- esoph %>% filter(tobgp != "0-9g/day") %>%  pull(ncases) %>% sum()
tob_cases/all_cases

#4d given that a person is a control, what is the probability that they smoke 10g or more a day
tob_controls <- esoph %>% filter(tobgp != "0-9g/day") %>%  pull(ncontrols) %>% sum()
tob_controls/all_controls         

#5a For cases, what is the probability of being in the highest alcohol group
high_alc_cases <- esoph %>% filter(alcgp == "120+") %>%  pull(ncases) %>% sum()
p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc

#5b For cases, what is the probability of being in the highest tobacco group
high_tob_cases <- esoph %>% filter(tobgp == "30+") %>%  pull(ncases) %>% sum()
p_case_high_tob <- high_tob_cases/all_cases
p_case_high_tob

#5c for cases, what is the probability of being in the highest alcohol group AND highest tobacco group
high_alc_tob_cases <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%  pull(ncases) %>% sum()
p_case_high_alc_tob <- high_alc_tob_cases/all_cases
p_case_high_alc_tob

#5d for cases, what is the probability of being in the highest alcohol group OR highest tobacco group
p_case_either_highest <- p_case_high_alc + p_case_high_tob - p_case_high_alc_tob
p_case_either_highest

#5d alt answer
alt_high_either_cases <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%  pull(ncases) %>% sum()
alt_high_either_cases/all_cases

#6a for controls, what is the probability of being in the highest alcohol group
high_alc_control <- esoph %>% filter(alcgp == "120+") %>% pull(ncontrols) %>% sum()
p_high_alc_control <- high_alc_control/all_controls
p_high_alc_control

#6b how many times more likely are cases than controls to be in the highest alcohol group
p_case_high_alc/p_high_alc_control

#6c for controls - prob of being in highest tob group
high_tob_controls <- esoph %>% filter(tobgp == "30+") %>% pull(ncontrols) %>% sum()
high_tob_controls
prob_high_tob_controls <- high_tob_controls/all_controls
prob_high_tob_controls

#6d for controls - prob of being in highest alc and tob group
high_alc_tob_controls <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%  pull(ncontrols) %>% sum()
high_alc_tob_controls
prob_high_alc_tob_controls <- high_alc_tob_controls/all_controls
prob_high_alc_tob_controls

#6e for controls - what is prob of being in the highest alc OR tob group?
high_either_controls <- p_high_alc_control + prob_high_tob_controls - prob_high_alc_tob_controls
high_either_controls

#6f how many times more likely are cases than controls to be in the highest alc or tob group?
p_case_either_highest/high_either_controls

