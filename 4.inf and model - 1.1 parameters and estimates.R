library(tidyverse)
library(dslabs)
ds_theme_set()
N <- 25
p <- seq(0,1,length.out = 100)
se <- sqrt((p*(1-p)/N))
plot(p, se)

sample_sizes <- c(25, 100, 1000)
for(N in sample_sizes){
  se <- sqrt((p*(1-p)/N))
  plot(p, se)
}
