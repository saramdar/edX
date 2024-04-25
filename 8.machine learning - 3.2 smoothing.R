#8.machine learning - 3.2 smoothing
# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Smoothing

### Introduction to Smoothing

library(tidyverse)
library(gridExtra)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
grid.arrange(p1, p2, p3)

library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
     mutate(resid = resid) %>% 
     ggplot(aes(day, margin)) + 
     geom_smooth(method = "lm", se = FALSE, color = "black") +
     geom_point(aes(color = resid), size = 3)

### Bin Smoothing and Kernels

span <- 3.5
tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) 
tmp %>% filter(center %in% c(-125, -55)) %>%
     ggplot(aes(day, margin)) +   
     geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
     geom_point(size = 2) +    
     geom_smooth(aes(group = center), 
                 method = "lm", formula=y~1, se = FALSE) +
     facet_wrap(~center)

#animated example for the video
library(gganimate)
library(transformr)
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="box", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>% 
     ggplot() +
     geom_smooth(aes(day, margin, group = center), method = "lm", formula=y~1, se = FALSE) +
     transition_reveal(center) +
     geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(day, margin)) +
     transition_states(center, transition_length=1, state_length=30)+
     geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
     labs(title = 'x0 = {closest_state}')
anim_p <- animate(p, nframes=300)
anim_p

#box kernal
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel="box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")

#clarifying examples for the video
x_0 <- -125
data.frame(x = polls_2008$day) %>% mutate(w_0 = 1*I(abs(x - x_0)<=span/2)) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_step()

x_0 <- -125
tmp <- with(data.frame(day = seq(min(polls_2008$day), max(polls_2008$day), .25)), 
            ksmooth(day, 1*I(day == x_0), kernel = "normal", x.points = day, bandwidth = span))
data.frame(x = tmp$x, w_0 = tmp$y) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_line()

tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) %>% 
     mutate(weight =  dnorm(dist, 0, span/2.54))%>%
     mutate(weight = weight/max(weight))

#normal kernal
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")

### Local Weighted Regression (loess)

#span of 21 days
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth), color="red")

#different spans give different estimates
spans <- c(.66, 0.25, 0.15, 0.10)
fits <- data_frame(span = spans) %>% 
     group_by(span) %>% 
     do(broom::augment(loess(margin ~ day, degree=1, span = .$span, data=polls_2008)))
tmp <- fits %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(rank(dist) / n() <= span) %>%
     mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

tmp %>% ggplot(aes(day, margin)) +
     geom_point(size = 2, alpha = .5, color = "grey") +
     geom_line(aes(day, .fitted), data = fits, color = "red") +
     facet_wrap(~span) # produces 4 plots with different spans and estimates

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008) # degree = 1
fit_2 <- loess(margin ~ day, span = span, data=polls_2008) # degree = 2 (default)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth_1), color="red", lty = 2) +
     geom_line(aes(day, smooth_2), color="orange", lty = 1) 

#default parameters
polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth()

#ideal parameters
polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth(span = 0.15,
                 method = "loess", method.args = list(degree=1))
