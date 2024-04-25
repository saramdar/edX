#8.machine learning - 3.2 smoothing assessment

#q1 in a previous section, we used this code to obtain mortality count in Puerto Rico for 2015-2018
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

#use loess function to obtain a smooth estimate of the expected number of deaths as a function of date.
#plot the resulting smooth funtion.  Make the span ~60 days long and use degree = 1
span <- 60/as.numeric(diff(range(dat$date))) #creating span - use 60 days and difference of range of dates after converting to numeric
fit <- dat %>% mutate(x = as.numeric(date)) %>% #convert date to numeric
  loess(deaths ~ x, degree=1, span = span, data = .) #use loess to create prediction

dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>% # add prediction called "smooth" this will be the line
  ggplot() + 
  geom_point(aes(date, deaths)) + #plot all points
  geom_line(aes(date, smooth), color = "red") # plot line called smooth


#Q2 write code for splotting smooth estimates vs day, on same plot, with different colors for years
dat %>% mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#OR

dat %>% mutate(smooth = predict(fit, as.numeric(date)), day = yday(date)) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#Q3 we want to predict 2s and 7s in the mnist_27 dataset with just the second covariate.  
library(broom)
library(caret)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy() #x_2 is not significant (see p value)
qplot(x_2, y, data = mnist_27$train) #scatterplot not helpful, y is binary

mnist_27


# fit loess line with degree =1 to the data to predict 2s and 7s in the mnist$test dataset with only second covariate. 
#what is the accuracy?
train <- mnist_27$train %>%
  mutate(y1 = ifelse(y=="7", 1, 0))
fit <- loess(y1 ~ x_2, degree=1, data= train)
p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, "7", "2"))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]
