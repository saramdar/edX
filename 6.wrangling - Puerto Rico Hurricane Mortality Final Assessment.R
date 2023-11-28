#6.wrangling - Puerto Rico Hurricane Mortality Final Assessment
library(tidyverse)
library(pdftools)
options(digits = 3)
library(dslabs)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

#Q1 describe the file after inspecting it using the command below
system("cmd.exe", input = paste("start", fn))

#Q2
#create a tidy dataset with each row representing one observation.  Variances will be year, month, day, and death
#examine pdf
txt <- pdf_text(fn)
class(txt)
length(txt)
txt

#Q3 extract 9th page and use str_split() so each line is a different entry
x <- strsplit(txt[9],"\n")
x
class(x)
length(x)

#Q4 define s to be the first entry of x
s <- x[[1]]
s
class(s)
length(s)

#Q5 remove extra spaces, what is the last character of element 1?
s <- str_trim(s)
s[1]

#Q6 find the row with the header (hint - find the first string that matches the pattern "2015")
header_index <- str_which(s, "2015")[1]
header_index

#Q7 extract the column names month and header (years)
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
#what month?
month
#what is the 3rd entry for year?
header[3]

#Q8 what is the row number with the totals in it?
tail_index <- str_which(s, "Total")
tail_index

#Q9 how many rows have a single number in them?
n <- str_count(s, "\\d+")
sum(n== 1)

#Q10 remove unneeded entries, how many entries left?
#define unneeded entries
out <- c(1:header_index, which(n==1), tail_index:length(s))
#remove them
s<-s[-out]
#count entries left
length(s)

#Q11 remove all text that is not a digit or a space
s <- str_remove_all(s, "[^\\d\\s]")
s

#Q12 convert  into a data matrix with just the day and death count data
s <-str_split_fixed(s, "\\s+", n=6)[,1:5]
s
class(s)
#s <- matrix(as.numeric(s),
#                ncol = ncol(s))


comp_header <- c("day", header)
comp_header
colnames(s) <- comp_header
s
mean(s[,2])
mean(s[,3])
mean(mat_s[1:19, 4])
mean(mat_s[20:30, 4])

#OR
tab <- s %>%
  as_data_frame() %>%
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])
class(tab)

#Q13 change table to tidy format
new <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths)) 
new
#OR
tab %>% pivot_longer(!day, names_to = "year", values_to = "deaths" )

#Plot
new %>%
  filter(year %in% c(2015, 2016, 2017)) %>%
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()
