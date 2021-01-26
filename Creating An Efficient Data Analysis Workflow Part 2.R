install.packages('readr')
library(readr)
install.packages('dplyr')
library(dplyr)
install.packages('stringr')
library(stringr)

sales <- read_csv('C:/Users/andya/Desktop/Study/R/Guided Project Creating An Efficient Data Analysis Workflow Part 2/sales2019.csv')
sales

glimpse(sales)

colnames(sales)

for (i in colnames(sales)) {
  print(typeof(sales[[i]]))
}

for (i in colnames(sales)) {
  print(unique(sales[[i]]))
}

# User Review and Total Purchased columns have NA values

sales <- sales %>%
  filter(!(is.na(user_submitted_review)))
glimpse(sales)

# About 18% of the data are gone

mean_purchased <- sales %>%
  filter(!(is.na(total_purchased))) %>%
  pull(total_purchased) %>%
  mean
mean_purchased

sales <- sales %>%
  mutate(new_total_purcahsed = if_else(is.na(total_purchased), mean_purchased, total_purchased))
glimpse(sales)

print(unique(sales[['user_submitted_review']]))

# I will count these as positive review: Awesome, ok, never, OK, learned
# Negative review: hated, other, not

install.packages('purrr')
library(purrr)

posneg <- function(sentence) {
  detector = case_when(str_detect(sentence, 'Awesome')~'Positive', str_detect(sentence, 'ok')~'Positive', str_detect(sentence, 'Never')~'Positive', str_detect(sentence, 'OK')~'Positive', str_detect(sentence, 'learned')~'Positive', T~'Negative')
  return(detector)
}
  
sales <- sales %>%
  mutate(pos_or_neg = unlist(map(user_submitted_review, posneg)))
  
glimpse(sales)













