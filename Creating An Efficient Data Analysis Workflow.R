# Goal: Which books are the most profitable for the company?

install.packages('readr')
library(readr)

install.packages('dplyr')
library(dplyr)

book <- read_csv('C:/Users/andya/Desktop/Study/R/Guided Project Creating An Efficient Data Analysis Workflow/book_reviews.csv')
book

dim(book)

colnames(book)

for (i in colnames(book)) {
  print(typeof(book[[i]]))
}

for (i in colnames(book)) {
  print(unique(book[[i]]))
}

booknew <- book %>%
  filter(!(is.na(review)))
booknew

booknew <- booknew %>%
  mutate(newstate = case_when(state == 'Texas'~'TX', state == 'California'~'CA', state == 'Florida'~'FL', state == 'New York'~'NY', T~state))
booknew

booknew <- booknew %>%
  mutate(review_num = case_when(review == 'Poor'~1, review == 'Fair'~2, review == 'Good'~3, review == 'Great'~4, review == 'Excellent'~5)) %>%
  mutate(is_high_review = case_when(review_num >= 4 ~ T, T~F))
booknew

by_price <- booknew %>%
  group_by(book) %>%
  summarize(sum(price))
by_price

by_book <- booknew %>%
  group_by(book) %>%
  summarize(n())
by_book