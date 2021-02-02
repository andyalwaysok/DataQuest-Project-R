# Goal: predicting the occurrence of forest fires in Portugal using modeling techniques.

library('readr')
ff <- read_csv('C:/Users/andya/Desktop/Study/R/Guided Project Analyzing Forest Fire Data/forestfires.csv')
ff

library('dplyr')
glimpse(ff)

ff %>% pull(month) %>% unique
ff %>% pull(day) %>% unique

ff <- ff %>%
  mutate(month = factor(month, levels=c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')))

ff <- ff %>%
  mutate(day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))

glimpse(ff)


month_count <- ff %>%
  group_by(month) %>%
  summarize(count=n())
month_count

day_count <- ff %>%
  group_by(day) %>%
  summarize(count=n())
day_count

library(ggplot2)
ggplot(data = month_count, aes(x = month, y = count)) + geom_bar(stat = 'identity') + labs(title = 'Forest Fire by month', x = 'Month', y = 'Number of Occurence') + theme(panel.background = element_rect(fill = 'white')) 
ggplot(data = day_count, aes(x = day, y = count)) + geom_bar(stat = 'identity') + labs(title = 'Forest Fire by days in a week', x = 'Day', y = 'Number of Occurence') + theme(panel.background = element_rect(fill = 'white'))

library(tidyverse)
forest_fires_long <- ff %>% 
  pivot_longer(cols = c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"), names_to = "data_col", values_to = "value")
forest_fires_long

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) + geom_boxplot() + facet_wrap(vars(data_col), scale = "free_y") + labs(title = "Variable changes over month", x = "Month", y = "Variable value")

forest_fires_long %>% 
  ggplot(aes(x = day, y = value)) + geom_boxplot() + facet_wrap(vars(data_col), scale = "free_y") + labs(title = "Variable changes over day", x = "Day", y = "Variable value")

forest_fires_long %>% 
  ggplot(aes(x = value, y = area)) + geom_point() + facet_wrap(vars(data_col), scales = "free_x") + labs(title = "Relationships between other variables and area burned", x = "Value of column", y = "Area burned (hectare)")

forest_fires_long %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) + geom_point() + facet_wrap(vars(data_col), scales = "free_x") + labs(title = "Relationships between other variables and area burned (area < 300)", x = "Value of column", y = "Area burned (hectare)")
