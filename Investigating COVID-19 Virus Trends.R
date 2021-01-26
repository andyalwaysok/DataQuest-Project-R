# The purpose of this project is to build skills and understanding of the data analysis work flow by evaluating the COVID-19 situation through this dataset.
# The goal of this project is Which countries have had the highest number of positive cases against the number of tests?
########################################################################################

install.packages('readr')
library(readr)
covid <- read_csv('C:/Users/andya/Desktop/Study/R/Guided Project Investigating COVID-19 Virus Trends/covid19.csv')
dim(covid)
vector_cols <- colnames(covid)
vector_cols
print(typeof(vector_cols))
print(head(covid), 5)
head(covid, 5)
glimpse(covid)
install.packages('dplyr')
library(dplyr)
glimpse(covid)
########################################################################################

covid_df_all_states <- covid %>%
  filter(Province_State == 'All States') %>%
  select(-Province_State)
covid_df_all_states
colnames(covid_df_all_states)
########################################################################################

covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
covid_df_all_states_daily
colnames(covid_df_all_states_daily)
########################################################################################

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarize(tested = sum(daily_tested), positive = sum(daily_positive), active = sum(active), hospitalized = sum(hospitalizedCurr)) %>%
  arrange(-tested)
covid_df_all_states_daily_sum
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_top_10
########################################################################################

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized
countries
tested_cases
positive_cases
names(positive_cases) <- countries
positive_cases
names(tested_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
positive_cases[['Russia']]
dividing <- positive_cases / tested_cases
dividing
positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)
positive_tested_top_3
########################################################################################

united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey  <- c(0.08, 2031192, 163941, 2980960, 0)
covid_mat <- rbind(united_kingdom, united_states, turkey)
covid_mat
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat
########################################################################################

question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
data_structure_list <- list(Dataframes = covid, covid_df_all_states, covid_df_all_states_daily, covid_top_10)
data_structure_list
data_structure_list <- list(Matrix = covid_mat)
data_structure_list <- list(Vectors = vector_cols, countries)
data_structure_list
data_structure_list <- list(Dataframes = covid, covid_df_all_states, covid_df_all_states_daily, covid_top_10, Matrix = covid_mat, Vectors = vector_cols, countries)
data_structure_list
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list
covid_analysis_list[2]
covid_analysis_list[[2]]
