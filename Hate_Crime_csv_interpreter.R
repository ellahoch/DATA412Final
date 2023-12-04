library(tidyverse)
library(janitor)

read_csv("hate_crime.csv") %>%
  filter(data_year == "2021") ->
  df

df <- df |> 
  rename(state = state_name)

df <- df |> 
  group_by(state)
count_of_crimes <- summarize(df, n = n())

leg_comp <- read_csv("state_leg2021.csv", skip = 1)
leg_comp <- clean_names(leg_comp)

combined_df <- count_of_crimes |> 
  left_join(leg_comp, by = "state")

combined_df <- combined_df |> 
  rename(hate_crime_count = n)

combined_df |> 
  filter(hate_crime_count > 450)

combined_df <- combined_df |> 
  mutate(total_prop = (senate_rep + house_rep)/total_seats)

combined_df <- combined_df |> 
  filter(!is.na(total_prop))

ggplot(combined_df, aes(total_prop, hate_crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "State Legislature Composition and 2021 Hate Crime Count",
       y = "Number of Hate Crimes Reported",
       x = "Proportion of Republicans in State Legislature")

ggplot(combined_df, aes(total_prop)) +
  geom_boxplot(bins = 7) +
  labs(x = "Proportion of Republicans in State Legislature",
       title = "Distribution of State Legislature Composition")

ggplot(combined_df, aes(hate_crime_count)) +
  geom_boxplot(bins = 10)+
  labs(x = "Hate Crime Count by State",
       title = "Distribution of Hate Crime Count by State")

### read in pop data

pop_state <- read_csv("NST-EST2021-alldata.csv")

combined_df |> left_join(pop_state)


ggplot(combined_df, aes(total_prop, hate_crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "State Legislature Composition and 2021 Hate Crime Count",
       y = "Number of Hate Crimes Reported",
       x = "Proportion of Republicans in State Legislature")