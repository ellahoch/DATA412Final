library(tidyverse)
library(janitor)

read_csv("data/hate_crime.csv") %>%
  filter(data_year == "2021") ->
  df

df <- df |> 
  rename(state = state_name)

df <- df |> 
  group_by(state)
count_of_crimes <- summarize(df, n = n())

leg_comp <- read_csv("data/state_leg2021.csv")
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

pop_state <- read_csv("data/NST-EST2021-alldata.csv")

combined_df <- combined_df |> left_join(pop_state)


summary(lm(hate_crime_count ~ total_prop + pop_2021, data = combined_df))


## regression no outliers

# no_outliers_df <- combined_df |> 
#   filter(hate_crime_count < 450) |> 
#   filter(hate_crime_count > 2)

summary(lm(hate_crime_count ~ total_prop + pop_2021, data = no_outliers_df))

summary(lm(hate_crime_count ~ pop_2021, data = combined_df))

police_data <- read_csv("data/policeofficersbystate.csv", skip = 3, col_names = c("state","num_police"))



combined_df <- combined_df |> 
  left_join(police_data, by = "state")

summary(lm(hate_crime_count ~ num_police + pop_2021, data = combined_df))

no_outs_df <- combined_df |> 
  filter(hate_crime_count > 10)

summary(lm(hate_crime_count ~ num_police + pop_2021, data = no_outs_df))

ggplot(no_outs_df, aes(num_police, hate_crime_count, color = pop_2021)) +
  geom_point(size = 2.5) +
  labs(
    title = "Association between Number of Police, Population, and Hate \nCrime Count by State",
    x = "Number of reported police",
    y = "Hate crime count"
  )


ggplot(combined_df, aes(num_police))+
  geom_boxplot() +
  scale_y_continuous(breaks = NULL) +
  labs(
    title = "Distribution of Number of Police Registered by State",
    x = "Number of Police"
  ) 

ratio_df <- combined_df |> 
  mutate(ratio_pol_pop = num_police/pop_2021)



ggplot(ratio_df, aes(ratio_pol_pop))+
  geom_boxplot() +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Distribution of Ratio of Police Officers and State Population",
       x = "Ratio Police/Population")

summary(lm(hate_crime_count~ratio_pol_pop, data = ratio_df))


group_df <- combined_df |> 
  group_by(leg_control)

dem_control <- combined_df |> 
  filter(state_control == "Dem") |> 
  pull(hate_crime_count)

rep_control <- combined_df |> 
  filter(state_control == "Rep") |> 
  pull(hate_crime_count)

t.test(dem_control, rep_control)

pol_funding <- read_csv("data/police_funding.csv", col_types = "cd",
                        col_names = c("state", "pol_funds"))

combined_df <- combined_df |> 
  left_join(pol_funding, by = "state")
  
ggplot(combined_df, aes(pol_funds)) +
  geom_boxplot() +
  scale_y_continuous(breaks = NULL)+
  labs(
    title = "Distribution of Police Funding per capita",
    x = "Police funds"
  )

summary(lm(hate_crime_count ~ pol_funds, data = combined_df))

summary(lm(hate_crime_count~pol_funds + total_prop, data = combined_df))
