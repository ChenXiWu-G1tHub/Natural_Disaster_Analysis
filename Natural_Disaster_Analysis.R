# Task 1: import dataset (identify necessary packages for the tasks before conducting the tasks)
install.packages("pacman")
pacman::p_load(tidyverse, kableExtra, flextable, skimr)
data <- read.csv("EMDAT.csv", header = TRUE)

# Completing the actual task
skim(data)
# Select the variables that can reflect the information related to deaths, injuries, homelessness caused by all disasters;after this, renaming them.
df <- data %>% select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, country = Entity)
glimpse(df)

# Create three tables showing the highest averages of deaths, injuries, 
#and homelessness (e.g. top 10)
# Calculate the averages
averages <- df %>%
  filter(!country %in% c("World", "Soviet Union")) %>%  # Remove "World" and "Soviet Union"
  group_by(country) %>%
  summarise(
    avg_deaths = mean(deaths, na.rm = TRUE),
    avg_injuries = mean(injuries, na.rm = TRUE),
    avg_homelessness = mean(homelessness, na.rm = TRUE))

# Create tables for the top 10 averages
top_10_deaths <- averages %>%
  arrange(desc(avg_deaths)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Deaths")

# Apply formatting
top_10_deaths %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

top_10_injuries <- averages %>%
  arrange(desc(avg_injuries)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Injuries")
top_10_injuries %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

top_10_homelessness <- averages %>%
  arrange(desc(avg_homelessness)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Homelessness")
top_10_homelessness %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)    
# Create a new binary variable in the original dataset to
#show whether the number of deaths by all disasters is higher than 500 in a given year

df <- df %>% mutate(high_death = ifelse(deaths > 500, 1, 0))

# Reshape the dataset (selected version) and save it as a separate dataset in your repository
df_wide <- df %>%
  pivot_wider(
    names_from = Year,       # Specify the columns to pivot
    values_from = c(deaths, injuries, homelessness, high_death)  # Specify the values columns
  )
# Save the df_wide data frame as a separate R data set
saveRDS(df_wide, "df_wide.rds")    
