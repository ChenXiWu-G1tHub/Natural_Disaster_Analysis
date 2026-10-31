# ##Week04 Lab Session -- Problem Set Solutions -- SOCS0100
#remove everything in your environment and setting up directory

rm(list = ls())
#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
  kableExtra,#table
  flextable, #table
  glue, #combining strings and objects
  ggplot2) #dataviz

options(scipen=999) #this is to avoid scientific notation in results

# importing data in .csv format

data <- read.csv("EMDAT.csv", header = TRUE)


# Subsetting and renaming data before automating the tasks 

df <- data %>% select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, country = Entity)

## Using purrr, please automate at least one data wrangling task based on the dataset 
#(e.g. summarising data)

df %>%
  select(-country, -Year) %>%  # Remove non numerical columns 
  map_dbl(mean, na.rm = TRUE)

## Using purrr please automate plotting the trends of deaths, injuries, 
##and homelessness caused by all disasters for 5 countries in the dataset

filtered_df <- df %>% filter(country %in% c("Brazil", "Turkey", "Argentina", "Afghanistan", "Belgium"))

create_point_plot <- function(i) {
  filtered_df %>%
    ggplot(aes_string(x = names(filtered_df)[2], y = names(filtered_df)[i])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add trend lin
    labs(
      title = glue("The Trend of {names(df) [i]}"),
      y = glue("{names(df) [i]}")
    )
}

plots_list <- map(3:ncol(filtered_df), create_point_plot)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Adjust ncol as needed

