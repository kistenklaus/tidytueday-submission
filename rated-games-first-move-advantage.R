rm(list = ls())

library(remotes)
options(repos = "https://cloud.r-project.org/")
options(scipen = 999)
if (!require("easypackages")) {
  install.packages("easypackages")
  library(easypackages)
} else {
  library(easypackages)
}
packages(
  "tidyverse",
  "tidytuesdayR",
  "dplyr",
  "readr",
  prompt = FALSE
)

tuesday_chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

# Filter and select relevant columns
chess <- tuesday_chess %>%
  filter(rated == TRUE) %>% # Fix logical comparison
  select(winner)

# Converts the chess table of column winner into a 3 columns, which count the
# amount of times a game ended with white winning, black winning or
# the game beeing drawn
outcomes <- table(chess$ winner)

outcome_percentages <- prop.table(outcomes) * 100

pie(outcome_percentages,
    labels = paste(
      names(outcome_percentages),
      "(", round(outcome_percentages, 1), "%)", sep = ""),
    col = c("lightblue", "lightgreen", "lightcoral"),
    main = "Chess Game Outcome Percentages")
