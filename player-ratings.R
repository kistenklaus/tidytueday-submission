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
  select(white_id, white_rating, black_id, black_rating, start_time)

player_ratings <- chess %>%
  pivot_longer(
    cols = c(white_rating, black_rating),
    names_to = "pivot_label",
    values_to = "rating"
  ) %>%
  mutate(player_id = ifelse(pivot_label == "white_rating", white_id, black_id)) %>%
  select(player_id, rating, start_time) %>%
  group_by(player_id) %>%
  summarise(
    max_rating = max(rating),
    min_rating = min(rating),
    avg_rating = mean(rating),
    rating = rating[which.max(start_time)],
    games_played = n()
  ) %>%
  ungroup()

ggplot(player_ratings, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count)) * 100),
    binwidth = 100,
    fill = "steelblue",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Histogram of Average Player Ratings",
    x = "Rating",
    y = "Percentage of Players"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
