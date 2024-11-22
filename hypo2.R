

#### Hypothesis
# 2. At lower ratings the difference between black and white is neglegible

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

chess <- tuesday_chess %>%
  dplyr::filter(rated == TRUE) %>%
  dplyr::filter(winner != "draw") %>%
  dplyr::select(winner, white_rating, black_rating) %>%
  dplyr::mutate(game_rating = (white_rating + black_rating) * 0.5)

white_win_data <- chess %>%
  mutate(
    avg_rating = (white_rating + black_rating) / 2,  # Average rating for grouping
    rating_bin = cut(avg_rating, breaks = seq(floor(min(avg_rating)), ceiling(max(avg_rating)), by = 50), include.lowest = FALSE)
  ) %>%
  group_by(rating_bin) %>%
  summarise(
    white_win_rate = sum(winner == "white") / n() * 100,  # White win percentage
    total_games = n()
  ) %>%
  ungroup()

# Plot white win percentage by rating range
ggplot(white_win_data, aes(x = rating_bin, y = white_win_rate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +  # Reference line at 50%
  labs(
    title = "White Win Percentage by Rating",
    x = "Average Rating (binned)",
    y = "White Win Percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


