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
  "weights",
  prompt = FALSE
)

tuesday_chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

chess <- tuesday_chess %>%
  dplyr::filter(rated == TRUE) %>%
  dplyr::filter(winner != "draw") %>%
  dplyr::group_by(time_increment) %>%
  summarise(
    white_wins = sum(winner == "white"),
    black_wins = sum(winner == "black"),
    total_games = white_wins + black_wins,
    white_win_rate = (white_wins / total_games) * 100.0,
  ) %>%
  dplyr::ungroup() %>%
  # Remove unimportant time controls
  dplyr::filter(total_games > 100) %>%
  # Sort by time controls
  tidyr::separate(time_increment, into = c("base_time", "increment"), sep = "\\+", convert = TRUE) %>%
  arrange(base_time, increment) %>%
  mutate(
    time_increment = paste0(base_time, "+", increment),
    time_increment = factor(time_increment, levels = unique(paste0(base_time, "+", increment)))
  ) %>%
  select(-base_time, -increment)



ggplot(chess, aes(x = time_increment, y = white_win_rate)) +
  geom_col(aes(alpha = total_games^(0.125), fill = "steelblue")) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +  # Reference line at 50%
  geom_text(aes(label = total_games),
            vjust = -0.5,  # Position the text above the bars
            color = "black",
            size = 3.5) +  # Adjust text size
  labs(
    title = "White Win Percentage by Time Increment",
    x = "Time Increment",
    y = "White Win Percentage"
  ) +
  coord_cartesian(ylim = c(40, 60)) +  # Set the visible y-axis range without clipping
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Removes the legend
  )

