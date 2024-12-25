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
  "weights",
  prompt = FALSE
)

tuesday_chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

chess <- tuesday_chess %>%
  dplyr::filter(rated == TRUE) %>%
  dplyr::filter(winner != "draw") %>%
  dplyr::select(winner, white_rating, black_rating) %>%
  dplyr::mutate(game_rating = (white_rating + black_rating) * 0.5)

bin_size <- 100
# Correct calculation of avg_rating
white_win_data <- chess %>%
  mutate(
    avg_rating = (white_rating + black_rating) / 2,  # Average rating for grouping
    # Define breaks with "nice" round numbers
    rating_bin = cut(avg_rating,
                     breaks = seq(600, max(avg_rating, na.rm = TRUE) + bin_size, by = bin_size),
                     include.lowest = TRUE)
  ) %>%
  group_by(rating_bin) %>%
  summarise(
    white_win_rate = sum(winner == "white") / n() * 100,  # White win percentage
    total_games = n(),
    avg_rating = mean(avg_rating)  # Calculate average rating for each bin
  ) %>%
  ungroup()

white_win_data_significant <- white_win_data %>%
  filter(total_games >= 200) %>%
  mutate(rating_bin = droplevels(rating_bin))

# Convert bins to strings in the format "800-900", "900-1000", etc. without scientific notation
bins_cleaned <- gsub("\\(|\\)|\\[|\\]", "", levels(white_win_data_significant$rating_bin)) %>%
  gsub(",", "-", .) %>%
  strsplit("-") %>%
  lapply(function(x) paste0(format(as.numeric(x), scientific = FALSE, trim = TRUE), collapse = "-")) %>%
  unlist()

ggplot(white_win_data_significant, aes(x = avg_rating, y = white_win_rate)) +
  geom_col(aes(alpha = total_games), fill = "steelblue") +  # Set alpha based on total_games
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +  # Reference line at 50%
  geom_text(aes(label = total_games),
            vjust = -0.5,  # Position the text above the bars
            color = "black",
            size = 3.5) +  # Adjust text size
  geom_smooth(
    aes(weight = total_games),  # Weight the interpolation by the total number of games
    method = "lm",              # Use linear regression
    color = "darkred",          # Set line color
    se = TRUE                   # Show confidence interval
  ) +
  labs(
    title = "White Win Percentage by Rating with Weighted Interpolation",
    x = "Average Rating (binned)",
    y = "White Win Percentage (%)",
    alpha = "Total Games"  # Add legend for alpha
  ) +
  scale_x_continuous(
    breaks = white_win_data_significant$avg_rating,  # Use midpoints for breaks
    labels = bins_cleaned  # Map custom labels to the breaks
  ) +
  scale_alpha_continuous(range = c(0.5, 1)) +  # Adjust alpha scaling range
  coord_cartesian(ylim = c(40, 60)) +  # Set the visible y-axis range without clipping
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Compute correlation excluding small bins
correlation <- cor(white_win_data$avg_rating, white_win_data$white_win_rate)
print(paste("Correlation coefficient:", correlation))

# Compute weighted correlation on the filtered dataset
weighted_corr <- wtd.cor(white_win_data$avg_rating, white_win_data$white_win_rate, weight = white_win_data$total_games)
print(weighted_corr)
