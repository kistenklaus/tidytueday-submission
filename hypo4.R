

#### Hypothesis
# 4. Games with lower time control end more likely with mate or out of time.


library(remotes)
options(repos = "https://cloud.r-project.org/")
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
  dplyr::filter(rated=TRUE) %>%
  dplyr::select(winner, white_rating, black_rating, white_id, black_id, end_time)
