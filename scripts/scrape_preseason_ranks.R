# Scrape historical pre-season rankings
library(pacman)
p_load(rvest, dplyr, readr)

get_preseason_ranks <- function(year) {

  url <- paste0("http://collegepollarchive.com/mbasketball/ap/app_preseason.cfm?sort=totapp&from=", year, "&to=", year)
  page <- read_html(url)

  rank <- page %>%
    html_nodes(".td-center:nth-child(6)") %>%
    html_text %>%
    as.numeric()

  team <- page %>%
    html_nodes(".td-left") %>%
    html_text

  data_frame(rank, team, year = year)

}

all_data <- lapply(1962:2017, get_preseason_ranks) %>%
  bind_rows

write_csv(all_data, "data/mens_cbb_preseason_rankings.csv", na = "")
