# Scrape historical pre-season rankings
# Data already stored as mens_cbb_preseason_rankings.csv
library(pacman)
p_load(rvest, dplyr)

years <- 1962:2016 %>% as.character

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

all_data <- bind_rows(
  lapply(years, get_preseason_ranks)
)

write_csv(all_data, "mens_cbb_preseason_rankings.csv", na = "")
