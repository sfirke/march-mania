library(pacman)
p_load(readxl, tidyr, dplyr, purrr, stringr, readr)

clean_sagarin <- function(sheet_name){
  raw_dat <- read_excel(file_source, sheet_name)

  col_names <- adapt_col_names(names(raw_dat))
  
  names(raw_dat) <- "init"
  raw_dat$year <- parse_number(sheet_name)
  
  to_separate <- raw_dat %>%
    filter(str_detect(init, "^[0-9]+")) %>%
    mutate(init = gsub("\\|", "", init),
           init = gsub(" = ", "", init),
           init = gsub("\\(", " ", init),
           init = gsub("\\)", "", init),
           init = gsub("[ ]{2, }", "_", init)) # there's one whitespace between
  # Florida and St., so zap 2+ as boundary
  
  clean <- to_separate %>%
    separate(col = init, into = col_names,
             sep = "_+",
             convert = TRUE)
  
  clean
}

# takes the single string of a column and teases out what's there
# Sagarin changes which metrics he reports in each year, though the first bunch
# are consistent
adapt_col_names <- function(raw_col_name_text){
  
  col_names <- raw_col_name_text %>%
    gsub("\\|", "  ", .) %>%
    gsub("\\(", "  ", .) %>%
    gsub("\\)", "  ", .) %>%
    gsub("[ ]{2, }", "-", .) %>%
    str_split(., "-") %>%
    unlist() %>%
    tolower()
  
  col_names <- col_names[! col_names %in% c("rating", "w", "l", "schedl", "rank", "vs top 25", "vs top 50")]
  col_names <- c(rbind(col_names, paste(col_names, "rank", sep = "_"))) # interleave "rank" after the metric name
  col_names <- c("rank",
                 "teamname",
                 "rating",
                 "wins",
                 "losses",
                 "schedule",
                 "schedule_rank",
                 "win_v_top_25",
                 "loss_v_top_25",
                 "win_v_top_50",
                 "loss_v_top_50",
                 col_names
  )
  col_names
}


file_source <- "womens/data/raw/sagarin_womens.xlsx"
sheet_names <- readxl::excel_sheets("womens/data/raw/sagarin_womens.xlsx")

all_sagarin <- map_df(sheet_names, clean_sagarin)

sagarin_clean <- all_sagarin %>%
  filter(rank < 355) %>% # eliminate one bad record
  mutate(elo = coalesce(elo_score, pure_elo, elo_chess),
         elo_rank = coalesce(elo_score_rank, pure_elo_rank, elo_chess_rank)) %>%
  dplyr::select(-elo_score, -pure_elo, -elo_chess, -elo_score_rank, -pure_elo_rank, -elo_chess_rank)

write_csv(sagarin_clean, "womens/data/clean/sagarin_cleaned.csv")
