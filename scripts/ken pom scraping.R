library(pacman)
p_load(googlesheets, dplyr, stringr, readr, tidyr, janitor)

# authenticate to the public sheet I created
# note the formula in the top left cell of each sheet
# access on the web at https://docs.google.com/spreadsheets/d/1AauzEVB-T01TqI2hY81sT6i-gloLwPbTnh8tqsw-TYY
ken_pom_data_sheet <- gs_key("1AauzEVB-T01TqI2hY81sT6i-gloLwPbTnh8tqsw-TYY")


# function that takes the Google Sheet data and reformats it to match training data
process_ken_pom_sheet <- function(dat){
  dat <- dat %>%
    clean_names() %>%
    mutate(seed = str_extract(x2, "[0-9]+")) %>% # extract seed where applicable
    mutate(x2 = gsub(" [0-9]+", "", x2)) # remove seed from school name
    names(dat) <- c("rank", "team", "conf", "wins_losses",
                  "adj_EM", "adj_offensive_efficiency", "adj_offensive_efficiency_seed",
                  "adj_defensive_efficiency", "adj_defensive_efficiency_seed",
                  "adj_tempo", "adj_tempo_seed", "luck", "luck_seed", "sos_adj_em",
                  "sos_adj_em_seed", "opposing_offenses", "opposing_offenses_seed",
                  "opposing_defenses", "opposing_defenses_seed", "ncsos_adj_em",
                  "ncsos_adj_em_seed", "year", "seed")
  
  dat <- dat[-1, ] %>%
    select(rank, everything()) %>%
    filter(!is.na(rank), rank != "Rank") %>%
    mutate(rank = as.numeric(rank)) %>%
    mutate_at(vars(adj_EM:year), parse_number)
  dat
}

# latest data is on first sheet, older years on subsequent sheets
get_kp_sheet <- function(sheet_n) {
  ken_pom_data_sheet %>%
    gs_read_cellfeed(ws = sheet_n) %>% 
    { # grab title of worksheet and include it as a column
      ws <- attr(., "ws_title")
      gs_reshape_cellfeed(.) %>% 
        mutate(source_dat = ws)      
    }

}    


# Grab and process the old years of data ---------------------------------

# skip the first sheet
kp_past_years <- seq_along(2002:2016) + 1

old_years_raw <- kp_past_years %>%
  lapply(get_kp_sheet)

old_years_processed <- lapply(old_years_raw, process_ken_pom_sheet) %>%
  bind_rows()

# Fix 2017s not having seeds
y2017 <- get_kp_sheet(1)
kp_2016 <- process_ken_pom_sheet(y2016)


write_csv(old_years_processed, "data/ken_pom_historical.csv", na = "")
