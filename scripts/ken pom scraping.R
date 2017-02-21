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
    separate(x2, into = c("Team", "seed"), sep = " (?=[^ ]+$)", extra = "merge") %>%
    mutate(x1 = gsub(" [0-9]+", "", x1))
    names(dat) <- c("rank", "team", "seed", "conf", "wins_losses",
                  "adj_EM", "adj_offensive_efficiency", "adj_offensive_efficiency_seed",
                  "adj_defensive_efficiency", "adj_defensive_efficiency_seed",
                  "adj_tempo", "adj_tempo_seed", "luck", "luck_seed", "sos_adj_em",
                  "sos_adj_em_seed", "opposing_offenses", "opposing_offenses_seed",
                  "opposing_defenses", "opposing_defenses_seed", "ncsos_adj_em",
                  "ncsos_adj_em_seed", "source_year")
  
  dat <- dat[-1, ] %>%
    mutate(class = NA) %>%
    select(rank, class, everything()) %>%
    filter(!is.na(rank), rank != "Rank") %>%
    mutate(rank = as.numeric(rank)) %>%
    mutate(adj_EM = parse_number(adj_EM))
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


y2016 <- get_kp_sheet(2)
kp_2016 <- process_ken_pom_sheet(y2016)


# Fix 2017s not having seeds


write_csv(kp_2016, "data/ken_pom/kenpom_2016.csv", na = "")
