library(pacman)
p_load(googlesheets, dplyr, stringr, readr)

# authenticate
ken_pom_data <- gs_key("1AauzEVB-T01TqI2hY81sT6i-gloLwPbTnh8tqsw-TYY")


# function that takes the Google Sheet data and reformats it to match training data
process_ken_pom_sheet <- function(dat, year){
  dat <- dat %>%
    mutate(year = year, class = NA, seed = str_extract(y2016$X.1, "[0-9]+")) %>%
    select(year, 1, class, 2, seed, X.2:X.18) %>%
    mutate(X.1 = gsub(" [0-9]+", "", X.1))

  names(dat) <- c("year", "rank", "class", "team", "seed", "conf", "wins_losses",
                  "pythagorean_rating", "adj_offensive_efficiency", "adj_offensive_efficiency_seed",                 "adj_defensive_efficiency", "adj_defensive_efficiency_seed",
                  "adj_tempo", "adj_tempo_seed", "luck", "luck_seed", "sos_pythagorean_rating",
                  "sos_pythagorean_seed", "opposing_offenses", "opposing_offenses_seed",
                  "opposing_defenses", "opposing_defenses_seed", "ncsos_pythagorean_rating",
                  "ncsos_pythagorean_seed")

  dat <- dat[-1, ]
  dat
}

# latest data is on first sheet, older years on subsequent sheets
y2016 <- ken_pom_data %>%
  gs_read(ws = 1)

kp_2016 <- process_ken_pom_sheet(y2016, 2016)

write_csv(kp_2016, "ken_pom/kenpom_2016.csv", na = "")
