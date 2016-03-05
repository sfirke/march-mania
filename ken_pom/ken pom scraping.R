library(pacman)
p_load(googlesheets, dplyr)

# authenticate
ken_pom_data <- gs_key("1AauzEVB-T01TqI2hY81sT6i-gloLwPbTnh8tqsw-TYY")

# latest data is on first sheet, older years on subsequent sheets
y2016 <- ken_pom_data %>%
  gs_read(ws = 1)

# function that takes the Google Sheet data and reformats it to match training data
process_ken_pom_sheet <- function(dat){
  names(dat) <- dat[1, ] %>%
    unlist(use.names = FALSE) %>%
    tolower %>%
    plyr::mapvalues(from = c("w-l"), to = "wins_losses") # placeholder to map to names in training data
  dat <- dat[-1, ]
  dat
}


