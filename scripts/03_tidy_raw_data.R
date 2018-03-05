# Script to predict win likelihood for college basketball games based on Ken Pomeroy ratings
# by Sam Firke - 2016-2018

#### Data cleaning  -------------------------------------------------------------------------------


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse)

# read previous years of Ken Pomeroy data
kp_raw <- read_csv("data/model_inputs/ken_pom.csv") %>%
  select(team_name = team, year, EM = adj_EM, adj_off = adj_offensive_efficiency, adj_def = adj_defensive_efficiency, adj_tempo)

# read preseason rankings, clean names to match Pomeroy
preseas <- read_csv("data/model_inputs/mens_cbb_preseason_rankings.csv") %>%
  # recode names to match KP
  # okay that KP doesn't have NYU, who was ranked in 1964
  mutate(team_name = gsub("State", "St.", team),
         team_name = recode(team_name, "Louisiana" = "Louisiana Lafayette", "Loyola-Chicago" = "Loyola Chicago", "Miami (FL)" = "Miami FL", "NC St." = "North Carolina St."),
         team_name = gsub(" \\([A-Z][A-Z]\\)", "", team_name)) %>%
  select(-team) %>%
  rename(pre_seas_rank = rank)

kp_dat <- left_join(kp_raw, preseas) %>%
  mutate(ranked = as.factor(ifelse(is.na(pre_seas_rank), "NO", "YES"))) %>%
  rename(pre_seas_rank_all = pre_seas_rank)
  
# impute rank for all unranked teams.  This is rough.
# Here's a crude formula, created from a regression.  See script stub_estimating_hypothetical...
# Turns out that preseason rankings is minimally used by the models, so of little import in the end

estimate_pre_season_rank <- function(em){
  fudge = 2.5 # arbitrary penalty - if these teams were unranked, they weren't perceived to be as good as their EM indicates
  max(c(
    (26.04 - em + fudge)/0.4224,
    1
  ))
}

# Impute missing preseason ranks
kp_dat$pre_seas_rank_all[is.na(kp_dat$pre_seas_rank_all)] <-
  kp_dat$EM[is.na(kp_dat$pre_seas_rank_all)] %>%
  estimate_pre_season_rank


# read training data - regular season results
treat_past_results <- function(filename){
  read_csv(filename) %>%
    clean_names() %>%
    rename(year = season, wteam = w_team_id, lteam = l_team_id) %>%
    mutate(lower_team = pmin(wteam, lteam), # lower refers to ID number
           higher_team = pmax(wteam, lteam),
           lower_team_wins = ifelse(lower_team == wteam, "YES", "NO")) # the outcome we'll predict
}

all_past_results <- bind_rows(
  treat_past_results("data/kaggle/RegularSeasonCompactResults.csv"),
treat_past_results("data/kaggle/NCAATourneyCompactResults.csv") %>%
  filter(year < rev(sort(unique(.$year)))[4]), # per contest rules, since the first round of predictions includes most recent 4 years of tourney games, don't train on those
treat_past_results("data/kaggle/SecondaryTourneyCompactResults.csv") %>%
  select(-secondary_tourney)
)

# read team names crosswalk
team_crosswalk <- read_csv("data/ken_pom_to_kaggle_crosswalk.csv")

# Join Pomeroy data to past data to make training set
kp_dat <- inner_join(kp_dat, team_crosswalk)

### Two functions that take a matchup (team1, team2, year) and merge in the correct predictor values, then calculate differences for predicting -----------
# Done as functions since the blank matchups to predict on will also need this formatting

# joins in pomeroy data to a df with year, lower_team, higher_team
add_kp_data <- function(dat){
  dat %>%
    left_join(., kp_dat, by = c("year", "lower_team" = "team_id")) %>%
    rename(lower_team_name = team_name, lower_EM = EM, lower_adj_off = adj_off, lower_adj_def = adj_def, lower_adj_tempo = adj_tempo, lower_pre_seas_rank_all = pre_seas_rank_all, lower_ranked = ranked) %>%
    left_join(., kp_dat, by = c("year", "higher_team" = "team_id")) %>%
    rename(higher_team_name = team_name, higher_EM = EM, higher_adj_off = adj_off, higher_adj_def = adj_def, higher_adj_tempo = adj_tempo, higher_pre_seas_rank_all = pre_seas_rank_all, higher_ranked = ranked)
}

# Create differential variables for prediction
create_vars_for_prediction <- function(dat){
  dat %>%
    mutate(EM_diff = lower_EM - higher_EM,
           adj_off_diff = lower_adj_off - higher_adj_off,
           adj_def_diff = lower_adj_def - higher_adj_def,
           adj_tempo_diff = lower_adj_tempo - higher_adj_tempo,
           pre_seas_rank_diff = lower_pre_seas_rank_all - higher_pre_seas_rank_all)
            # there are few cases where a very weak team played games but isn't in the Pomeroy table
}

# Apply functions to prep data for prediction
past_dat <- all_past_results %>%
  add_kp_data() %>%
  create_vars_for_prediction() %>%
  mutate(lower_team_wins = as.factor(lower_team_wins),
         lower_team_court_adv = as.factor(ifelse(lower_team == wteam,
                                                 w_loc,
                                                 recode(w_loc, "A" = "H", "H" = "A", "N" = "N")))) %>% # reframe home field advantage
  dplyr::select(lower_team_wins, contains("diff"), lower_team_court_adv, contains("rank"), -contains("all")) %>% # drop unneeded vars
  filter(complete.cases(.)) %>%
  as.data.frame()


saveRDS(past_dat, "data/model_ready/past_dat.Rds")
