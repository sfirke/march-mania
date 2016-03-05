library(pacman)
p_load(dplyr, readr, stringr, caret)

# read previous years of Ken Pomeroy data
files_to_read <- list.files("ken_pom/") %>% .[grepl(".csv", .)]

kp_raw <- bind_rows(
  lapply(
    files_to_read, function(x) { read_csv(paste0("ken_pom/", x)) }
  ),
  .id = "year") %>%
  select(-seed, -class) %>%
  mutate(year = 2001 + as.numeric(year),
         team_name = tolower(team) %>% gsub("[.]", "", .)) %>%
  select(team_name, year, pyth = pythagorean_rating, adj_off = adj_offensive_efficiency, adj_def = adj_defensive_efficiency)

# read training data - regular season results
past_reg_results <- read_csv("kaggle_data/RegularSeasonCompactResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year > 2001) %>%
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID #
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO")) # the outcome we'll predict

# read team names crosswalk
team_crosswalk <- read_csv("kaggle_data/teams.csv") %>%
  setNames(tolower(names(.))) %>%
  mutate(team_name = tolower(team_name),
         team_name = plyr::mapvalues(team_name, from = c("albany ny"), to = c("albany")))
### need to clean more here to get the full join

# Join Pomeroy data to past data to make training set
kp_dat <- inner_join(kp_raw, team_crosswalk)

past_dat_full <- left_join(past_reg_results, kp_dat, by = c("year", "lower_team" = "team_id")) %>%
  rename(lower_team_name = team_name, lower_pyth = pyth, lower_adj_off = adj_off, lower_adj_def = adj_def) %>%
  left_join(., kp_dat, by = c("year", "higher_team" = "team_id")) %>%
  rename(higher_team_name = team_name, higher_pyth = pyth, higher_adj_off = adj_off, higher_adj_def = adj_def) %>%
  filter(!is.na(lower_team_name), !is.na(higher_team_name))

# Create differential variables for prediction
past_dat <- past_dat_full %>%
  mutate(pyth_diff = lower_pyth - higher_pyth,
         adj_off_diff = lower_adj_off - higher_adj_off,
         adj_def_diff = lower_adj_def - higher_adj_def,
         lower_team_wins = as.factor(lower_team_wins))

# Partition past data
set.seed(1)
train_ind <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_ind, ]
test_dat <- past_dat[-train_ind, ]

# Train model

trainControl = trainControl(classProbs=TRUE)

glm1 <- train(lower_team_wins ~ pyth_diff + adj_off_diff + adj_def_diff,
             data = train_dat,
             method = "glm", family = "binomial")

glmb1 <- train(lower_team_wins ~ pyth_diff + adj_off_diff + adj_def_diff,
               data = train_dat,
               method = "bayesglm", family = "binomial")

glm_pyth1 <- train(lower_team_wins ~ pyth_diff,
              data = train_dat,
              method = "glm", family = "binomial")

glm_off_def1 <- train(lower_team_wins ~ adj_off_diff + adj_def_diff,
               data = train_dat,
               method = "glm", family = "binomial")

# Assess models
log_loss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

glm1_test_preds <- predict(glm1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric, glm1_test_preds)

glmb1_test_preds <- predict(glmb1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric, glmb1_test_preds)

glm_pyth1_test_preds <- predict(glm_pyth1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric, glm_pyth1_test_preds)

glm_off_def1_test_preds <- predict(glm_off_def1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric, glm_off_def1_test_preds)

# Add cross-validation
# Add other factors: e.g., control for homefield advantage in training, then set most games in NCAA tournament to "neutral"

# Load Kaggle rd 1 submission list, reformat + join to predictors
# Generate Kaggle predictions, write out, submit
