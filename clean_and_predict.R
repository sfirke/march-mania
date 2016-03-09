library(pacman)
p_load(dplyr, readr, stringr, caret, ggplot2)

# read previous years of Ken Pomeroy data
files_to_read <- list.files("ken_pom/") %>% .[grepl(".csv", .)]

kp_raw <- bind_rows(
  lapply(
    files_to_read, function(x) { read_csv(paste0("ken_pom/", x)) }
  ),
  .id = "year") %>%
  select(-seed, -class) %>%
  mutate(year = 2001 + as.numeric(year)) %>%
  select(team_name = team, year, pyth = pythagorean_rating, adj_off = adj_offensive_efficiency, adj_def = adj_defensive_efficiency)

# read training data - regular season results
past_reg_results <- read_csv("kaggle_data/RegularSeasonCompactResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year > 2001) %>%
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID #
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO")) # the outcome we'll predict

# read team names crosswalk
team_crosswalk <- read_csv("ken_pom_to_kaggle_crosswalk.csv")

# Join Pomeroy data to past data to make training set
kp_dat <- inner_join(kp_raw, team_crosswalk)

past_dat_full <- past_reg_results %>%
  left_join(., kp_dat, by = c("year", "lower_team" = "team_id")) %>%
  rename(lower_team_name = team_name, lower_pyth = pyth, lower_adj_off = adj_off, lower_adj_def = adj_def) %>%
  left_join(., kp_dat, by = c("year", "higher_team" = "team_id")) %>%
  rename(higher_team_name = team_name, higher_pyth = pyth, higher_adj_off = adj_off, higher_adj_def = adj_def)

# Create differential variables for prediction

past_dat <- past_dat_full %>%
  mutate(pyth_diff = lower_pyth - higher_pyth,
         adj_off_diff = lower_adj_off - higher_adj_off,
         adj_def_diff = lower_adj_def - higher_adj_def,
         lower_team_wins = as.factor(lower_team_wins),
         lower_team_court_adv = as.factor(ifelse(lower_team == wteam,
                                       wloc,
                                       plyr::mapvalues(wloc, from = c("A", "H", "N"), to = c("H", "A", "N"))))) %>% # reframe home field advantage
  dplyr::select(lower_team_wins, contains("diff"), lower_team_court_adv) %>% # drop unneeded vars
  filter(complete.cases(.)) # there are few cases where a very weak team played games but isn't in the Pomeroy table

# Partition past data
set.seed(1)
train_ind <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_ind, ]
test_dat <- past_dat[-train_ind, ]

# Train model

trControl = trainControl(classProbs=TRUE)

glm1 <- train(lower_team_wins ~ .,
             data = train_dat,
             method = "glm", family = "binomial")

glmb1 <- train(lower_team_wins ~ .,
               data = train_dat,
               method = "bayesglm", family = "binomial")

glmnet1 <- train(lower_team_wins ~ .,
               data = train_dat,
               method = "glmnet", family = "binomial")

rf1 <- train(y = train_dat$lower_team_wins,
             x = train_dat[, -1],
             method = "rf",
          #   ntree = 151,
         # do.trace = TRUE,
             trainControl = trControl)

# Assess models
log_loss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

glm1_test_preds <- predict(glm1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glm1_test_preds)

glmb1_test_preds <- predict(glmb1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glmb1_test_preds)

rf1_test_preds <- predict(rf1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, rf1_test_preds)

glmnet1_test_preds <- predict(glmnet1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glmnet1_test_preds)

# my rf1 model with ntree = 151 has accuracy 0.742, but on the test data it has high log-loss

# Add cross-validation
# Create ensemble?

# Load Kaggle rd 1 submission list, reformat + join to predictors
# Generate Kaggle predictions, write out, submit