# Script to predict win likelihood for college basketball games based on Ken Pomeroy ratings
# by Sam Firke - 2016

library(pacman)
p_load(dplyr, readr, stringr, caret, ggplot2, tidyr)

# read previous years of Ken Pomeroy data
kp_raw <- read_csv("data/ken_pom_historical.csv") %>%
  select(team_name = team, year, EM = adj_EM, adj_off = adj_offensive_efficiency, adj_def = adj_defensive_efficiency, adj_tempo)

# read preseason rankings, clean names to match Pomeroy
preseas <- read_csv("data/mens_cbb_preseason_rankings.csv") %>%
  mutate(team_name = gsub("State", "St.", team),
         team_name = plyr::mapvalues(team_name, from = c("Louisiana-Lafayette", "Miami (FL)", "NC St."), to = c("Louisiana Lafayette", "Miami FL", "North Carolina St."))) %>%
  select(-team) %>%
  rename(pre_seas_rank = rank)

kp_dat <- left_join(kp_raw, preseas) %>%
  mutate(ranked = as.factor(ifelse(is.na(pre_seas_rank), "NO", "YES"))) %>%
  rename(pre_seas_rank_all = pre_seas_rank) %>%
  mutate(pre_seas_rank_all = if_else(is.na(pre_seas_rank_all), 30, pre_seas_rank_all)) # impute rank for all unranked teams.  This is rough; could I build a model to predict preseason rank from adj_EM and then use that to backfill this?  Tough, as only ranks 1-25 are in the data.

# read training data - regular season results
past_reg_results <- read_csv("data/kaggle/RegularSeasonCompactResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year > 2001) %>% # don't have Pomeroy ratings before this year
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID number
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO")) # the outcome we'll predict

# add in NCAA tourney results from 2003-2012
past_tourney_results <- read_csv("data/kaggle/TourneyCompactResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year < 2013) %>% # per contest rules, since the first round of predictions includes 2013 tourney games, don't train on those
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID number
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO"))

all_past_results <- bind_rows(past_reg_results, past_tourney_results)

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
                                                 wloc,
                                                 plyr::mapvalues(wloc, from = c("A", "H", "N"), to = c("H", "A", "N"))))) %>% # reframe home field advantage
  dplyr::select(lower_team_wins, contains("diff"), lower_team_court_adv, contains("rank"), -contains("all")) %>% # drop unneeded vars
  filter(complete.cases(.))


# Partition past data
set.seed(1)
train_index <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_index, ]
test_dat <- past_dat[-train_index, ]

# Train best model for entry
glm_model <- train(lower_team_wins ~ .,
                   data = past_dat,
                   method = "glm", family = "binomial")

glmnet_model <- train(lower_team_wins ~ .,
                   data = past_dat,
                   method = "glmnet", family = "binomial")

bayesglm_model <- train(lower_team_wins ~ .,
                      data = past_dat,
                      method = "bayesglm", family = "binomial")


tctrl <- trainControl(method = "repeatedcv", number = 4, repeats = 3, # no rhyme or reason to this...
                      classProbs = TRUE,
                      summaryFunction = multiClassSummary)

rf_model <- train(y = train_dat$lower_team_wins,
             x = train_dat[, -1],
             method = "rf",
             metric = "logLoss",
             #   ntree = 151,
             # do.trace = TRUE,
             trainControl = trControl)


# Model evaluation

# Assess models

log_loss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

glm_test_preds <- predict(glm_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glm_test_preds)

bayesglm_test_preds <- predict(bayesglm_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, bayesglm_test_preds)

glmnet_test_preds <- predict(glmnet_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glmnet_test_preds)

rf_test_preds <- predict(rf_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, rf_test_preds)

# The three binomial models have virtually the same accuracy and are perfectly correlated
# = doesn't matter which one I pick, let's just use glm for simplicity

# Train and test ensemble model of random forest, glm - though they have same predictors
test2_ind <- createDataPartition(test_dat$lower_team_wins, list = FALSE)
test1_dat <- test_dat[test2_ind, ]
test2_dat <- test_dat[-test2_ind, ]

preds <- data.frame(glm = predict(bayesglm_model, test1_dat, type = "prob")[, 2],
                    rf = predict(rf_model, test1_dat, type = "prob")[, 2])

ens1 <- train(y = test1_dat$lower_team_wins,
              x = preds,
              method = "bayesglm", family = "binomial")

ens1_preds <- predict(ens1, data.frame(glm = predict(bayesglm_model, test2_dat, type = "prob")[, 2],
                                       rf = predict(rf_model, test2_dat, type = "prob")[, 2],
                                       lower_team_wins = test2_dat$lower_team_wins),
                      type = "prob")[, 2]

log_loss(test2_dat$lower_team_wins %>% as.numeric - 1, ens1_preds) # ensemble has 0.467 log loss, not any better

# compare to simpler model on same data
bayesglm_test_preds_testdat2 <- predict(bayesglm_model, test2_dat, type = "prob")[, 2]
log_loss(test2_dat$lower_team_wins %>% as.numeric - 1, bayesglm_test_preds_testdat2)
rf_test_preds_testdat2 <- predict(rf_model, test2_dat, type = "prob")[, 2]

# the simpler model has log loss 0.458.  Also it is >.99 correlated with the simpler of its two feeder models, vs. .90 correlated with the random forest, implying that the RF input isn't doing much.  Original correlation between glm and rf was 0.91.

### Make predictions ----------------------------

# For stage 1:
top_model <- glm_model

blank_stage_1_preds <- read_csv("data/kaggle/sample_submission.csv") %>%
  separate(id, into = c("year", "lower_team", "higher_team"), sep = "_", convert = TRUE) %>%
  select(-pred)

stage_1_with_data <- blank_stage_1_preds %>%
  add_kp_data %>%
  create_vars_for_prediction %>%
  mutate(lower_team_court_adv = as.factor("N")) %>%
  dplyr::select(contains("diff"), lower_team_court_adv, contains("rank"))

stage_1_preds <- predict(top_model, stage_1_with_data, type = "prob")[, 2]

preds_to_send <- read_csv("kaggle_data/SampleSubmission.csv") %>% # re-read to get the 3-part Kaggle unique game code 
  mutate(Pred = stage_1_preds)

write_csv(preds_to_send, "predictions/glm_1.csv")

# For final round: Average with 538 first round predictions





# Add cross-validation
# Create ensemble?