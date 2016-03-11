# Script to predict win likelihood for college basketball games based on Ken Pomeroy ratings
# by Sam Firke - 2016

library(pacman)
p_load(dplyr, readr, stringr, caret, ggplot2, tidyr)

# read previous years of Ken Pomeroy data
files_to_read <- list.files("ken_pom/") %>% .[grepl(".csv", .)]

kp_raw <- bind_rows(
  lapply(
    files_to_read, function(x) { read_csv(paste0("ken_pom/", x)) }
  ),
  .id = "year") %>%
  select(-seed, -class) %>%
  mutate(year = 2001 + as.numeric(year)) %>%
  select(team_name = team, year, pyth = pythagorean_rating, adj_off = adj_offensive_efficiency, adj_def = adj_defensive_efficiency, adj_tempo)

# read preseason rankings, clean names to match Pomeroy
preseas <- read_csv("mens_cbb_preseason_rankings.csv") %>%
  mutate(team_name = gsub("State", "St.", team),
         team_name = plyr::mapvalues(team_name, from = c("Louisiana-Lafayette", "Miami (FL)", "NC St."), to = c("Louisiana Lafayette", "Miami FL", "North Carolina St."))) %>%
  select(-team) %>%
  rename(pre_seas_rank = rank)

kp_dat <- left_join(kp_raw, preseas)  %>%
  mutate(ranked = as.factor(ifelse(is.na(pre_seas_rank), "NO", "YES")),
         pre_seas_rank_all = pre_seas_rank) %>%
  select(-pre_seas_rank)
kp_dat$pre_seas_rank_all <- kp_dat$pre_seas_rank
kp_dat$pre_seas_rank_all[is.na(kp_dat$pre_seas_rank_all)] <- 30 # impute rank for all unranked teams

# read training data - regular season results
past_reg_results <- read_csv("kaggle_data/RegularSeasonCompactResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year > 2001) %>% # don't have Pomeroy ratings before this year
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID number
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO")) # the outcome we'll predict

# add in NCAA tourney results from 2003-2011 - exempt later years per contest rules
past_tourney_results <- read_csv("kaggle_data/TourneyDetailedResults.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = season) %>%
  filter(year < 2011) %>% # per contest rules
  mutate(lower_team = pmin(wteam, lteam), # lower refers to ID number
         higher_team = pmax(wteam, lteam),
         lower_team_wins = ifelse(lower_team == wteam, "YES", "NO"))

all_past_results <- bind_rows(past_reg_results, past_tourney_results)

# read team names crosswalk
team_crosswalk <- read_csv("ken_pom_to_kaggle_crosswalk.csv")

# Join Pomeroy data to past data to make training set
kp_dat <- inner_join(kp_dat, team_crosswalk)

# joins in pomeroy data to a df with year, lower_team, higher_team
add_kp_data <- function(dat){
  dat %>%
    left_join(., kp_dat, by = c("year", "lower_team" = "team_id")) %>%
    rename(lower_team_name = team_name, lower_pyth = pyth, lower_adj_off = adj_off, lower_adj_def = adj_def, lower_adj_tempo = adj_tempo, lower_pre_seas_rank_all = pre_seas_rank_all, lower_ranked = ranked) %>%
    left_join(., kp_dat, by = c("year", "higher_team" = "team_id")) %>%
    rename(higher_team_name = team_name, higher_pyth = pyth, higher_adj_off = adj_off, higher_adj_def = adj_def, higher_adj_tempo = adj_tempo, higher_pre_seas_rank_all = pre_seas_rank_all, higher_ranked = ranked)
}
past_dat_full <- add_kp_data(all_past_results)

# Create differential variables for prediction
create_vars_for_prediction <- function(dat){
  dat %>%
    mutate(pyth_diff = lower_pyth - higher_pyth,
           adj_off_diff = lower_adj_off - higher_adj_off,
           adj_def_diff = lower_adj_def - higher_adj_def,
           adj_tempo_diff = lower_adj_tempo - higher_adj_tempo,
           pre_seas_rank_diff = lower_pre_seas_rank_all - higher_pre_seas_rank_all)
            # there are few cases where a very weak team played games but isn't in the Pomeroy table
}
past_dat <- create_vars_for_prediction(past_dat_full) %>%
  mutate(lower_team_wins = as.factor(lower_team_wins),
         lower_team_court_adv = as.factor(ifelse(lower_team == wteam,
                                                 wloc,
                                                 plyr::mapvalues(wloc, from = c("A", "H", "N"), to = c("H", "A", "N"))))) %>% # reframe home field advantage
  dplyr::select(lower_team_wins, contains("diff"), lower_team_court_adv, contains("rank"), -contains("all")) %>% # drop unneeded vars
  filter(complete.cases(.))


# Partition past data
set.seed(1)
train_ind <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_ind, ]
test_dat <- past_dat[-train_ind, ]

# Train best model for entry
top_model <- train(lower_team_wins ~ .,
                   data = past_dat,
                   method = "glm", family = "binomial")

# Make predictions
blank_stage_1_preds <- read_csv("kaggle_data/SampleSubmission.csv") %>%
  separate(Id, into = c("year", "lower_team", "higher_team"), sep = "_", convert = TRUE) %>%
  select(-Pred)

stage_1_with_data <- blank_stage_1_preds %>%
  add_kp_data %>%
  create_vars_for_prediction %>%
  mutate(lower_team_court_adv = as.factor("N")) %>%
  dplyr::select(contains("diff"), lower_team_court_adv, contains("rank"))

stage_1_preds <- predict(top_model, stage_1_with_data, type = "prob")[, 2]

preds_to_send <- read_csv("kaggle_data/SampleSubmission.csv") %>%
  mutate(Pred = stage_1_preds)
write_csv(preds_to_send, "predictions/glm_1.csv")

# Average with net prophet's
np_preds <- read_csv("predictions/steal-submission-phase1-net-prophet.csv")
avg_preds <- inner_join(np_preds, preds_to_send, by = "Id")
avg_preds$Pred <- rowMeans(avg_preds[, 2:3])
write_csv(avg_preds %>% select(Id, Pred), "predictions/avg_with_np.csv")
# Model evaluation - for selecting a model

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

glmnet1_test_preds <- predict(glmnet1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glmnet1_test_preds)

rf1_test_preds <- predict(rf1, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, rf1_test_preds)

# The three binomial models have virtually the same accuracy and are perfectly correlated
# = doesn't matter which one I pick, let's just use glm for simplicity

# Train and test ensemble model of random forest, glm - though they have same predictors
test2_ind <- createDataPartition(test_dat$lower_team_wins, list = FALSE)
test1_dat <- test_dat[test2_ind, ]
test2_dat <- test_dat[-test2_ind, ]

preds <- data.frame(glm = predict(glmb1, test1_dat, type = "prob")[, 2],
                    rf = predict(rf1, test1_dat, type = "prob")[, 2])

ens1 <- train(y = test1_dat$lower_team_wins,
              x = preds,
              method = "bayesglm", family = "binomial")

ens1_preds <- predict(ens1, data.frame(glm = predict(glmb1, test2_dat, type = "prob")[, 2],
                                       rf = predict(rf1, test2_dat, type = "prob")[, 2],
                                       lower_team_wins = test2_dat$lower_team_wins),
                      type = "prob")[, 2]

log_loss(test2_dat$lower_team_wins %>% as.numeric - 1, ens1_preds) # ensemble has 0.467 log loss, not any better

# compare to simpler model on same data
glmb1_test_preds_testdat2 <- predict(glmb1, test2_dat, type = "prob")[, 2]
log_loss(test2_dat$lower_team_wins %>% as.numeric - 1, glmb1_test_preds_testdat2)
rf1_test_preds_testdat2 <- predict(rf1, test2_dat, type = "prob")[, 2]

# the simpler model has log loss 0.458.  Also it is >.99 correlated with the simpler of its two feeder models, vs. .90 correlated with the random forest, implying that the RF input isn't doing much.  Original correlation between glm and rf was 0.91.




# Add cross-validation
# Create ensemble?