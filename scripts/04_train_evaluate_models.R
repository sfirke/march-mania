if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(caret, MASS) # load ML packages that unfortunately mask dplyr functions
p_load(tidyverse, modelr)

past_dat <- read_rds("data/model_ready/past_dat.Rds")


#### Modelling  -------------------------------------------------------------------------------

# Partition past data
set.seed(1)
train_index <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_index, ]
test_dat <- past_dat[-train_index, ]

# Train models for entry
glm_model <- train(lower_team_wins ~ .,
                   data = train_dat,
                   method = "glm", family = "binomial")


tc <- trainControl(method = "repeatedcv", number = 4, repeats = 3,
                      classProbs = TRUE,
                      summaryFunction = multiClassSummary)

rf_model <- train(y = train_dat$lower_team_wins,
                  x = train_dat[, -1],
                  method = "rf",
                  do.trace = TRUE,
              #    trControl = tc,
                  ntree = 150
                  )

# prep data for xgboost
train_xgb <- sparse.model.matrix(lower_team_wins ~ .-1, data = train_dat)
test_xgb <- sparse.model.matrix(lower_team_wins ~ .-1, data = test_dat)

xgb_model <- train(y = train_dat$lower_team_wins,
                   x = train_xgb,
                   method = "xgbTree")

xgbl_model <- train(y = train_dat$lower_team_wins,
                   x = train_xgb,
                   method = "xgbLinear")

# Model evaluation

# Assess models

log_loss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}


glm_test_preds <- predict(glm_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glm_test_preds)

rf_test_preds <- predict(rf_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, rf_test_preds)

xgb_test_preds <- predict(xgb_model, test_xgb, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, xgb_test_preds)

xgbl_test_preds <- predict(xgbl_model, test_xgb, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, xgbl_test_preds)

# Random Forest model has higher log loss, more extreme predictions
# may be overconfident b/c not correctly using leaf class percentages?

# simple binomial regression performs as well as xgboost, stick with it
# retrain on all data

top_model <- train(lower_team_wins ~ .,
                   data = past_dat,
                   method = "glm", family = "binomial")


### Make predictions for stage 1 ----------------------------

# For stage 1:

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