
#### Modelling  -------------------------------------------------------------------------------

# Partition past data
set.seed(1)
train_index <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_index, ]
test_dat <- past_dat[-train_index, ]

# Train models for entry
glm_model <- train(lower_team_wins ~ .,
                   data = past_dat,
                   method = "glm", family = "binomial")

# skipping bayesglm and glmnet, they are virtually identical in result to simpler logistic regression above, correlation >.9

tctrl <- trainControl(method = "repeatedcv", number = 4, repeats = 3,
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

rf_test_preds <- predict(rf_model, test_dat, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, rf_test_preds)

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