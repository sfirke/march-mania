if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(caret, MASS, e1071, xgboost, randomForest) # load ML packages that unfortunately mask dplyr functions
p_load(tidyverse, Matrix)

past_dat <- read_rds("data/model_ready/past_dat.Rds")

### Duplicate data flipping lower and upper

mirror_past_dat <- past_dat %>%
  mutate(
    higher_ranked = case_when(
      higher_ranked == "YES" ~ "NO",
      higher_ranked == "NO" ~ "YES"
    ),
    lower_ranked = case_when(
      lower_ranked == "YES" ~ "NO",
      lower_ranked == "NO" ~ "YES"
    ),
    lower_team_court_adv = case_when(
      lower_team_court_adv == "A" ~ "H",
      lower_team_court_adv == "H" ~ "A",
      lower_team_court_adv == "N" ~ "N"
    ),
    lower_team_wins = case_when(
      lower_team_wins == "YES" ~ "NO",
      lower_team_wins == "NO" ~ "YES"
    )) %>%
  mutate_if(is.numeric, function(x) {-x})

past_dat <- bind_rows(past_dat, mirror_past_dat)

character_vars <- lapply(past_dat, class) == "character"
past_dat[, character_vars] <- lapply(past_dat[, character_vars], as.factor)

#### Modelling  -------------------------------------------------------------------------------

# Partition past data
set.seed(11)
train_index <- createDataPartition(past_dat$lower_team_wins, list = FALSE)
train_dat <- past_dat[train_index, ]
test_dat <- past_dat[-train_index, ]

# Train models for entry
glm_model <- train(lower_team_wins ~ .,
                   data = train_dat,
                   method = "glm", family = "binomial")

base_glm <- glm(lower_team_wins ~ 0 + .,
                data = train_dat,
                family = binomial())


# prep data for xgboost
train_xgb <- sparse.model.matrix(lower_team_wins ~ .-1, data = train_dat)
test_xgb <- sparse.model.matrix(lower_team_wins ~ .-1, data = test_dat)
all_xgb <- sparse.model.matrix(lower_team_wins ~ .-1, data = past_dat)

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


glm_test_preds <- predict(glm_model, test_dat, type = "prob")
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, glm_test_preds$YES)

xgb_test_preds <- predict(xgb_model, test_xgb, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, xgb_test_preds)

xgbl_test_preds <- predict(xgbl_model, test_xgb, type = "prob")[, 2]
log_loss(test_dat$lower_team_wins %>% as.numeric - 1, xgbl_test_preds)

# simple binomial regression performs as well as xgboost, stick with it
# retrain on all data

top_model <- train(lower_team_wins ~ 0 + .,
                   data = past_dat,
                   method = "glm", family = "binomial")

# top_model <- train(y = past_dat$lower_team_wins,
#                    x = all_xgb,
#                    method = "xgbTree")

dir.create("data/models")
saveRDS(top_model, "data/models/glm_all_data_no_intercept.Rds")


# ML part of this is kinda weak.  Could add cross-validation?  Create ensemble?