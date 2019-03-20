if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(caret, MASS) # load ML packages that unfortunately mask dplyr functions
p_load(tidyverse, janitor)

active_model <- read_rds("data/models/glm_all_data_no_intercept.Rds")
source("scripts/03_tidy_raw_data.R")
### Make predictions for stage 1 ----------------------------

blank_stage_1_preds <- read_csv("data/kaggle/SampleSubmissionStage1.csv") %>%
  clean_names() %>%
  separate(id, into = c("year", "lower_team", "higher_team"), sep = "_", remove = FALSE, convert = TRUE) %>%
  dplyr::select(-pred)

stage_1_with_data <- blank_stage_1_preds %>%
  add_kp_data %>% # get this from the file 03_tidy_raw_data.R ; you'll also need the object kp_dat so run that script first
  create_vars_for_prediction %>%
  mutate(lower_team_court_adv = as.factor("N")) %>%
  dplyr::select(contains("diff"), lower_team_court_adv, contains("rank"))

stage_1_preds <- predict(active_model, stage_1_with_data, type = "prob")[, 2]

preds_to_send <- blank_stage_1_preds %>%
  dplyr::select(id) %>%
  mutate(Pred = stage_1_preds)

dir.create("predictions/round 1/")
write_csv(preds_to_send, "predictions/round 1/top_model_stage_1.csv")

### Make predictions for final round ----------------------------

# For final round: Average with 538 first round predictions
# And/or, if you don't mind the impurity, gain an edge by picking a game 100% in one submission and 0% in another

final_blank <- read_csv("data/kaggle/SampleSubmissionStage2.csv") %>%
  clean_names() %>%
  separate(id, into = c("year", "lower_team", "higher_team"), sep = "_", remove = FALSE, convert = TRUE) %>%
  dplyr::select(-pred)

final_blank_with_data <- final_blank %>%
  add_kp_data %>%
  create_vars_for_prediction %>%
  mutate(lower_team_court_adv = as.factor("N")) %>%
  dplyr::select(contains("diff"), lower_team_court_adv, contains("rank")) %>%
  dplyr::select(-lower_pre_seas_rank_all, -higher_pre_seas_rank_all)

levels(final_blank_with_data$lower_team_court_adv) <- c("N", "H", "A") # to make levels match the training set

final_preds <- predict(active_model, final_blank_with_data, type = "prob")[, 2]

final_preds_to_send <- final_blank %>%
  dplyr::select(id) %>%
  mutate(Pred = final_preds)


write_csv(final_preds_to_send, "predictions/round 2/final_glm_plain.csv")

