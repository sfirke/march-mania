if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(caret, MASS) # load ML packages that unfortunately mask dplyr functions
p_load(tidyverse, janitor)

active_model <- read_rds("data/models/glm_all_data_no_intercept.Rds")

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

dir.create("data/predictions")
write_csv(preds_to_send, "data/predictions/glm_1.csv")

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

predict(top_model, dummy_game, type = "prob")

final_preds_to_send <- final_blank %>%
  dplyr::select(id) %>%
  mutate(Pred = final_preds)


write_csv(final_preds_to_send, "data/predictions/final_glm_plain.csv")

# Average with 538 1st round predictions
# Or if you don't mind the impurity of it, pick a first round game 0% in one submission and 100% in the other to gain an edge

# Export list of teams with names, for manual tinkering
for_manual_editing <- left_join(
  final_blank, team_crosswalk %>%
    rename(lower_team_name = team_name, lower_team_id = team_id),
  by = c("lower_team" = "lower_team_id")
) %>%
  left_join(
    ., team_crosswalk %>%
      rename(higher_team_name = team_name, higher_team_id = team_id),
    by = c("higher_team" = "higher_team_id")
  )

write_csv(for_manual_editing, "data/predictions/blank_manual_list_of_games.csv")

manual_inputs <- read_csv("data/predictions/mens_outside_round_1_inputs.csv") %>%
  filter(round1_game) %>%
  mutate(vegas = parse_number(devigged),
         fivethirtyeight = parse_number(fivethirtyeight))

joint <- left_join(
  final_preds_to_send,
  manual_inputs %>%
    dplyr::select(id, lower_team_name, higher_team, fivethirtyeight, vegas)
)

joint$hybrid <- rowMeans(joint %>% dplyr::select(Pred, fivethirtyeight, vegas), na.rm = TRUE)
joint <- joint %>%
  dplyr::select(id, Pred = hybrid)

# Flip a certain game both ways
# Should be 8/9 seeds if that's what others use for modeling?
# And as close to 50/50 as possible.  Let's do Nevada/Texas for 2018
final_preds_1 <- joint
final_preds_1$Pred[final_preds_1$id == "2018_1305_1400"] <- 1
final_preds_2 <- joint
final_preds_2$Pred[final_preds_2$id == "2018_1305_1400"] <- 0

tabyl(final_preds_1$Pred == final_preds_2$Pred) # validation

write_csv(final_preds_1, "data/predictions/round_2_glm_1.csv")
write_csv(final_preds_2, "data/predictions/round_2_glm_0.csv")
