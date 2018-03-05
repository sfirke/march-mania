final_blank <- read_csv("data/kaggle/SampleSubmission.csv") %>%
  separate(Id, into = c("year", "lower_team", "higher_team"), sep = "_", convert = TRUE) %>%
  dplyr::select(-Pred)

final_blank_with_data <- final_blank %>%
  add_kp_data %>%
  create_vars_for_prediction %>%
  mutate(lower_team_court_adv = as.factor("N")) %>%
  dplyr::select(contains("diff"), lower_team_court_adv, contains("rank")) %>%
  dplyr::select(-lower_pre_seas_rank_all, -higher_pre_seas_rank_all)

levels(final_blank_with_data$lower_team_court_adv) <- c("N", "H", "A") # to make levels match the training set

final_preds_1 <- predict(glm_model, final_blank_with_data, type = "prob")[, 2]

final_preds_1 <- read_csv("data/kaggle/SampleSubmission.csv") %>% # re-read to get the 3-part Kaggle unique game code 
  mutate(Pred = final_preds_1)

write_csv(final_preds_1, "predictions/round 2/final_glm.csv")

# Average with 538 1st round predictions

