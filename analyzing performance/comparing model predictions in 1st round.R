library(pacman)
p_load(readxl, readr, dplyr, ggplot2, tidyr, stringr, corrplot)

dat <- read_excel("predictions/rd_2_avgs.xlsx") %>%
  mutate(game_desc = paste(lower_team_name, "over", higher_team_name, sep = " "))

played_games <- dat %>%
  select(game_desc, lower_team_name, higher_team_name, Sam, Ensemble, Vegas = VegasWinProb, FiveThirtyEight, NetProphet) %>%
  filter(!is.na(FiveThirtyEight)) %>%
  gather(model, prob, Sam:NetProphet) %>%
  left_join(., dat %>%
              filter(!is.na(Outcome)) %>%
              select(game_desc, outcome = Outcome)) %>%
  mutate(log_loss = -(outcome * log(prob) + (1 - outcome) * (log(1 - prob))),
         game_desc = factor(game_desc, levels = rev(levels(as.factor(game_desc)))), # order alphabetically
         true_game_desc = if_else(  # create a new variable with the _actual_ description of the game, for log_loss charting
           outcome == 0, paste(higher_team_name, "beat", lower_team_name, sep = " "),
           paste(lower_team_name, "beat", higher_team_name, sep = " "))) %>%
  group_by(game_desc) %>%
  mutate(min_log_loss = min(log_loss),
         relative_log_loss = (log_loss - min_log_loss)) %>%
  select(-contains("team_name"), -min_log_loss)


# order the games by SD of log loss, i.e. those that distinguished us the most
game_order <- played_games %>%
  group_by(game_desc, true_game_desc) %>%
  summarise(ll_sd = sd(log_loss),
            prob_sd = sd(prob)) %>%
  arrange(ll_sd) %>%
  ungroup() %>%
  mutate(game_desc = factor(game_desc, game_desc),
         true_game_desc = factor(true_game_desc, true_game_desc))

played_games$game_desc <- factor(played_games$game_desc, game_order$game_desc)
played_games$true_game_desc <- factor(played_games$true_game_desc, game_order$true_game_desc)

# colors
my_colors <- c(Sam = "#EE2C2C", Ensemble = "#EA8936", Vegas ="#034772", FiveThirtyEight = "#2888BC", NetProphet = "#73B7CE")

# chart predictions
ggplot(played_games, aes(x = prob, y = true_game_desc, color = model)) +
  geom_point(size = 4) +
  labs(y = "") +
  scale_color_manual(values = my_colors)
ggsave("prediction_comparisons.png", width = 9, height = 5.5)

# chart log-loss
ggplot(played_games, aes(x = log_loss, y = true_game_desc, color = model)) +
  geom_point(size = 4) +
  labs(y = "") +
  scale_color_manual(values = my_colors) + 
  theme_minimal()

ggplot(played_games, aes(x = log_loss, y = c(0), color = model)) +
  geom_point(size = 4) +
  facet_wrap(~ game_desc, ncol = 4) +
  labs(y = "") +
  scale_color_manual(values = my_colors) + 
  theme_minimal()

ggplot(played_games, aes(x = relative_log_loss, y = true_game_desc, color = model)) +
  geom_point(size = 4) +
  labs(y = "") +
  scale_color_manual(values = my_colors) +
  theme_minimal()

# ggsave("log-loss_comparisons.png", width = 9, height = 5.5)


# game_level_log_loss <- played_games %>%
#   select(-model, -log_loss, everything()) %>%
#   select(-prob) %>%
#   spread(model, log_loss)

ggplot(game)
# Calculate log-loss
played_games %>%
#  filter(!str_detect(game_desc, "Michigan St."), !str_detect(game_desc, "Syracuse over Virginia")) %>%
  group_by(model) %>%
  summarise(log_loss = mean(log_loss, na.rm = TRUE)) %>%
  arrange(log_loss)

played_games %>%
  select(-log_loss) %>%
  spread(model, prob) %>%
  select(Ensemble:Vegas) %>%
  cor() %>%
  corrplot(method = "number")

