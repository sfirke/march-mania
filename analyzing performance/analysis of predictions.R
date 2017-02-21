# Analysis of March Madness predictions

library(pacman)
p_load(ggplot2, dplyr, readxl, ggrepel)

tourney2015 <- read_excel("sam_vs_nate_2015_analysis.xlsx") %>%
  mutate(difference = Sam - FiveThirtyEight) %>% 
  arrange(desc(FiveThirtyEight))

summary(tourney2015$difference)

ggplot(tourney2015, aes(x = Sam, y = FiveThirtyEight)) +
  geom_text_repel(label = paste0(tourney2015$Team1, " - ", tourney2015$Team2)) +
  geom_point() +
  geom_smooth()