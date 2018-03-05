library(dplyr)
lm(EM ~ pre_seas_rank_all, data = kp_dat %>% filter(pre_seas_rank_all != 30)) %>%
  summary()

# estimate pre-season ranks from Pomeroy EM
# this was a straight linear regression on all available data (~25 cases per year back to 2002)
# linear model probably doesn't adequately capture this...

estimate_pre_season_rank <- function(em){
  fudge = 2.5 # arbitrary penalty - if these teams were unranked, they weren't perceived to be as good as their EM indicates
  max(c(
    (26.04 - em + fudge)/0.4224,
    1
  ))
}

estimate_pre_season_rank(-40)
