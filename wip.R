targets::tar_load_globals()

tar_load(games_draws)
tar_load(season_game_info)

team_info = cfbfastR::cfbd_team_info()

prepare_game_simulations = function(data, game_info) {

  data |>
  group_by(game_id) |>
  mutate(.prediction = case_when(.prediction == 0 ~ sample(c(3, -3, 7, -7), size = 1, replace = T), TRUE ~ .prediction)) |>
  mutate(
    pred_margin = round_any(mean(.prediction), .5),
    pred_label = case_when(
      pred_margin > 0 ~ paste(home_team, pred_margin, sep = " by "),
      pred_margin < 0 ~ paste(away_team, pred_margin, sep = " by ")
    )
  ) |>
  ungroup() |>
  left_join(
    game_info |>
      add_game_outcomes() |>
      mutate(game_outcome = case_when(
        home_margin > 0 ~ paste(home_team, home_margin, sep = " by "),
        home_margin < 0 ~ paste(away_team, home_margin, sep = " by "),
        TRUE ~ "")
      )  |>
      select(any_of(c("game_id", "home_margin", "home_win", "game_outcome")))
  ) |>
  mutate(team_color = case_when(.prediction > 0 ~ home_team, .prediction < 0 ~ away_team)) |>
  mutate(game_label = paste0(
    paste(start_date),
    "\n",
    paste(home_team, away_team, sep = " vs "),
    "\n",
    paste("Prediction:", pred_label),
    "\n",
    paste("   Actual:", game_outcome))
  ) |>
  mutate(team_color = case_when(.prediction > 0 ~ home_team, .prediction < 0 ~ away_team)) |>
  mutate(game_label = paste0(
    paste(start_date),
    "\n",
    paste(home_team, away_team, sep = " vs "),
    "\n",
    paste("Prediction:", pred_label),
    "\n",
    paste(game_outcome))
  )

}

plot_game_simulations = function(data, bins = 100, alt_colors = c('USC')) {

  data |>
  ggplot(
    aes(x=.prediction, fill = team_color)
  )+
  geom_histogram(bins = bins)+
  theme_cfb()+
  geom_vline(aes(xintercept = home_margin), linetype = 'dashed', color = 'grey80')+
  facet_wrap(game_label ~.)+
  cfbplotR::scale_fill_cfb(alt_colors = alt_colors)+
  theme(
    strip.text = element_text(hjust = 0.5) # Left-align with some margin
  )+
  coord_cartesian(xlim = c(-100, 100))+
  xlab("Simulated Game Margin")+
  ylab("Count")
}

games_draws |>
  ungroup() |>
  nest(-game_id) |>
  sample_n(1) |>
  unnest() |>
  prepare_game_simulations(game_info = season_game_info) |>
  plot_game_simulations()+
  theme(strip.text)
