prepare_games <- function(data) {
  data |>
    as_tibble() |>
    mutate(
      game_date = lubridate::as_datetime(start_date)
    ) |>
    select(
      game_id,
      season,
      week,
      season_type,
      game_date,
      completed,
      neutral_site,
      conference_game,
      attendance,
      venue_id,
      venue,
      home_id,
      home_team,
      home_conference,
      home_division,
      home_points,
      away_id,
      away_team,
      away_conference,
      away_division,
      away_points,
      notes
    ) |>
    # fix for division
    mutate(
      across(
        contains("division"),
        ~ replace_na(.x, "missing")
      )
    ) |>
    mutate(
      outcome = case_when(
        home_points > away_points ~ "home win",
        home_points > away_points ~ "away win",
        home_points == away_points ~ "tie"
      )
    ) |>
    mutate(
      home_win =
        case_when(
          home_points > away_points ~ "yes",
          TRUE ~ "no"
        )
    ) |>
    mutate(home_win = factor(home_win, levels = c("no", "yes")))
}

tune_elo_ratings <- function(games, params) {
  games |>
    calc_elo_ratings(
      k = params$k,
      v = params$v,
      reversion = params$reversion,
      home_field_advantage = params$home_field_advantage
    )
}

assess_elo_ratings <-
  function(data,
           metrics = yardstick::metric_set(
             yardstick::roc_auc,
             yardstick::brier_class,
             yardstick::mn_log_loss,
             yardstick::accuracy,
             yardstick::pr_auc
           )) {
    tmp <-
      data |>
      select(game_outcomes, settings) |>
      unnest(cols = c(game_outcomes, settings)) |>
      add_elo_predictions()

    # overall
    overall <-
      tmp |>
      group_by(home_field_advantage, reversion, k, v) |>
      metrics(
        truth = home_win,
        estimate = home_pred,
        home_prob,
        event_level = "second"
      )

    # by season
    season <-
      tmp |>
      group_by(season, home_field_advantage, reversion, k, v) |>
      metrics(
        truth = home_win,
        estimate = home_pred,
        home_prob,
        event_level = "second"
      )

    tibble(
      overall = list(overall),
      seasons = list(season)
    )
  }

select_elo_params <- function(data, n = 1) {
  data |>
    pivot_wider(names_from = .metric, values_from = .estimate) |>
    arrange(desc(accuracy)) |>
    mutate(
      accuracy_d = d_max(accuracy, use_data = T),
      roc_d = d_max(roc_auc, use_data = T),
      pr_auc_d = d_max(pr_auc, use_data = T),
      loss_d = d_min(mn_log_loss, use_data = T),
      brier_d = d_min(brier_class, use_data = T),
      overall = d_overall(across(ends_with("_d")))
    ) |>
    slice_max(overall, n = 1)
}


calc_elo_ratings <-
  function(games,
           teams = list(),
           team_seasons = list(),
           home_field_advantage,
           reversion,
           k,
           v,
           verbose = TRUE) {
    # define an empty tibble to store the game outcomes
    game_outcomes <- tibble()

    # loop over games
    for (i in 1:nrow(games)) {
      ### get the individual game
      game <- games[i, ]

      ### get pre game elo ratings
      # look for team in teams list
      # if not defined, set to 1500 for fbs teams and 1200 for non fbs

      # get home elo rating
      if (game$home_team %in% names(teams)) {
        home_rating <- teams[[game$home_team]]
      } else if (game$home_division == "fbs") {
        home_rating <- 1500
      } else {
        home_rating <- 1200
      }

      # get away elo rating
      if (game$away_team %in% names(teams)) {
        away_rating <- teams[[game$away_team]]
      } else if (game$away_division == "fbs") {
        away_rating <- 1500
      } else {
        away_rating <- 1200
      }

      # check whether its a neutral site game to apply home field advantage adjustment
      if (game$neutral_site == T) {
        add_home_field_advantage <- 0
      } else if (game$neutral_site == F) {
        add_home_field_advantage <- home_field_advantage
      }

      # check whether the team has already played in a season
      # check whether the season of the game is the same season
      # as the current team season value
      # if not, apply a mean reversion

      # set reversion amount
      # home team
      if (length(team_seasons[[game$home_team]]) == 0) {
        team_seasons[[game$home_team]] <- game$season
      } else if (game$season == team_seasons[[game$home_team]]) {
        home_rating <- home_rating
      } else if (
        (team_seasons[[game$home_team]] < game$season) & game$home_division == "fbs"
      ) {
        home_rating <- ((reversion * 1500) + (1 - reversion) * home_rating)
      } else if (
        (team_seasons[[game$home_team]] < game$season) & game$home_division != "fbs"
      ) {
        home_rating <- ((reversion * 1200) + (1 - reversion) * home_rating)
      }

      # away team
      if (length(team_seasons[[game$away_team]]) == 0) {
        team_seasons[[game$away_team]] <- game$season
      } else if (game$season == team_seasons[[game$away_team]]) {
        away_rating <- away_rating
      } else if (
        (team_seasons[[game$away_team]] < game$season) & game$away_division == "fbs"
      ) {
        away_rating <- ((reversion * 1500) + (1 - reversion) * away_rating)
      } else if (
        (team_seasons[[game$away_team]] < game$season) & game$away_division != "fbs"
      ) {
        away_rating <- ((reversion * 1200) + (1 - reversion) * away_rating)
      }


      ### recruiting
      # get each team's

      ## get the score margin based on the home team
      home_margin <- game$home_points - game$away_points

      # define outcome
      if (home_margin > 0) {
        home_outcome <- "win"
      } else if (home_margin < 0) {
        home_outcome <- "loss"
      } else if (home_margin == 0) {
        home_outcome <- "tie"
      }

      if (home_margin > 0) {
        away_outcome <- "loss"
      } else if (home_margin < 0) {
        away_outcome <- "win"
      } else if (home_margin == 0) {
        away_outcome <- "tie"
      }

      # get updated elo for both teams
      new_elos <- get_new_elos(
        home_rating,
        away_rating,
        home_margin,
        add_home_field_advantage,
        k,
        v
      )

      # add pre game elo ratings to the selected game
      # do not include the adjustment for home advantage in the pre game
      game$home_pregame_elo <- home_rating
      game$away_pregame_elo <- away_rating

      # add pre game prob
      game$home_prob <- new_elos[3]
      game$away_prob <- new_elos[4]

      # add post game elo ratings to the selected game
      game$home_postgame_elo <- new_elos[1]
      game$away_postgame_elo <- new_elos[2]

      # get the score and game outcome
      game$home_margin <- home_margin
      game$home_outcome <- home_outcome
      game$away_margin <- -home_margin
      game$away_outcome <- away_outcome

      # update the list storing the current elo rating for each team
      teams[[game$home_team]] <- new_elos[1]
      teams[[game$away_team]] <- new_elos[2]

      # upaate the list storing the current team season
      team_seasons[[game$home_team]] <- game$season
      team_seasons[[game$away_team]] <- game$season

      # store
      game_outcomes <- bind_rows(
        game_outcomes,
        game
      )

      # log output
      if (verbose == T) {
        cat("\r", i, "of", nrow(games), "games completed")
        flush.console()
      }
    }

    # create a table at the team level that is easy to examine the results
    team_outcomes <-
      game_outcomes |>
      longer_games()

    # store settings
    settings <-
      tibble(
        home_field_advantage = home_field_advantage,
        reversion = reversion,
        k = k,
        v = v
      )

    tibble(
      game_outcomes = list(game_outcomes),
      team_outcomes = list(team_outcomes),
      team_seasons = list(team_seasons),
      teams = list(teams),
      settings = list(settings)
    )
  }

get_new_elos <-
  function(home_rating,
           away_rating,
           home_margin,
           home_field_advantage,
           k,
           v) {
    # get observed home score
    # if the home team wins, then home score = 1
    # if the home team loses, then home score = 0
    # in a tie, home score = 0.5
    if (home_margin > 0) {
      home_score <- 1
    } else if (home_margin < 0) {
      home_score <- 0
    } else {
      home_score <- 0.5
    }

    # get observed away score, 1-home
    away_score <- 1 - home_score

    ## determine whether there is home field advantage

    ## get home and away expected scores
    # get expected home score based on the pre game rating and home field advantage
    home_expected_score <- get_expected_score(home_rating + home_field_advantage,
                                              away_rating,
                                              v = v
    )


    # get expected away score based on pre game rating
    away_expected_score <- get_expected_score(away_rating,
                                              home_rating + home_field_advantage,
                                              v = v
    )

    ## define margin of victory multiplier based on winner
    if (home_margin > 0) {
      mov_multi <- log(abs(home_margin) + 1) * (2.2 / (((home_rating + home_field_advantage - away_rating) * 0.001) + 2.2))
    } else if (home_margin < 0) {
      mov_multi <- log(abs(home_margin) + 1) * (2.2 / (((away_rating - home_rating + home_field_advantage) * 0.001) + 2.2))
    } else {
      mov_multi <- 2.2 * log(2)
    }

    ## update ratings
    # update home rating
    home_new_rating <- home_rating +
      (mov_multi *
         k *
         (home_score - home_expected_score))

    # update away rating
    away_new_rating <- away_rating +
      (mov_multi *
         k *
         (away_score - away_expected_score))

    return(c(
      home_new_rating,
      away_new_rating,
      home_expected_score,
      away_expected_score
    ))
  }

get_expected_score <-
  function(team_rating, opponent_rating, v = 400) {
    return(1 / (1 + 10^((opponent_rating - team_rating) / v)))
  }


add_elo_predictions <- function(data) {
  data |>
    mutate(
      home_pred =
        case_when(
          home_prob >= .5 ~ "yes",
          TRUE ~ "no"
        )
    ) |>
    mutate(
      home_win = case_when(
        home_outcome == "win" ~ "yes",
        TRUE ~ "no"
      )
    ) |>
    mutate(
      home_pred = factor(home_pred, levels = c("no", "yes")),
      home_win = factor(home_win, levels = c("no", "yes"))
    )
}

add_spread_features <- function(data) {
  data |>
    mutate(
      home_score_diff = home_points - away_points,
      home_elo_diff = home_pregame_elo - away_pregame_elo,
      home_game = case_when(
        neutral_site == F ~ 1,
        TRUE ~ 0
      ),
      neutral_site = case_when(
        neutral_site == T ~ 1,
        TRUE ~ 0
      )
    )
}

model_spread <- function(data) {
  data %>%
    lm(
      home_score_diff ~ home_elo_diff + home_game - 1,
      data = .
    )
}

# pivot home-away table to team table
longer_games <- function(data, game_vars = c("game_id", "season_type", "week", "game_date")) {
  home <-
    data |>
    select(
      all_of(game_vars),
      starts_with("home_"),
      opponent = away_team,
      opponent_points = away_points
    ) |>
    rename_with(.fn = ~ gsub("home_", "", .x), cols = everything())

  away <-
    data |>
    select(
      all_of(game_vars),
      starts_with("away_"),
      opponent = home_team,
      opponent_points = home_points
    ) |>
    rename_with(.fn = ~ gsub("away_", "", .x), cols = everything())

  bind_rows(home, away)
}

# simulate future elo ratings
sim_elo_ratings <-
  function(games,
           teams = list(),
           team_seasons = list(),
           home_field_advantage,
           reversion,
           k,
           v,
           ties = F,
           points_model = points_model,
           verbose = F) {
    # define an empty tibble to store the game outcomes
    game_outcomes <- tibble()

    # loop over games
    for (i in 1:nrow(games)) {
      ### get the individual game
      game <- games[i, ]

      ### get pre game elo ratings
      # look for team in teams list
      # if not defined, set to 1500 for fbs teams and 1200 for non fbs

      # get home elo rating
      if (game$home_team %in% names(teams)) {
        home_rating <- teams[[game$home_team]]
      } else if (game$home_division == "fbs") {
        home_rating <- 1500
      } else {
        home_rating <- 1200
      }

      # get away elo rating
      if (game$away_team %in% names(teams)) {
        away_rating <- teams[[game$away_team]]
      } else if (game$away_division == "fbs") {
        away_rating <- 1500
      } else {
        away_rating <- 1200
      }

      # check whether its a neutral site game to apply home field advantage adjustment
      if (game$neutral_site == t) {
        add_home_field_advantage <- 0
      } else if (game$neutral_site == f) {
        add_home_field_advantage <- home_field_advantage
      }

      # check whether the team has already played in a season
      # check whether the season of the game is the same season
      # as the current team season value
      # if not, apply a mean reversion

      # set reversion amount
      # home team
      if (length(team_seasons[[game$home_team]]) == 0) {
        team_seasons[[game$home_team]] <- game$season
      } else if (game$season == team_seasons[[game$home_team]]) {
        home_rating <- home_rating
      } else if (
        (team_seasons[[game$home_team]] < game$season) & game$home_division == "fbs"
      ) {
        home_rating <- ((reversion * 1500) + (1 - reversion) * home_rating)
      } else if (
        (team_seasons[[game$home_team]] < game$season) & game$home_division != "fbs"
      ) {
        home_rating <- ((reversion * 1200) + (1 - reversion) * home_rating)
      }

      # away team
      if (length(team_seasons[[game$away_team]]) == 0) {
        team_seasons[[game$away_team]] <- game$season
      } else if (game$season == team_seasons[[game$away_team]]) {
        away_rating <- away_rating
      } else if (
        (team_seasons[[game$away_team]] < game$season) & game$away_division == "fbs"
      ) {
        away_rating <- ((reversion * 1500) + (1 - reversion) * away_rating)
      } else if (
        (team_seasons[[game$away_team]] < game$season) & game$away_division != "fbs"
      ) {
        away_rating <- ((reversion * 1200) + (1 - reversion) * away_rating)
      }

      ## simulate the margin via specified points model
      home_margin <-
        sim_game_margin(
          home_rating + add_home_field_advantage,
          away_rating,
          points_model
        )

      # adjust for ties
      # if ties =f, then a margin of 0 will give home team a 1 point win
      if (ties == F & home_margin == 0) {
        home_margin <- 1
      } else {
        home_margin <- home_margin
      }

      ## get the score margin based on the home team
      #  home_margin = game$home_points - game$away_points

      # define outcome
      if (home_margin > 0) {
        home_outcome <- "win"
      } else if (home_margin < 0) {
        home_outcome <- "loss"
      } else if (home_margin == 0) {
        home_outcome <- "tie"
      }
      if (home_margin > 0) {
        away_outcome <- "loss"
      } else if (home_margin < 0) {
        away_outcome <- "win"
      } else if (home_margin == 0) {
        away_outcome <- "tie"
      }

      # get updated elo for both teams
      new_elos <- get_new_elos(
        home_rating,
        away_rating,
        home_margin,
        add_home_field_advantage,
        k,
        v
      )

      # add pre game elo ratings to the selected game
      # do not include the adjustment for home advantage in the pre game
      game$home_pregame_elo <- home_rating
      game$away_pregame_elo <- away_rating

      # add pre game prob
      game$home_prob <- new_elos[3]
      game$away_prob <- new_elos[4]

      # add post game elo ratings to the selected game
      game$home_postgame_elo <- new_elos[1]
      game$away_postgame_elo <- new_elos[2]

      # get the score and game outcome
      game$home_sim_margin <- home_margin
      game$home_sim_outcome <- home_outcome
      game$away_sim_margin <- -home_margin
      game$away_sim_outcome <- away_outcome

      # update the list storing the current elo rating for each team
      teams[[game$home_team]] <- new_elos[1]
      teams[[game$away_team]] <- new_elos[2]

      # upaate the list storing the current team season
      team_seasons[[game$home_team]] <- game$season
      team_seasons[[game$away_team]] <- game$season

      # store
      game_outcomes <- bind_rows(
        game_outcomes,
        game
      )

      # log output
      if (verbose == t) {
        cat("\r", i, "of", nrow(games), "games completed")
        flush.console()
      }
    }

    team_outcomes <-
      game_outcomes |>
      longer_games()

    # store settings
    settings <-
      tibble(
        home_field_advantage = home_field_advantage,
        reversion = reversion,
        k = k,
        v = v
      )

    tibble(
      game_outcomes = list(game_outcomes),
      team_outcomes = list(team_outcomes),
      team_seasons = list(team_seasons),
      teams = list(teams),
      settings = list(settings)
    )
  }

# prepare_game_weeks <- function(data, season_weeks) {
#   data |>
#     select(season, season_type, week, home_team, away_team, game_id, start_date) |>
#     mutate(start_date = as.Date(start_date)) |>
#     left_join(
#       season_weeks |>
#         distinct(),
#       by = join_by(season, season_type, week),
#       relationship = "many-to-many"
#     ) |>
#     mutate(diff = as.numeric(week_date - start_date)) |>
#     filter(diff > 0) |>
#     group_by(game_id) |>
#     slice_min(diff, n = 1) |>
#     ungroup() |>
#     select(-diff) |>
#     select(season, season_week, week, week_date, season_type, home_team, away_team, game_id, start_date) |>
#     arrange(start_date, week_date)
# }

add_season_week <- function(data) {
  data |>
    mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7)))
}

prepare_team_estimates <- function(data) {
  data |>
    arrange(week_date) |>
    # remove offense and defense special teams estimates; only using overall
    anti_join(
      tibble(
        type = c("offense", "defense"),
        play_situation = "special"
      ),
      by = join_by(play_situation, type)
    ) |>
    # get week from season week
    add_season_week() |>
    select(season, season_week, season_type, week_date, team, metric, type, estimate) |>
    pivot_wider(
      names_from = c("type"),
      values_from = c("estimate")
    ) |>
    select(season_week, season, season_type, week_date, team, any_of(c("overall", "offense", "defense", "special"))) |>
    rename_with(
      .cols = any_of(c("overall", "offense", "defense", "special")),
      .fn = ~ paste0("postgame_", .x)
    ) |>
    group_by(team) |>
    mutate(
      pregame_overall = dplyr::lag(postgame_overall, 1),
      pregame_offense = dplyr::lag(postgame_offense, 1),
      pregame_defense = dplyr::lag(postgame_defense, 1),
      pregame_special = dplyr::lag(postgame_special, 1)
    ) |>
    ungroup()
}
#
# join_team_estimates <- function(data, season_vars = c("season", "season_type", "season_week"), estimates) {
#   data |>
#     inner_join(
#       estimates |>
#         select(
#           any_of(season_vars),
#           home = team,
#           home_overall = pregame_overall,
#           home_offense = pregame_offense,
#           home_defense = pregame_defense,
#           home_special = pregame_special
#         ),
#       by = join_by(season, season_week, season_type, home)
#     ) |>
#     inner_join(
#       estimates |>
#         select(
#           any_of(season_vars),
#           away = team,
#           away_overall = pregame_overall,
#           away_offense = pregame_offense,
#           away_defense = pregame_defense,
#           away_special = pregame_special
#         ),
#       by = join_by(season, season_week, season_type, away)
#     )
# }

add_game_outcomes <- function(data) {
  data |>
    mutate(
      home_margin = home_points - away_points,
      home_win = case_when(
        home_points > away_points ~ "yes",
        home_points < away_points ~ "no"
      ),
      home_win = factor(home_win, levels = c("yes", "no")),
      total_points = home_points + away_points
    )
}

prepare_fcs_teams <- function(data) {
  # handle fcs case
  data |>
    mutate(
      home = case_when(is.na(home_division) | home_division != "fbs" ~ "fcs", TRUE ~ home_team),
      away = case_when(is.na(away_division) | away_division != "fbs" ~ "fcs", TRUE ~ away_team)
    )
}

add_game_weeks <- function(data) {
  find_nearest_week <- function(data) {
    data |>
      mutate(diff = as.numeric(week_date - start_date)) |>
      filter(diff > 0) |>
      group_by(game_id) |>
      slice_min(diff, n = 1) |>
      ungroup() |>
      select(-diff)
  }

  season_weeks <-
    data |>
    find_season_weeks()

  joined <-
    data |>
    mutate(start_date = as.Date(start_date)) |>
    left_join(
      season_weeks,
      by = c("season", "season_type", "week"),
      relationship = "many-to-many"
    )

  out <-
    joined |>
    find_nearest_week() |>
    select(season, season_type, season_week, week, week_date, week, start_date, game_id, everything()) |>
    arrange(start_date)

  if (nrow(data) > nrow(out)) {
    dropped <- data$game_id[!(data$game_id %in% out$game_id)]

    warning(paste(paste(dropped, collapse = ","), "game ids dropped from data"))
  } else if (nrow(data) < nrow(out)) {
    warning("too many games returned; check")
  }

  out
}

rename_team_variables <- function(
    data,
    home_away,
    team_variables = c("pregame_overall", "pregame_offense", "pregame_defense", "pregame_special"),
    season_variables = c("season", "season_type", "season_week")) {
  team_text <- paste0(home_away, "_")

  data |>
    select(
      any_of(season_variables),
      team,
      any_of(team_variables)
    ) |>
    rename_with(
      .cols = any_of(team_variables),
      .fn = function(x) gsub("^[^_]+_", team_text, x)
    ) |>
    rename_with(
      .cols = "team",
      .fn = ~ paste0(team_text, .x)
    )
}

join_team_variables <- function(
    games_data,
    teams_data,
    team_variables = c("pregame_overall", "pregame_offense", "pregame_defense", "pregame_special"),
    season_variables = c("season", "season_type", "season_week")) {
  home_team_data <-
    teams_data |>
    rename_team_variables(
      home_away = "home",
      team_variables = team_variables,
      season_variables = season_variables
    ) |>
    rename(home = home_team)

  away_team_data <-
    teams_data |>
    rename_team_variables(
      home_away = "away",
      team_variables = team_variables,
      season_variables = season_variables
    ) |>
    rename(away = away_team)

  games_data |>
    left_join(
      home_team_data
    ) |>
    left_join(
      away_team_data
    )
}

prepare_game_info <- function(data) {
  data |>
    mutate(
      start_date = as.Date(start_date),
      neutral_site = case_when(neutral_site == T ~ 1, TRUE ~ 0)
    ) |>
    select(
      -ends_with("_elo"),
      -ends_with("excitement_index"),
      -ends_with("post_win_prob"),
      -any_of(c("highlights")),
      -any_of(c("notes"))
    ) |>
    filter(season_type %in% c("regular", "postseason"))
}


prepare_game_estimates <- function(
    games,
    team_estimates,
    season_variables = c("season", "season_type", "season_week"),
    team_variables = c("pregame_overall", "pregame_offense", "pregame_defense", "pregame_special")) {
  games |>
    prepare_game_info() |>
    add_game_weeks() |>
    prepare_fcs_teams() |>
    join_team_variables(
      teams_data = team_estimates,
      team_variables = team_variables,
      season_variables = season_variables
    )
}

join_team_scores <- function(data, estimates) {
  data |>
    left_join(
      estimates |>
        select(
          season,
          season_type,
          season_week,
          home = team,
          home_overall = overall
        )
    ) |>
    left_join(
      estimates |>
        select(
          season,
          season_type,
          season_week,
          away = team,
          away_overall = overall
        )
    )
}

simulate_games <- function(object, newdata, ndraws = 1000, seed = 1999, ...) {
  if (class(object) == "workflow") {
    model <- object |>
      hardhat::extract_fit_engine()
  } else if (inherits(model |> extract_fit_engine(), "stanreg")) {
    model <- object
  } else {
    error("requires stanreg")
  }

  model |>
    tidybayes::predicted_draws(newdata = newdata, ndraws = ndraws,  seed = seed, ...) |>
    mutate(.prediction = case_when(
      .prediction == 0 ~ sample(c(3, -3), size = 1, replace = T),
      TRUE ~ .prediction
    ))
}

summarize_simulations <- function(simulations, seed = 1999) {

  set.seed(1999)
  simulations |>
    mutate(
      home_win = case_when(
        .prediction > 0 ~ "yes",
        .prediction == 0 ~ sample(c("yes", "no"), size = 1, replace = T),
        .prediction < 0 ~ "no"
      ),
      home_win = factor(home_win, levels = c("yes", "no"))
    ) |>
    group_by(across(any_of(c("season", "season_type", "season_week", "game_id", "home_team", "away_team")))) |>
    summarize(
      home_wins = sum(home_win == "yes"),
      pred_margin = round_any(mean(.prediction), .5),
      sims = n_distinct(.draw),
      .groups = "drop"
    ) |>
    mutate(
      home_prob = home_wins / sims,
      home_pred = case_when(
        pred_margin > 0 ~ 'yes',
        pred_margin < 0 ~ 'no',
        home_prob  > .5 ~ 'yes',
        home_prob < .5 ~ 'no'
      ),
      home_pred = factor(home_pred, levels = c("yes", "no"))
    ) |>
    select(any_of(c("season", "season_type", "season_week", "game_id", "home_team", "away_team", "pred_margin", "home_prob", "home_pred")))
}

calculate_game_quality <- function(data, groups = NULL, home_adjust = 3) {
  data |>
    mutate(across(c(home_overall, away_overall), normalize)) |>
    group_by(across(any_of(groups))) |>
    mutate(
      game_quality = harmonic_mean(home_overall+home_adjust, away_overall),
      game_quality = normalize(game_quality)
    ) |>
    ungroup()
}

harmonic_mean <- function(x, y) {
  2 / ((1 / x) + (1 / y))
}

normalize <- function(x) {
  (x - min(x)) / ((max(x) - min(x)))
}

split_by_season <- function(data, end_train_season, valid_seasons) {
  train <- data |>
    filter(season <= end_train_season)

  valid <- data |>
    filter(season > end_train_season & season <= end_train_season + valid_seasons)

  test <- data |>
    filter(season > end_train_season + valid_seasons)

  n_train <- nrow(train)
  n_val <- nrow(valid)
  train_id <- seq(1, n_train, by = 1)
  val_id <- seq(n_train + 1, n_train + n_val, by = 1)

  res <- list(data = data, train_id = train_id, val_id = val_id, test_id = NA, id = "split")
  class(res) <- c(
    "initial_validation_time_split", "initial_validation_split",
    "three_way_split"
  )
  res
}

game_formula <- function() {
  as.formula(home_margin ~ home_offense + home_defense + home_special + away_offense + away_defense + away_special + neutral_site)
}

build_games_wflow <- function(data,
                              model = linear_reg(engine = "stan"),
                              formula = game_formula(),
                              weights = T) {
  wflow <-
    workflow() |>
    add_model(
      model
    ) |>
    add_formula(
      formula
    )

  if (weights == T) {
    wflow <- wflow |>
      add_case_weights(weight)
  }

  wflow
}

calculate_team_scores <- function(model, data) {
  model |>
    augment(
      new_data =
        data |>
        mutate(season_week,
               season,
               season_type,
               team,
               home_offense = postgame_offense,
               home_defense = postgame_defense,
               home_special = postgame_special,
               away_offense = 0,
               away_defense = 0,
               away_special = 0,
               neutral_site = 1,
               .keep = "none"
        )
    ) |>
    add_season_week() |>
    select(
      season,
      season_type,
      season_week,
      team,
      score = .pred,
      offense = home_offense,
      defense = home_defense,
      special = home_special
    )
}

add_correct = function(data, pred = home_pred, actual = home_win) {

  data |>
    mutate(correct = case_when({{pred}} == {{actual}} ~ "yes",
                               {{pred}} != {{actual}} ~ "no"))

}

join_game_outcomes = function(data, games) {

  data |>
    left_join(
      games |>
        add_game_outcomes() |>
        select(game_id, home_margin, home_win, home_points, away_points),
      by = join_by(game_id)
    ) |>
    add_season_week() |>
    add_correct()

}

calculate_game_interest = function(data) {

  data |>
    mutate(
      game_compete  = -4 * (home_prob - 0.5)^2 + 1,
      game_interest = (0.4 * game_compete) + (0.6 * game_quality),
      game_interest = normalize(game_interest)
    )

}

add_team_scores <- function(data, teams_data, current_week, current_season) {
  completed_games <-
    data |>
    filter(completed == T)

  upcoming_games <-
    data |>
    filter(completed != T)

  active_teams_data <-
    teams_data |>
    group_by(team) |>
    slice_max(season_week, n = 1) |>
    ungroup()

  completed <-
    completed_games |>
    left_join(
      teams_data |>
        select(
          season,
          season_type,
          season_week,
          home = team,
          home_overall = score,
          home_offense = offense,
          home_defense = defense,
          home_special = special
        )
    ) |>
    left_join(
      teams_data |>
        select(
          season,
          season_type,
          season_week,
          away = team,
          away_overall = score,
          away_offense = offense,
          away_defense = defense,
          away_special = special
        )
    )

  upcoming <-
    upcoming_games |>
    left_join(
      active_teams_data |>
        select(
          season,
          home = team,
          home_overall = score,
          home_offense = offense,
          home_defense = defense,
          home_special = special
        )
    ) |>
    left_join(
      active_teams_data |>
        select(
          season,
          away = team,
          away_overall = score,
          away_offense = offense,
          away_defense = defense,
          away_special = special
        )
    )

  bind_rows(
    completed,
    upcoming
  )
}

prepare_game_predictions <- function(data) {
  data |>
    mutate(start_date = lubridate::as_datetime(start_date), tz = "UTC") |>
    select(
      season,
      season_type,
      start_date,
      week,
      game_id,
      home_team,
      away_team,
      pred_margin,
      home_margin,
      home_prob,
      home_pred,
      home_win,
      home_overall,
      away_overall
    ) |>
    mutate(
      correct = case_when(home_pred == home_win ~ "yes", home_pred != home_win ~ "no"),
      prediction = case_when(
        home_pred == "yes" ~ paste(home_team, pred_margin, sep = " by "),
        home_pred == "no" ~ paste(away_team, -pred_margin, sep = " by ")
      ),
      actual = case_when(
        home_win == "yes" ~ paste(home_team, home_margin, sep = " by "),
        home_win == "no" ~ paste(away_team, -home_margin, sep = " by ")
      )
    ) |>
    calculate_game_quality() |>
    calculate_game_interest() |>
    mutate(across(c("game_interest", "game_quality"), ~ 100 * .x)) |>
    select(game_id, season, season_type, week, start_date, game_quality, game_interest, home_team, away_team, home_prob, prediction, actual, correct) |>
    arrange(desc(week), desc(game_quality))
}

join_betting_lines = function(data, betting) {

  data |>
    left_join(
      betting |>
        select(game_id, provider, spread, spread_open) |>
        mutate(spread =
                 case_when(
                   is.na(spread) ~ as.numeric(spread_open),
                   TRUE ~ as.numeric(spread)
                 ),
               spread_open = as.numeric(spread_open),
               spread_margin = -spread
        )
    )
}

assess_games = function(data, groups = c('season', 'season_type', 'week')) {

  class_metrics = yardstick::metric_set(mn_log_loss, accuracy)
  reg_metrics = yardstick::metric_set(mae)

  # add number of games
  tmp =
    data |>
    add_correct() |>
    group_by(across(any_of(groups))) |>
    mutate(games = n_distinct(game_id)) |>
    group_by(across(any_of(groups)), games)

  class =
    tmp |>
    class_metrics(
      truth = home_win,
      estimate = home_pred,
      home_prob
    )

  reg =
    tmp |>
    reg_metrics(
      truth = home_margin,
      estimate = pred_margin
    )

  record =
    tmp |>
    filter(!is.na(correct)) |>
    group_by(across(any_of(groups)), games, correct) |>
    count() |>
    pivot_wider(
      names_from = c(correct),
      values_from = n
    ) |>
    ungroup() |>
    mutate(record = paste(yes, no, sep = "-")) |>
    select(any_of(groups), record)


  metrics =
    bind_rows(class,
              reg) |>
    select(any_of(groups), games, .metric, .estimate) |>
    mutate_if(is.numeric, round, 3) |>
    pivot_estimates()

  metrics |>
    left_join(record)
}

prepare_games_for_prediction = function(data, estimates) {

  prepare_games_internal = function(data) {
    data |>
      prepare_game_info() |>
      add_game_weeks() |>
      prepare_fcs_teams() |>
      select(season,
             season_type,
             season_week,
             week,
             week_date,
             start_date,
             game_id,
             completed,
             neutral_site,
             home_id,
             away_id,
             home_team,
             away_team,
             home,
             away)
  }


  games =
    data |>
    prepare_games_internal()

  games |>
    join_team_estimates(estimates = estimates)

}

join_team_estimates <- function(data, estimates) {

  join_estimates_completed = function(data, estimates) {

    data |>
      inner_join(
        estimates |>
          select(
            season,
            season_type,
            season_week,
            home = team,
            home_overall = pregame_overall,
            home_offense = pregame_offense,
            home_defense = pregame_defense,
            home_special = pregame_special
          ),
        by = join_by(season, season_week, season_type, home)
      ) |>
      inner_join(
        estimates |>
          select(
            season,
            season_type,
            season_week,
            away = team,
            away_overall = pregame_overall,
            away_offense = pregame_offense,
            away_defense = pregame_defense,
            away_special = pregame_special
          ),
        by = join_by(season, season_week, season_type, away)
      )

  }

  join_estimates_upcoming = function(data, estimates) {

    data |>
      inner_join(
        active_team_estimates |>
          select(
            season,
            home = team,
            home_overall = overall,
            home_offense = offense,
            home_defense = defense,
            home_special = special
          )
      ) |>
      inner_join(
        active_team_estimates |>
          select(
            season,
            away = team,
            away_overall = overall,
            away_offense = offense,
            away_defense = defense,
            away_special = special
          )
      )
  }

  # for games that have been completed
  completed_with_estimates =
    data |>
    join_estimates_completed(estimates = estimates)

  # filter to games missing estimates; add the most recently observed team estimates for these
  upcoming =
    data |>
    anti_join(completed_with_estimates |> select(game_id),
              by = join_by(game_id))

  # what is the first game that is missing an estimate?
  min_date =
    upcoming |>
    summarize(min_date = min(start_date)) |>
    pull(min_date)

  # join in team estimates
  active_team_estimates =
    estimates |>
    filter(week_date <= min_date) |>
    group_by(team) |>
    slice_max(week_date, n =1) |>
    ungroup() |>
    select(
      season,
      week_date,
      team,
      overall = postgame_overall,
      offense = postgame_offense,
      defense = postgame_defense,
      special = postgame_special
    )

  upcoming_with_estimates =
    upcoming |>
    join_estimates_upcoming(estimates = estimates)

  bind_rows(
    completed_with_estimates,
    upcoming_with_estimates
  )

}
