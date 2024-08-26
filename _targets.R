# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)

# authenticate
#googleCloudStorageR::gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
googleAuthR::gar_auth_service(
    json_file = gargle::secret_decrypt_json(path = ".secrets", key = "GARGLE_KEY"),
    scope =  c("https://www.googleapis.com/auth/devstorage.full_control",
                "https://www.googleapis.com/auth/cloud-platform")
)

# set default bucket
suppressMessages({
  googleCloudStorageR::gcs_global_bucket(bucket = "cfb_models")
})

# Set target options:
tar_option_set(
  packages = c(
    "cfbfastR",
    "dplyr",
    "tidyr",
    "purrr"
  ),
  format = "qs",
  memory = "transient",
  resources =
  tar_resources(
    gcp = tar_resources_gcp(
      bucket = "cfb_models",
      predefined_acl = "bucketLevel",
      prefix = "data"
    )
  ),
  controller =
  crew_controller_local(workers = 7),
  repository = "gcp",
  storage = "worker",
  retrieval = "worker"
)

# Run the R scripts in the R/ folder with your custom functions:
suppressMessages({tar_source("R")})
# tar_source("other_functions.R") # Source other scripts as needed.

# running over seasons
seasons <- 2000:2023

# Replace the target list below with your own:
list(
  ### cfbd data
  # load all games
  tar_target(
    cfbd_games_tbl,
    map_df(
      1869:cfbfastR:::most_recent_cfb_season(),
      ~ cfbd_game_info(
        year = .x,
        season_type = "both",
        division = "fbs"
      ) |>
      as_tibble()
    )
  ),
  # calendars
  tar_target(
    cfbd_calendar_tbl,
    map_df(
      seasons,
      ~ cfbd_calendar(year = .x)
    ) |>
    as_tibble()
  ),
  # conferences
  tar_target(
    cfbd_conferences_tbl,
    cfbd_conferences() |>
    as_tibble()
  ),
  # fbs team info
  tar_target(
    cfbd_team_info_tbl,
    map_df(
      seasons,
      ~ cfbd_team_info(
        only_fbs = T,
        year = .x
      ) |>
      as_tibble() |>
      mutate(season = .x)
    ) |>
    select(season, everything())
  ),
  # talent
  tar_target(
    cfbd_team_talent_tbl,
    map_df(
      seasons,
      ~ cfbd_team_talent(year = .x)
    ) |>
    as_tibble()
  ),
  # team recruiting,
  tar_target(
    cfbd_recruiting_team,
    map_df(
      seasons,
      ~ cfbd_recruiting_team(year = .x)
    ) |>
    as_tibble()
  ),
  # games for selected seasons
  tar_target(
    cfbd_game_info_tbl,
    map_df(
      seasons,
      ~ cfbd_game_info(
        year = .x,
        season_type = "both"
      )
    ) |>
    as_tibble()
  ),
  # betting lines
  tar_target(
    cfbd_betting_lines_tbl,
    {
      tmp <- expand_grid(
        season = seasons,
        type = c("regular", "postseason")
      )

      map2_df(
        .x = tmp$season,
        .y = tmp$type,
        ~ cfbd_betting_lines(
          year = .x,
          season_type = .y
        )
      )
    }
  ),
  # rankings
  tar_target(
    cfbd_game_rankings_tbl,
    {
      tmp <- expand_grid(
        season = seasons,
        type = c("regular", "postseason")
      )

      map2_df(
        .x = tmp$season,
        .y = tmp$type,
        ~ cfbd_rankings(
          year = .x,
          season_type = .y
        )
      )
    }
  ),
  # draft picks
  tar_target(
    cfbd_draft_picks,
    map_df(
      seasons,
      ~ cfbd_draft_picks(year = .x)
    ) |>
    as_tibble()
  ),
  # play types
  tar_target(
    cfbd_play_types_tbl,
    cfbd_play_types() |>
    as_tibble()
  ),
  # coaches
  tar_target(
    cfbd_coaches_tbl,
    map_df(
      seasons,
      ~ cfbd_coaches(year = .x) |>
      add_season(year = .x)
    )
  ),
  # rosters
  tar_target(
    cfbd_team_roster_tbl,
    map_df(
      seasons,
      ~ cfbd_team_roster(year = .x) |>
      add_season(year = .x)
    )
  ),
  # recruiting_player
  tar_target(
    cfbd_recruiting_player_tbl,
    map_df(
      seasons,
      ~ cfbd_recruiting_player(year = .x) |>
      add_season(year = .x)
    )
  ),
  # recruiting position
  tar_target(
    cfbd_recruiting_position_tbl,
    map_df(
      seasons,
      ~ cfbd_recruiting_position(
        start_year = .x,
        end_year = .x
      ) |>
      add_season(year = .x)
    )
  ),
  # player usage
  tar_target(
    cfbd_player_usage_tbl,
    map_df(
      seasons[seasons > 2012],
      ~ cfbd_player_usage(year = .x) |>
      as_tibble()
    )
  ),
  # player returning
  tar_target(
    cfbd_player_returning_tbl,
    map_df(
      seasons,
      ~ cfbd_player_returning(year = .x) |>
      as_tibble()
    )
  ),
  # drives
  tar_target(
    cfbd_drives_tbl,
    map_df(
      seasons,
      ~ cfbd_drives(
        year = .x,
        season_type = "both"
      ) |>
      add_season(year = .x)
    )
  ),
  ### now get espn data
  # calendar
  tar_target(
    espn_cfb_calendar_tbl,
    map_df(
      seasons,
      ~ espn_cfb_calendar(year = .x) |>
      as_tibble()
    )
  ),
  # schedule
  tar_target(
    espn_cfb_schedule_tbl,
    map_df(
      seasons,
      ~ espn_cfb_schedule(year = .x) |>
      as_tibble()
    )
  ),
  # espn games
  tar_target(
    espn_cfb_game_ids,
    espn_cfb_schedule_tbl |>
    # seasons with pbp data
    filter(season > 2002) |>
    filter(play_by_play_available == T) |>
    distinct(game_id) |>
    pull()
  ),
  # espn fpi
  tar_target(
    espn_ratings_fpi_tbl,
    map_df(
      seasons[seasons > 2004],
      ~ espn_ratings_fpi(year = .x) |>
      as_tibble()
    )
  ),
  # dynamic branch over seasons, weeks, and season type to get play by play
  tar_target(
    cfbd_season_week_games,
    cfbd_game_info_tbl |>
    select(season, week, season_type) |>
    distinct() |>
    filter(season_type %in% c("regular", "postseason")) |>
    group_by(season, week, season_type) |>
    tar_group(),
    iteration = "group"
  ),
  # get cfbd plays for each branch
  tar_target(
    cfbd_plays_tbl,
    get_cfbd_plays(cfbd_season_week_games),
    pattern = map(cfbd_season_week_games),
    error = "null"
  ),
  # get cleaned cfbd pbp (cfbfastR) for each branch
  tar_target(
    cfbd_pbp_data_tbl,
    get_cfbd_pbp_data(cfbd_season_week_games),
    pattern = map(cfbd_season_week_games),
    error = "null"
  ),
  # filter to only relevant
  tar_target(
    filtered_pbp,
    cfbd_pbp_data_tbl |>
    # filter to only games with both fbs divisions
    inner_join(
      cfbd_game_info_tbl |>
      filter(home_division == "fbs" | away_division == "fbs")
    ) |>
    # filter to games after 2005
    filter(season > 2005)
  ),
  # prepare pbp data using custom functions
  tar_target(
    prepared_pbp,
    filtered_pbp |>
    filter_plays() |>
    prepare_pbp() |>
    add_score_events(),
    deployment = "main"
  ),
  # prepare games for use in elo functions
  tar_target(
    prepared_games,
    cfbd_games_tbl |>
    prepare_games()
  ),
  # elo parameters
  tar_target(
    elo_params,
    expand.grid(
      reversion = c(0, 0.1, 0.2),
      k = c(25, 35, 45),
      v = 400,
      home_field_advantage = c(25, 75, 100)
    )
  ),
  tar_target(
    elo_tuning_results,
    prepared_games |>
    tune_elo_ratings(params = elo_params),
    pattern = map(elo_params),
    iteration = "vector",
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    elo_metrics,
    elo_tuning_results |>
    select(game_outcomes, settings) |>
    # prioritize games since 2000
    mutate(game_outcomes = map(game_outcomes, ~ .x |> filter(season >= 2000))) |>
    assess_elo_ratings()
  ),
  tar_target(
    elo_best_params,
    elo_metrics |>
    select(overall) |>
    unnest() |>
    select_elo_params(),
    packages = c("desirability2")
  ),
  tar_target(
    elo_games,
    prepared_games |>
    tune_elo_ratings(params = elo_best_params)
  ),
  # quarto
  tar_quarto(
    reports,
    quiet = F
  )
)
