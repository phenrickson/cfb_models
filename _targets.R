# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
    packages = c("cfbfastR",
                 "dplyr",
                 "tidyr",
                 "purrr"),
    format = "qs",
    memory = "transient"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")
# tar_source("other_functions.R") # Source other scripts as needed.

# running over seasons
seasons = 2000:2023

# Replace the target list below with your own:
list(
    ### cfbd data
    # calendars
    tar_target(
        cfbd_calendar_tbl,
        map_df(seasons,
               ~ cfbd_calendar(year = .x)) |>
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
        map_df(seasons,
               ~ cfbd_team_info(only_fbs = T,
                                year = .x) |>
                   as_tibble() |>
                   mutate(season = .x)) |>
            select(season, everything())
    ),
    # talent
    tar_target(
        cfbd_team_talent_tbl,
        map_df(seasons,
               ~ cfbd_team_talent(year = .x)) |>
            as_tibble()
    ),
    # team recruiting,
    tar_target(
        cfbd_recruiting_team,
        map_df(seasons,
               ~ cfbd_recruiting_team(year = .x)) |>
            as_tibble()
    ),
    # games
    tar_target(
        cfbd_game_info_tbl,
        map_df(seasons,
               ~ cfbd_game_info(year = .x,
                                season_type = "both")) |>
            as_tibble()
    ),
    # betting lines
    tar_target(
        cfbd_betting_lines_tbl,
        {
            tmp = expand_grid(season = seasons, 
                              type = c("regular", "postseason"))
            
            map2_df(.x = tmp$season,
                    .y = tmp$type,
                    ~ cfbd_betting_lines(year = .x,
                                         season_type = .y)
            )
        }
    ),
    # rankings
    tar_target(
        cfbd_game_rankings_tbl,
        {
            tmp = expand_grid(season = seasons, 
                              type = c("regular", "postseason"))
            
            map2_df(.x = tmp$season,
                    .y = tmp$type,
                    ~ cfbd_rankings(year = .x,
                                    season_type = .y)
            )
        }
    ),
    # draft picks
    tar_target(
        cfbd_draft_picks,
        map_df(seasons,
               ~ cfbd_draft_picks(year = .x)) |>
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
        map_df(seasons,
               ~ cfbd_coaches(year = .x) |>
                   add_season(year = .x))
    ),
    # rosters
    tar_target(
        cfbd_team_roster_tbl,
        map_df(seasons,
               ~  cfbd_team_roster(year = .x) |>
                   add_season(year = .x))
    ),
    # recruiting_player
    tar_target(
        cfbd_recruiting_player_tbl,
        map_df(seasons,
               ~ cfbd_recruiting_player(year = .x) |>
                   add_season(year = .x))
    ),
    # recruiting position
    tar_target(
        cfbd_recruiting_position_tbl,
        map_df(seasons,
               ~ cfbd_recruiting_position(start_year = .x,
                                          end_year = .x) |>
                   add_season(year = .x))
    ),
    # player usage
    tar_target(
        cfbd_player_usage_tbl,
        map_df(
            seasons[seasons>2012],
            ~ cfbd_player_usage(year =.x) |>
                as_tibble()
        )
    ),
    # player returning
    tar_target(
        cfbd_player_returning_tbl,
        map_df(seasons,
               ~ cfbd_player_returning(year = .x) |>
                   as_tibble()
        )
    ),
    # drives
    tar_target(
        cfbd_drives_tbl,
        map_df(seasons,
               ~ cfbd_drives(year = .x, 
                             season_type = 'both') |>
                   add_season(year = .x))
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
    # get cleaned cfbd pbp (cfbfastR) for each branchi
    tar_target(
        cfbd_pbp_data_tbl,
        get_cfbd_pbp_data(cfbd_season_week_games),
        pattern = map(cfbd_season_week_games),
        error = "null"
    )
)