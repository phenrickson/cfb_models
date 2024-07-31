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

# static branching over seasons
seasons = data.frame(season = 2000:2023)

# Replace the target list below with your own:
cfbd = 
    list(
        ### cfbd data
        # calendars
        tar_target(
            cfbd_calendars_tbl,
            map_df(seasons$season,
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
            map_df(seasons$season,
                   ~ cfbd_team_info(only_fbs = T,
                                    year = .x) |>
                       as_tibble() |>
                       mutate(season = .x)) |>
                select(season, everything())
        ),
        # talent
        tar_target(
            cfbd_team_talent_tbl,
            map_df(seasons$season,
                   ~ cfbd_team_talent(year = .x)) |>
                as_tibble()
        ),
        # team recruiting,
        tar_target(
            cfbd_recruiting_team,
            map_df(seasons$season,
                   ~ cfbd_recruiting_team(year = .x)) |>
                as_tibble()
        ),
        # games
        tar_target(
            cfbd_game_info_tbl,
            map_df(seasons$season,
                   ~ cfbd_game_info(year = .x,
                                    season_type = "both")) |>
                as_tibble()
        ),
        # betting lines
        tar_target(
            cfbd_betting_lines_tbl,
            {
                tmp = expand_grid(season = seasons$season, 
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
                tmp = expand_grid(season = seasons$season, 
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
            map_df(seasons$season,
                   ~ cfbd_draft_picks(year = .x))
        ),
        # play types
        tar_target(
            cfbd_play_types_tbl,
            cfbd_play_types()
        )
    )

# # dynamic branch over games, weeks, seasons
# dynamic = 
#     list(
#         tar_target(
#             cfbd_season_week_games,
#             cfbd_calendars_tbl |>
#                 select(season, week, season_type) |>
#                 distinct() |>
#                 group_by(season, week, season_type) |>
#                 tar_group(),
#             iteration = "group"
#         ),
#         tar_target(
#             cfbd_game_player_stats_tbl,
#             get_game_player_stats(cfbd_season_week_games),
#             pattern = map(cfbd_season_week_games)
#         )
#     )

# static branch over seasons
static =
    # static branch over seasons
    tar_map(
        values = seasons,
        # drives
        tar_target(
            cfbd_drives_by_season,
            cfbd_drives(year = season,
                        season_type = 'both') |>
                add_season(year = season)
        ),
        # plays
        tar_target(
            cfbd_plays_by_season,
            cfbd_plays(season_type = 'both') |>
                add_season(year = season) 
        ),
        # coaches
        tar_target(
            cfbd_coaches_by_season,
            cfbd_coaches(year = season) |>
                add_season(year = season)
        ),
        # rosters
        tar_target(
            cfbd_team_roster_by_season,
            cfbd_team_roster(year = season) |>
                add_season(year = season)
        ),
        # recruits
        tar_target(
            cfbd_recruiting_player_by_season,
            cfbd_recruiting_player(year = season) |>
                add_season(year = season)
        )
    )

# combine results from static branches
combined = 
    list(
        tar_combine(
            cfbd_drives_tbl,
            static[["cfbd_drives_by_season"]],
            command = dplyr::bind_rows(!!!.x)
        ),
        tar_combine(
            cfbd_plays_tbl,
            static[["cfbd_plays_by_season"]],
            command = dplyr::bind_rows(!!!.x)
        ),
        tar_combine(
            cfbd_coaches_tbl,
            static[["cfbd_coaches_by_season"]],
            command = dplyr::bind_rows(!!!.x)
        ),
        tar_combine(
            cfbd_team_roster_tbl,
            static[["cfbd_team_roster_by_season"]],
            command = dplyr::bind_rows(!!!.x)
        ),
        tar_combine(
            cfbd_recruiting_player_tbl,
            static[["cfbd_recruiting_player_by_season"]],
            command = dplyr::bind_rows(!!!.x)
        )
    )

# return results
list(
    cfbd,
    static,
    combined
)