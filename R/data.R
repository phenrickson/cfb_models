# function to add season to data
add_season <- function(data, year) {
  tbl <-
    data |>
    as_tibble()

  tbl$season <- year

  tbl |>
    select(season, everything())
}

get_cfbd_plays <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_plays(
    year = season,
    week = week,
    season_type = season_type
  )
}

get_cfbd_pbp_data <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_pbp_data(
    year = as.numeric(season),
    week = week,
    season_type = season_type
  )
}

get_game_player_stats <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_game_player_stats(
    year = season,
    week = week,
    season_type = season_type
  )
}

clean_plays_func <-
  function(data) {
    data |>
      mutate(ID = as.numeric(ID)) %>%
      group_by(GAME_ID) %>%
      arrange(ID) %>%
      # clean up PERIOD by game
      mutate(STATUS_PERIOD = case_when(
        PERIOD == 0 & nchar(PLAY_TEXT) <= 1 ~ "Drop",
        PERIOD == 0 ~ "Take Previous Value",
        TRUE ~ "No Change"
      )) %>%
      filter(STATUS_PERIOD != "Drop") %>%
      mutate(PERIOD = case_when(
        STATUS_PERIOD == "Take Previous Value" ~ lag(PERIOD, 1),
        TRUE ~ PERIOD
      )) %>%
      ungroup() %>%
      # then clean up the down
      mutate(STATUS_DOWN = case_when(
        DOWN %in% c(1, 2, 3, 4) ~ "Regular Down",
        TRUE ~ "Change to Special Teams"
      )) %>%
      mutate(DOWN = case_when(
        DOWN %in% c(1, 2, 3, 4) ~ DOWN,
        TRUE ~ -1
      )) %>%
      # flag yard lines that are outside correct
      mutate(STATUS_YARD_LINE = case_when(
        YARD_LINE < 0 | YARD_LINE > 100 ~ "Invalid",
        TRUE ~ "Valid"
      ))
  }

make_time_features_func <-
  function(x) {
    x %>%
      mutate(TIME = gsub('\\{|}|"', "", CLOCK)) %>%
      separate(TIME, into = c("MINUTES", "SECOND"), sep = ",") %>%
      mutate(MINUTES = as.numeric(gsub("minutes:", "", MINUTES))) %>%
      mutate(SECOND = as.numeric(gsub("seconds:", "", SECOND))) %>%
      mutate(MINUTES_IN_HALF = case_when(
        PERIOD == 1 ~ MINUTES + 15,
        PERIOD == 2 ~ MINUTES,
        PERIOD == 3 ~ MINUTES + 15,
        PERIOD == 4 ~ MINUTES
      )) %>%
      mutate(SECONDS_IN_HALF = MINUTES_IN_HALF * 60 + SECOND)
    #    mutate(SECONDS_IN_HALF = )
  }
