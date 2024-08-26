seasons <- 2002:2004
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/data/parquet/play_by_play_{x}.parquet"), "tmp.parquet")
  # df <- arrow::read_parquet("tmp.parquet")
  return(df)
})

# https://github.com/sportsdataverse/cfbfastR-data/tree/main/pbp/parquet

seasons <- 2002:2020
pbp <- purrr::map_df(seasons, function(x) {
  readr::read_csv(
    url(
      glue::glue("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/pbp/csv/play_by_play_{x}.csv.gz")
    )
  )
})

seasons <- 2002:2008
pbp <- purrr::map(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/pbp/parquet/play_by_play_{x}.parquet"), "tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
})

foo <-
  purrr::map(
    2007,
    ~ cfbfastR::load_cfb_pbp(.x)
  )

seasons <- 2007:2024

conferences <-
  cfbfastR::cfbd_conferences()

calendars <-
  purrr::map_df(
    seasons,
    ~ cfbfastR::espn_cfb_calendar(year = .x)
  )

schedules <-
  purrr::map_df(
    seasons,
    ~ cfbfastR::espn_cfb_schedule(year = .x)
  )
