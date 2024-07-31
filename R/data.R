add_season = function(data, year) {
    
    tbl = 
        data |>
        as_tibble()
    
    tbl$season = year
    
    tbl |>
        select(season, everything())
}

get_game_player_stats = function(data) {
    
    season = data$season
    week = data$week
    season_type = data$season_type
    
    cfbd_game_player_stats(year = season,
                           week = week,
                           season_type = season_type)
}