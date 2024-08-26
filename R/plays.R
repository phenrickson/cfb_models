prepare_drives <- function(data) {
  data |>
  rename(
    drive_pts = pts_drive
  ) |>
  rename_with(
    .cols = all_of(
      c(
        "scoring",
        "start_period",
        "start_yardline",
        "start_yards_to_goal",
        "end_period",
        "end_yardline",
        "end_yards_to_goal",
        "plays",
        "yards",
        "is_home_offense",
        "start_offense_score",
        "start_defense_score",
        "end_offense_score",
        "end_defense_score",
        "time_minutes_start",
        "time_seconds_start",
        "time_minutes_end",
        "time_seconds_end",
        "time_seconds_elapsed",
        "time_minutes_elapsed"
      )
    ),
    .fn = ~ paste0("drive_", .x)
  ) |>
  select(
    season,
    game_id,
    starts_with("drive_")
  )
}

prepare_plays <- function(data) {
  data |>
  mutate(
    half = case_when(
      period %in% c(1, 2) ~ 1,
      period %in% c(3, 4) ~ 1
    )
  ) |>
  select(
    play_id,
    game_id,
    drive_id,
    offense,
    defense,
    home,
    away,
    offense_score,
    defense_score,
    drive_number,
    play_number,
    period,
    half,
    offense_timeouts,
    defense_timeouts,
    yard_line,
    yards_to_goal,
    down,
    distance,
    scoring,
    yards_gained,
    play_type,
    play_text,
    wallclock,
    clock.minutes,
    clock.seconds
  ) |>
  mutate(
    across(
      c(
        "play_id",
        "game_id",
        "drive_id"
      ),
      as.numeric
    )
  )
}

add_play_time <- function(data) {
  data |>
  mutate(
    half = case_when(
      period %in% c(1, 2) ~ 1,
      period %in% c(3, 4) ~ 2
    ),
    clock_seconds = case_when(
      clock.minutes > 15 & clock.seconds == 0 ~ clock.minutes,
      clock.minutes == 15 & clock.seconds > 0 ~ 0,
      TRUE ~ clock.seconds
    ),
    clock_minutes = case_when(
      clock.minutes > 15 ~ 0,
      TRUE ~ clock.minutes
    ),
    seconds_in_half = case_when(
      period == 1 | period == 3 ~ ((15 + clock_minutes) * 60) + clock_seconds,
      period == 2 | period == 4 ~ (clock_minutes * 60) + clock_seconds
    )
  )
}

clean_yards <- function(data) {
  clean_yard_line <- function(data) {
    data |>
    mutate(
      yard_line_str = as.character(yard_line),
      yard_line = case_when(
        yard_line > 100 ~ as.numeric(substr(yard_line_str, 2, nchar(yard_line_str))),
        TRUE ~ yard_line
      ),
    ) |>
    select(-yard_line_str)
  }

  # recalculate yards_to_goal
  clean_yards_to_goal <- function(data) {
    data |>
    mutate(yards_to_goal = case_when(
      offense == home ~ 100 - yard_line,
      offense != home ~ yard_line
    ))
  }

  data |>
  clean_yard_line() |>
  clean_yards_to_goal()
}

clean_timeouts <- function(data) {
  data |>
  mutate(
    across(c(offense_timeouts, defense_timeouts), ~ case_when(.x < 0 ~ 0, TRUE ~ .x))
  )
}

clean_distance <- function(data) {
  data |>
  mutate(
    distance = case_when(
      distance <= 0 & down == 1 ~ 10,
      TRUE ~ distance
    )
  ) |>
  group_by(game_id, drive_id) |>
  mutate(
    distance = case_when(
      distance <= 0 & down == 1 ~ 10,
      distance <= 0 | distance > 100 ~ lag(distance, 1),
      TRUE ~ distance
    )
  ) |>
  ungroup()
}

filter_play_type <- function(data) {
  data |>
  filter(!(play_type %in% c("End of Half", "End of Period", "Timeout"))) |>
  filter(!(grepl("Kickoff", play_type)))
}

filter_plays <- function(data) {
  data |>
  filter(down %in% c(1, 2, 3, 4)) |>
  filter(period %in% c(1, 2, 3, 4))
}

# implement pbp functions
prepare_pbp <- function(data) {
  data |>
  add_play_time() |>
  filter_play_type() |>
  select(
    season,
    play_id = id_play,
    game_id,
    offense = offense_play,
    defense = defense_play,
    home,
    away,
    offense_score,
    defense_score,
    drive_id,
    drive_number,
    play_number,
    half,
    period,
    wallclock,
    clock_minutes,
    clock_seconds,
    seconds_in_half,
    offense_timeouts,
    defense_timeouts,
    yard_line,
    yards_to_goal,
    down,
    distance,
    scoring,
    play_type,
    play_text,
    any_of(c("drive_result", "drive_is_home_offense"))
  ) |>
  clean_yards() |>
  clean_distance() |>
  clean_timeouts() |>
    # ad hoc clean up of specific plays
  mutate(distance = case_when(
    play_id == "303030030315" & distance < 0 ~ 3,
    TRUE ~ distance)
  )
}

penalty_text <- function() {
  c(
    "roughing passer", "offensive holding", "pass interference", "encroachment",
    "defensive pass interference", "offensive pass interference", "illegal procedure",
    "defensive holding", "holding", "offensive offside|offside offense",
    "defensive offside|offside defense", "offside", "illegal fair catch signal",
    "illegal batting", "neutral zone infraction", "ineligible downfield",
    "illegal use of hands", "kickoff out of bounds|kickoff out-of-bounds",
    "12 men on the field", "illegal block", "personal foul", "false start",
    "substitution infraction", "illegal formation", "illegal touching",
    "sideline interference", "clipping", "sideline infraction", "crackback",
    "illegal snap", "illegal helmet contact", "roughing holder", "horse collar tackle",
    "illegal participation", "tripping", "illegal shift", "illegal motion",
    "roughing the kicker", "delay of game", "targeting", "face mask",
    "illegal forward pass", "intentional grounding", "illegal kicking",
    "illegal conduct", "kick catching interference", "unnecessary roughness",
    "unsportsmanlike conduct", "running into kicker",
    "failure to wear required equipment", "player disqualification"
  )
}

end_text <- function() {
  c(
    "End of Half",
    "End of 2nd Quarter"
  )
}

penalty_details_text <- function() {
  c(
    "Roughing the Passer", "Offensive Holding", "Pass Interference", "Encroachment",
    "Defensive Pass Interference", "Offensive Pass Interference", "Illegal Procedure",
    "Defensive Holding", "Holding", "Offensive Offside", "Defensive Offside",
    "Offside", "Illegal Fair Catch Signal", "Illegal Batting", "Neutral Zone Infraction",
    "Ineligible Man Down-Field", "Illegal Use of Hands", "Kickoff Out-of-Bounds",
    "12 Men on the Field", "Illegal Block", "Personal Foul", "False Start",
    "Substitution Infraction", "Illegal Formation", "Illegal Touching",
    "Sideline Interference", "Clipping", "Sideline Infraction", "Crackback",
    "Illegal Snap", "Illegal Helmet Contact", "Roughing the Holder", "Horse-Collar Tackle",
    "Illegal Participation", "Tripping", "Illegal Shift", "Illegal Motion",
    "Roughing the Kicker", "Delay of Game", "Targeting", "Face Mask",
    "Illegal Forward Pass", "Intentional Grounding", "Illegal Kicking",
    "Illegal Conduct", "Kick Catch Interference", "Unnecessary Roughness",
    "Unsportsmanlike Conduct", "Running Into Kicker",
    "Failure to Wear Required Equipment", "Player Disqualification"
  )
}

score_text <- function() {
  c(
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
    "Punt (Safety)",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown",
    "Defensive 2pt Conversion",
    "Uncategorized",
    "Two Point Rush",
    "Safety",
    "Penalty (Safety)",
    "Punt Team Fumble Recovery Touchdown",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown"
  )
}

offense_score_text <- function() {
  c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown", #<--- Punting Team recovers the return team fumble and scores
    "Punt Team Fumble Recovery Touchdown",
    "Kickoff Touchdown", #<--- Kickoff Team recovers the return team fumble and scores
    "Kickoff Team Fumble Recovery Touchdown"
  )
}

defense_score_text <- function() {
  c(
    "Blocked Punt Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Kickoff Return Touchdown",
    "Defensive 2pt Conversion",
    "Safety",
    "Sack Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
  )
}

turnover_text <- function() {
  c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )
}

normalplay_text <- function() {
  c(
    "Rush",
    "Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Pass Completion",
    "Sack",
    "Fumble Recovery (Own)"
  )
}

penalty_text <- function() {
  c(
    "Penalty",
    "Penalty (Kickoff)",
    "Penalty (Safety)"
  )
}

punt_text <- function() {
  c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
    "Punt (Safety)",
    "Punt",
    "Punt Touchdown",
    "Punt Team Fumble Recovery",
    "Punt Team Fumble Recovery Touchdown",
    "Punt Return Touchdown"
  )
}

kickoff_text <- function() {
  c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Penalty (Kickoff)"
  )
}

int_text <- function() {
  c(
    "Interception",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception",
    "Pass Interception Return",
    "Pass Interception Return Touchdown"
  )
}

add_scoring_plays <- function(data) {
  data |>
  dplyr::mutate(
    #-- Touchdowns----
    scoring_play = ifelse(.data$play_type %in% score_text(), 1, 0),
    td_play = ifelse(stringr::str_detect(.data$play_text, regex("touchdown|for a TD", ignore_case = TRUE)) &
    !is.na(.data$play_text), 1, 0),
    touchdown = ifelse(stringr::str_detect(.data$play_type, regex("touchdown", ignore_case = TRUE)), 1, 0),
    safety = ifelse(stringr::str_detect(.data$play_text, regex("safety", ignore_case = TRUE)), 1, 0)
  )
}

add_rush_plays <- function(data) {
  data |>
  mutate(
    rush = ifelse(
      (.data$play_type == "Rush" & !is.na(.data$play_text)) |
      .data$play_type == "Rushing Touchdown" |
      (.data$play_type == "Safety" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
      (.data$play_type == "Fumble Recovery (Opponent)" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
      (.data$play_type == "Fumble Recovery (Opponent) Touchdown" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
      (.data$play_type == "Fumble Recovery (Own)" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
      (.data$play_type == "Fumble Recovery (Own) Touchdown" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)) |
      (.data$play_type == "Fumble Return Touchdown" &
      stringr::str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) & !is.na(.data$play_text)), 1, 0
    )
  )
}

add_pass_plays <- function(data) {
  data |>
  mutate(
    pass = if_else(
      .data$play_type == "Pass Reception" |
      .data$play_type == "Pass Completion" |
      .data$play_type == "Passing Touchdown" |
      .data$play_type == "Sack" |
      .data$play_type == "Pass" |
      .data$play_type == "Interception" |
      .data$play_type == "Pass Interception Return" |
      .data$play_type == "Interception Return Touchdown" |
      (.data$play_type == "Pass Incompletion" & !is.na(.data$play_text)) |
      .data$play_type == "Sack Touchdown" |
      (.data$play_type == "Safety" &
      stringr::str_detect(.data$play_text, regex("sacked",
      ignore_case = TRUE
    )) & !is.na(.data$play_text)) |
    (.data$play_type == "Safety" &
    stringr::str_detect(.data$play_text, regex("pass complete",
    ignore_case = TRUE
  )) & !is.na(.data$play_text)) |
  (.data$play_type == "Fumble Recovery (Own)" &
  stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted",
  ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Recovery (Own)" &
stringr::str_detect(.data$play_text, regex("sacked",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Recovery (Own) Touchdown" &
stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Recovery (Opponent)" &
stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete|pass intercepted",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Recovery (Opponent)" &
stringr::str_detect(.data$play_text, regex("sacked",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Recovery (Opponent) Touchdown" &
stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Return Touchdown" &
stringr::str_detect(.data$play_text, regex("pass complete|pass incomplete",
ignore_case = TRUE
)) & !is.na(.data$play_text)) |
(.data$play_type == "Fumble Return Touchdown" &
stringr::str_detect(.data$play_text, regex("sacked",
ignore_case = TRUE
)) & !is.na(.data$play_text)), 1, 0
),
int = ifelse(.data$play_type %in% c("Interception Return", "Interception Return Touchdown"), 1, 0),
int_td = ifelse(.data$play_type %in% c("Interception Return Touchdown"), 1, 0),
pass_completion = ifelse(.data$play_type %in% c("Pass Reception", "Pass Completion", "Passing Touchdown") |
((.data$play_type %in% c(
  "Fumble Recovery (Own)",
  "Fumble Recovery (Own) Touchdown",
  "Fumble Recovery (Opponent)",
  "Fumble Recovery (Opponent) Touchdown"
) & .data$pass == 1 &
!stringr::str_detect(.data$play_text, "sacked"))), 1, 0),
pass_attempt = ifelse(.data$play_type %in% c(
  "Pass Reception",
  "Pass Completion",
  "Passing Touchdown",
  "Pass Incompletion",
  "Interception Return",
  "Interception Return Touchdown"
) |
((.data$play_type %in% c(
  "Fumble Recovery (Own)",
  "Fumble Recovery (Own) Touchdown",
  "Fumble Recovery (Opponent)",
  "Fumble Recovery (Opponent) Touchdown"
) & .data$pass == 1 &
!stringr::str_detect(.data$play_text, "sacked"))), 1, 0),
pass_target = ifelse(.data$play_type %in% c(
  "Pass Reception",
  "Pass Completion",
  "Passing Touchdown",
  "Pass Incompletion"
) |
((.data$play_type %in% c(
  "Fumble Recovery (Own)",
  "Fumble Recovery (Own) Touchdown",
  "Fumble Recovery (Opponent)",
  "Fumble Recovery (Opponent) Touchdown"
) & .data$pass == 1 &
!stringr::str_detect(.data$play_text, "sacked"))), 1, 0)
)
}

add_kick_plays <- function(data) {
  data |>
  mutate(
    kickoff = ifelse(.data$play_type %in% kickoff_text(), 1, 0),
    kickoff_tb = ifelse(stringr::str_detect(.data$play_text, regex("touchback", ignore_case = TRUE)) &
    (.data$kickoff == 1) & !is.na(.data$play_text), 1, 0),
    kickoff_onside = ifelse(stringr::str_detect(.data$play_text, regex("on-side|onside|on side", ignore_case = TRUE)) &
    (.data$kickoff == 1) & !is.na(.data$play_text), 1, 0),
    kickoff_oob = ifelse(stringr::str_detect(.data$play_text, regex("out-of-bounds|out of bounds", ignore_case = TRUE)) &
    (.data$kickoff == 1) & !is.na(.data$play_text), 1, 0),
    kickoff_fair_catch = ifelse(stringr::str_detect(.data$play_text, regex("fair catch|fair caught", ignore_case = TRUE)) &
    (.data$kickoff == 1) & !is.na(.data$play_text), 1, 0),
    kickoff_downed = ifelse(stringr::str_detect(.data$play_text, regex("downed", ignore_case = TRUE)) &
    (.data$kickoff == 1) & !is.na(.data$play_text), 1, 0),
    kickoff_safety = ifelse(!(.data$play_type %in% c("Blocked Punt", "Penalty")) & .data$safety == 1 &
    stringr::str_detect(.data$play_text, regex("kickoff", ignore_case = TRUE)), 1, 0)
  )
}

add_punt_plays <- function(data) {
  data |>
  mutate(
    punt = ifelse(.data$play_type %in% punt_text(), 1, 0),
    punt_play = ifelse(stringr::str_detect(.data$play_text, regex("punt", ignore_case = TRUE)) &
    !is.na(.data$play_text), 1, 0),
    punt_tb = ifelse(stringr::str_detect(.data$play_text, regex("touchback", ignore_case = TRUE)) &
    (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
    punt_oob = ifelse(stringr::str_detect(.data$play_text, regex("out-of-bounds|out of bounds", ignore_case = TRUE)) &
    (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
    punt_fair_catch = ifelse(stringr::str_detect(.data$play_text, regex("fair catch|fair caught", ignore_case = TRUE)) &
    (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
    punt_downed = ifelse(stringr::str_detect(.data$play_text, regex("downed", ignore_case = TRUE)) &
    (.data$punt == 1) & !is.na(.data$play_text), 1, 0),
    punt_safety = ifelse((.data$play_type %in% c("Blocked Punt", "Punt")) & .data$safety == 1 &
    stringr::str_detect(.data$play_text, regex("punt", ignore_case = TRUE)), 1, 0),
    punt_blocked = ifelse(.data$punt == 1 & stringr::str_detect(.data$play_text, regex("blocked", ignore_case = TRUE)), 1, 0)
  )
}

add_sack_plays <- function(data) {
  data |>
  mutate(
    sack = ifelse((.data$play_type %in% c("Sack", "Sack Touchdown") |
    (.data$play_type %in% c(
      "Fumble Recovery (Own)",
      "Fumble Recovery (Own) Touchdown",
      "Fumble Recovery (Opponent)",
      "Fumble Recovery (Opponent) Touchdown"
    ) &
    .data$pass == 1 & stringr::str_detect(.data$play_text, "sacked")) |
    (.data$play_type == "Safety" & stringr::str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)))) &
    !is.na(.data$play_text), 1, 0),
  )
}

add_turnover_plays <- function(data) {
  data |>
  mutate(
    fumble = ifelse(stringr::str_detect(.data$play_text, "fumble") & !is.na(.data$play_text), 1, 0),
    int = ifelse(.data$play_type %in% c("Interception Return", "Interception Return Touchdown"), 1, 0),
    int_td = ifelse(.data$play_type %in% c("Interception Return Touchdown"), 1, 0),
    turnover = ifelse(.data$play_type %in% turnover_text(), 1, 0),
    offense_score_play = ifelse(.data$play_type %in% offense_score_text(), 1, 0),
    defense_score_play = ifelse(.data$play_type %in% defense_score_text(), 1, 0)
  )
}


add_penalty_plays <- function(data) {
  #-- 'Penalty' in play text ----
  pen_text <- stringr::str_detect(data$play_text, regex("penalty", ignore_case = TRUE))
  #-- 'Declined' in play text ----
  pen_declined_text <- stringr::str_detect(data$play_text, regex("declined", ignore_case = TRUE))
  #-- 'No Play' in play text ----
  pen_no_play_text <- stringr::str_detect(data$play_text, regex("no play", ignore_case = TRUE))
  #-- 'Off-setting' in play text ----
  pen_offset_text <- stringr::str_detect(data$play_text, regex("off-setting", ignore_case = TRUE))
  #-- '1st Down' in play text ----
  pen_1st_down_text <- stringr::str_detect(data$play_text, regex("1st down", ignore_case = TRUE))

  #-- Penalty play_types
  pen_type <- data$play_type == "Penalty" | data$play_type == "penalty"

  #-- T/F flag conditions penalty_flag
  data$penalty_flag <- FALSE
  data$penalty_flag[pen_type] <- TRUE
  data$penalty_flag[pen_text] <- TRUE
  #-- T/F flag conditions penalty_declined
  data$penalty_declined <- FALSE
  data$penalty_declined[pen_text & pen_declined_text] <- TRUE
  data$penalty_declined[pen_type & pen_declined_text] <- TRUE
  #-- T/F flag conditions penalty_no_play
  data$penalty_no_play <- FALSE
  data$penalty_no_play[pen_text & pen_no_play_text] <- TRUE
  data$penalty_no_play[pen_type & pen_no_play_text] <- TRUE
  #-- T/F flag conditions penalty_offset
  data$penalty_offset <- FALSE
  data$penalty_offset[pen_text & pen_offset_text] <- TRUE
  data$penalty_offset[pen_type & pen_offset_text] <- TRUE
  #-- T/F flag conditions penalty_1st_conv
  data$penalty_1st_conv <- FALSE
  data$penalty_1st_conv[pen_text & pen_1st_down_text] <- TRUE
  data$penalty_1st_conv[pen_type & pen_1st_down_text] <- TRUE
  #-- T/F flag for penalty text but not penalty play type --
  data$penalty_text <- FALSE
  data$penalty_text[pen_text & !pen_type & !pen_declined_text &
    !pen_offset_text & !pen_no_play_text] <- TRUE

    data |>
    dplyr::mutate(
      penalty_detail = case_when(
        .data$penalty_offset ~ "Off-Setting",
        .data$penalty_declined ~ "Penalty Declined",
        stringr::str_detect(.data$play_text, regex(" roughing passer ", ignore_case = TRUE)) ~ "Roughing the Passer",
        stringr::str_detect(.data$play_text, regex(" offensive holding ", ignore_case = TRUE)) ~ "Offensive Holding",
        stringr::str_detect(.data$play_text, regex(" pass interference", ignore_case = TRUE)) ~ "Pass Interference",
        stringr::str_detect(.data$play_text, regex(" encroachment", ignore_case = TRUE)) ~ "Encroachment",
        stringr::str_detect(.data$play_text, regex(" defensive pass interference ", ignore_case = TRUE)) ~ "Defensive Pass Interference",
        stringr::str_detect(.data$play_text, regex(" offensive pass interference ", ignore_case = TRUE)) ~ "Offensive Pass Interference",
        stringr::str_detect(.data$play_text, regex(" illegal procedure ", ignore_case = TRUE)) ~ "Illegal Procedure",
        stringr::str_detect(.data$play_text, regex(" defensive holding ", ignore_case = TRUE)) ~ "Defensive Holding",
        stringr::str_detect(.data$play_text, regex(" holding ", ignore_case = TRUE)) ~ "Holding",
        stringr::str_detect(.data$play_text, regex(" offensive offside | offside offense", ignore_case = TRUE)) ~ "Offensive Offside",
        stringr::str_detect(.data$play_text, regex(" defensive offside | offside defense", ignore_case = TRUE)) ~ "Defensive Offside",
        stringr::str_detect(.data$play_text, regex(" offside ", ignore_case = TRUE)) ~ "Offside",
        stringr::str_detect(.data$play_text, regex(" illegal fair catch signal ", ignore_case = TRUE)) ~ "Illegal Fair Catch Signal",
        stringr::str_detect(.data$play_text, regex(" illegal batting ", ignore_case = TRUE)) ~ "Illegal Batting",
        stringr::str_detect(.data$play_text, regex(" neutral zone infraction ", ignore_case = TRUE)) ~ "Neutral Zone Infraction",
        stringr::str_detect(.data$play_text, regex(" ineligible downfield ", ignore_case = TRUE)) ~ "Ineligible Man Down-Field",
        stringr::str_detect(.data$play_text, regex(" illegal use of hands ", ignore_case = TRUE)) ~ "Illegal Use of Hands",
        stringr::str_detect(.data$play_text, regex(" kickoff out of bounds | kickoff out-of-bounds ", ignore_case = TRUE)) ~ "Kickoff Out-of-Bounds",
        stringr::str_detect(.data$play_text, regex(" 12 men on the field ", ignore_case = TRUE)) ~ "12 Men on the Field",
        stringr::str_detect(.data$play_text, regex(" illegal block ", ignore_case = TRUE)) ~ "Illegal Block",
        stringr::str_detect(.data$play_text, regex(" personal foul ", ignore_case = TRUE)) ~ "Personal Foul",
        stringr::str_detect(.data$play_text, regex(" false start ", ignore_case = TRUE)) ~ "False Start",
        stringr::str_detect(.data$play_text, regex(" substitution infraction ", ignore_case = TRUE)) ~ "Substitution Infraction",
        stringr::str_detect(.data$play_text, regex(" illegal formation ", ignore_case = TRUE)) ~ "Illegal Formation",
        stringr::str_detect(.data$play_text, regex(" illegal touching ", ignore_case = TRUE)) ~ "Illegal Touching",
        stringr::str_detect(.data$play_text, regex(" sideline interference ", ignore_case = TRUE)) ~ "Sideline Interference",
        stringr::str_detect(.data$play_text, regex(" clipping ", ignore_case = TRUE)) ~ "Clipping",
        stringr::str_detect(.data$play_text, regex(" sideline infraction ", ignore_case = TRUE)) ~ "Sideline Infraction",
        stringr::str_detect(.data$play_text, regex(" crackback ", ignore_case = TRUE)) ~ "Crackback",
        stringr::str_detect(.data$play_text, regex(" illegal snap ", ignore_case = TRUE)) ~ "Illegal Snap",
        stringr::str_detect(.data$play_text, regex(" illegal helmet contact ", ignore_case = TRUE)) ~ "Illegal Helmet contact",
        stringr::str_detect(.data$play_text, regex(" roughing holder ", ignore_case = TRUE)) ~ "Roughing the Holder",
        stringr::str_detect(.data$play_text, regex(" horse collar tackle ", ignore_case = TRUE)) ~ "Horse-Collar Tackle",
        stringr::str_detect(.data$play_text, regex(" illegal participation ", ignore_case = TRUE)) ~ "Illegal Participation",
        stringr::str_detect(.data$play_text, regex(" tripping ", ignore_case = TRUE)) ~ "Tripping",
        stringr::str_detect(.data$play_text, regex(" illegal shift ", ignore_case = TRUE)) ~ "Illegal Shift",
        stringr::str_detect(.data$play_text, regex(" illegal motion ", ignore_case = TRUE)) ~ "Illegal Motion",
        stringr::str_detect(.data$play_text, regex(" roughing the kicker ", ignore_case = TRUE)) ~ "Roughing the Kicker",
        stringr::str_detect(.data$play_text, regex(" delay of game ", ignore_case = TRUE)) ~ "Delay of Game",
        stringr::str_detect(.data$play_text, regex(" targeting ", ignore_case = TRUE)) ~ "Targeting",
        stringr::str_detect(.data$play_text, regex(" face mask ", ignore_case = TRUE)) ~ "Face Mask",
        stringr::str_detect(.data$play_text, regex(" illegal forward pass ", ignore_case = TRUE)) ~ "Illegal Forward Pass",
        stringr::str_detect(.data$play_text, regex(" intentional grounding ", ignore_case = TRUE)) ~ "Intentional Grounding",
        stringr::str_detect(.data$play_text, regex(" illegal kicking ", ignore_case = TRUE)) ~ "Illegal Kicking",
        stringr::str_detect(.data$play_text, regex(" illegal conduct ", ignore_case = TRUE)) ~ "Illegal Conduct",
        stringr::str_detect(.data$play_text, regex(" kick catching interference ", ignore_case = TRUE)) ~ "Kick Catch Interference",
        stringr::str_detect(.data$play_text, regex(" unnecessary roughness ", ignore_case = TRUE)) ~ "Unnecessary Roughness",
        stringr::str_detect(.data$play_text, regex("Penalty, UR")) ~ "Unnecessary Roughness",
        stringr::str_detect(.data$play_text, regex(" unsportsmanlike conduct ", ignore_case = TRUE)) ~ "Unsportsmanlike Conduct",
        stringr::str_detect(.data$play_text, regex(" running into kicker ", ignore_case = TRUE)) ~ "Running Into Kicker",
        stringr::str_detect(.data$play_text, regex(" failure to wear required equipment ", ignore_case = TRUE)) ~ "Failure to Wear Required Equipment",
        stringr::str_detect(.data$play_text, regex(" player disqualification ", ignore_case = TRUE)) ~ "Player Disqualification",
        .data$penalty_flag ~ "Missing",
        TRUE ~ NA_character_
      ),
      penalty_play_text = ifelse(.data$penalty_flag, stringr::str_extract(.data$play_text, regex("Penalty(.+)", ignore_case = TRUE)), NA_character_),
      yds_penalty = ifelse(.data$penalty_flag,
        stringr::str_extract(.data$penalty_play_text, "(.{0,3})yards to the |(.{0,3})yds to the |(.{0,3})yd to the "), NA_real_
      ),
      yds_penalty = stringr::str_remove(.data$yds_penalty, " yards to the | yds to the | yd to the "),
      yds_penalty = ifelse(.data$penalty_flag & stringr::str_detect(.data$play_text, "ards\\)") & is.na(.data$yds_penalty),
      stringr::str_extract(.data$play_text, "(.{0,4})yards\\)|(.{0,4})Yards\\)|(.{0,4})yds\\)|(.{0,4})Yds\\)"), .data$yds_penalty
    ),
    yds_penalty = stringr::str_remove(.data$yds_penalty, "yards\\)|Yards\\)|yds\\)|Yds\\)"),
    yds_penalty = stringr::str_remove(.data$yds_penalty, "\\("),
    penalty_safety = ifelse((.data$play_type %in% c("Penalty")) & .data$safety == 1, 1, 0)
  ) |>
  mutate(yds_penalty = stringr::str_trim(.data$yds_penalty)) |>
  rename(penalty_yds = yds_penalty)
}

clean_play_type <- function(data) {
  data |>
  # add change of possession
  group_by(game_id) |>
  arrange(game_id, play_id, drive_id) |>
  mutate(
    change_of_poss = case_when(
      offense != dplyr::lead(offense, 1) & (!play_text %in% end_text() | is.na(dplyr::lead(play_type, 1))) ~ 1,
      play_text %in% end_text() ~ 1,
      TRUE ~ 0
    )
  ) |>
  # cleaning play types
  mutate(
    play_type = ifelse(stringr::str_detect(.data$play_text, regex(" coin toss ", ignore_case = TRUE)),
    "Coin Toss", .data$play_type
  ),
  ## Fix Strip-Sacks to Fumbles----
  play_type = ifelse(.data$fumble == 1 & .data$pass == 1 &
    .data$change_of_poss == 1 & .data$td_play == 0 & .data$down != 4 &
    !(.data$play_type %in% defense_score_text()),
    "Fumble Recovery (Opponent)", .data$play_type
  ),
  play_type = ifelse(.data$fumble == 1 & .data$pass == 1 &
    .data$change_of_poss == 1 & .data$td_play == 1,
    "Fumble Recovery (Opponent) Touchdown", .data$play_type
  ),
  ## Fix rushes with fumbles and a change of possession to fumbles----
  play_type = ifelse(.data$fumble == 1 & .data$rush == 1 &
    .data$change_of_poss == 1 & .data$td_play == 0 &
    !(.data$play_type %in% defense_score_text()),
    "Fumble Recovery (Opponent)", .data$play_type
  ),
  play_type = ifelse(.data$fumble == 1 & .data$rush == 1 &
    .data$change_of_poss == 1 & .data$td_play == 1,
    "Fumble Recovery (Opponent) Touchdown", .data$play_type
  ),
  ## Portion of touchdown check for plays where touchdown is not listed in the play_type--
  td_check = ifelse(!str_detect(.data$play_type, "Touchdown"), 1, 0),
  #-- Fix kickoff fumble return TDs ----
  play_type = ifelse(.data$kickoff == 1 & .data$fumble == 1 &
    .data$td_play == 1 & .data$td_check == 1,
    paste0(.data$play_type, " Touchdown"),
    .data$play_type
  ),
  #-- Fix punt return TDs ----
  play_type = ifelse(.data$punt_play == 1 & .data$td_play == 1 & .data$td_check == 1,
    paste0(.data$play_type, " Touchdown"),
    .data$play_type
  ),
  #-- Fix kick return TDs----
  play_type = ifelse(.data$kickoff == 1 & .data$fumble == 0 &
    .data$td_play == 1 & .data$td_check == 1,
    "Kickoff Return Touchdown",
    .data$play_type
  ),
  #-- Fix rush/pass tds that aren't explicit----
  play_type = ifelse(.data$td_play == 1 & .data$rush == 1 &
    .data$fumble == 0 & .data$td_check == 1,
    "Rushing Touchdown",
    .data$play_type
  ),
  play_type = ifelse(.data$td_play == 1 & .data$pass == 1 & .data$td_check == 1 &
    .data$fumble == 0 & !(.data$play_type %in% int_text()),
    "Passing Touchdown",
    .data$play_type
  ),
  play_type = ifelse(.data$pass == 1 & .data$play_type == "Pass Reception" &
  .data$yards_gained == .data$yards_to_goal &
  .data$fumble == 0 & !(.data$play_type %in% int_text()),
  "Passing Touchdown",
  .data$play_type
),
play_type = ifelse(.data$play_type == "Blocked Field Goal" &
stringr::str_detect(.data$play_text, regex("for a TD", ignore_case = TRUE)),
"Blocked Field Goal Touchdown",
.data$play_type
),
#-- Fix duplicated TD play_type labels----
play_type = ifelse(.data$play_type == "Punt Touchdown Touchdown", "Punt Touchdown", .data$play_type),
play_type = ifelse(.data$play_type == "Fumble Return Touchdown Touchdown", "Fumble Return Touchdown", .data$play_type),
play_type = ifelse(.data$play_type == "Rushing Touchdown Touchdown", "Rushing Touchdown", .data$play_type),
play_type = ifelse(.data$play_type == "Uncategorized Touchdown Touchdown", "Uncategorized Touchdown", .data$play_type),
#-- Fix Pass Interception Return TD play_type labels----
play_type = ifelse(stringr::str_detect(.data$play_text, "pass intercepted for a TD") & !is.na(.data$play_text),
"Interception Return Touchdown", .data$play_type
),
#-- Fix Sack/Fumbles Touchdown play_type labels----
play_type = ifelse(stringr::str_detect(.data$play_text, regex("sacked", ignore_case = TRUE)) &
stringr::str_detect(.data$play_text, regex("fumbled", ignore_case = TRUE)) &
stringr::str_detect(.data$play_text, regex("TD", ignore_case = TRUE)) &
!is.na(.data$play_text),
"Fumble Recovery (Opponent) Touchdown", .data$play_type
),
#-- Fix generic pass plays ----
## -- first one looks for complete pass
play_type = ifelse(.data$play_type == "Pass" & str_detect(.data$play_text, "pass complete"),
"Pass Completion", .data$play_type
),
## -- second one looks for incomplete pass
play_type = ifelse(.data$play_type == "Pass" & str_detect(.data$play_text, "pass incomplete"),
"Pass Incompletion", .data$play_type
),
## -- third one looks for interceptions
play_type = ifelse(.data$play_type == "Pass" & str_detect(.data$play_text, "pass intercepted"),
"Pass Interception", .data$play_type
),
## -- fourth one looks for sacked
play_type = ifelse(.data$play_type == "Pass" & str_detect(.data$play_text, "sacked"), "Sack", .data$play_type),
## -- fifth one play type is Passing Touchdown, but its intercepted
play_type = ifelse(.data$play_type == "Passing Touchdown" & str_detect(.data$play_text, "pass intercepted for a TD"),
"Interception Return Touchdown", .data$play_type
),
#--- Moving non-Touchdown pass interceptions to one play_type: "Interception Return" -----
play_type = ifelse(.data$play_type == "Interception", "Interception Return", .data$play_type),
play_type = ifelse(.data$play_type == "Pass Interception", "Interception Return", .data$play_type),
play_type = ifelse(.data$play_type == "Pass Interception Return", "Interception Return", .data$play_type),
#--- Moving Kickoff/Punt Touchdowns without fumbles to Kickoff/Punt Return Touchdown
play_type = ifelse(.data$play_type == "Kickoff Touchdown" & .data$fumble == 0,
"Kickoff Return Touchdown", .data$play_type
),
play_type = ifelse(.data$play_type %in% c("Kickoff", "Kickoff Return (Offense)") &
.data$fumble == 1 & .data$change_of_poss == 0,
"Kickoff Team Fumble Recovery", .data$play_type
),
play_type = ifelse(.data$play_type == "Punt Touchdown" &
(.data$fumble == 0 | (.data$fumble == 1 & .data$game_id == 401112100)),
"Punt Return Touchdown", .data$play_type
),
play_type = ifelse(.data$play_type == "Punt" & .data$fumble == 1 & .data$change_of_poss == 0,
"Punt Team Fumble Recovery", .data$play_type
),
play_type = ifelse(.data$play_type == "Punt Touchdown", "Punt Team Fumble Recovery Touchdown", .data$play_type),
play_type = ifelse(.data$play_type == "Kickoff Touchdown", "Kickoff Team Fumble Recovery Touchdown", .data$play_type),
play_type = ifelse(.data$play_type == "Fumble Return Touchdown" & (.data$pass == 1 | .data$rush == 1),
"Fumble Recovery (Opponent) Touchdown", .data$play_type
),
#--- Safeties (kickoff, punt, penalty) ----
play_type = ifelse(.data$play_type %in% c("Pass Reception", "Rush", "Rushing Touchdown") &
(.data$pass == 1 | .data$rush == 1) & .data$safety == 1,
"Safety", .data$play_type
),
play_type = ifelse(.data$kickoff_safety == 1, "Kickoff (Safety)", .data$play_type),
play_type = ifelse(.data$punt_safety == 1, paste0(.data$play_type, " (Safety)"), .data$play_type),
play_type = ifelse(.data$penalty_safety == 1, paste0(.data$play_type, " (Safety)"), .data$play_type),
)
}

# td_text = function(text = score_text(), pattern = "Touchdown") {
#
#     text[stringr::str_detect(text, "Touchdown")]
#
# }
#
# fg_text = function(text = score_text(), pattern = "Fied Goal Good") {
#
#     text[stringr::str_detect(text, "Field Goal")]
#
# }

add_score_events <- function(data, td_text = "TD$", fg_text = "FG$", safety_text = "SF$") {
  data |>
  mutate(
    score_event = case_when(
      stringr::str_detect(drive_result, pattern = td_text) & drive_is_home_offense == T ~ "HOME TD",
      stringr::str_detect(drive_result, pattern = td_text) & drive_is_home_offense == F ~ "AWAY TD",
      stringr::str_detect(drive_result, pattern = fg_text) & drive_is_home_offense == T ~ "HOME FG",
      stringr::str_detect(drive_result, pattern = fg_text) & drive_is_home_offense == F ~ "AWAY FG",
      stringr::str_detect(drive_result, pattern = safety_text) & drive_is_home_offense == T ~ "AWAY Safety",
      stringr::str_detect(drive_result, pattern = safety_text) & drive_is_home_offense == F ~ "HOME Safety"
    )
  ) |>
  # add next score event
  group_by(game_id) |>
  arrange(game_id, period, half, drive_number, play_number) |>
  # identify last drive of each half
  group_by(game_id, half) |>
  mutate(
    last_drive_in_half = case_when(
      drive_number == max(drive_number) ~ T,
      TRUE ~ F
    )
  ) |>
  # fill in next score
  group_by(game_id) |>
  mutate(
    next_score_event = case_when(
      is.na(score_event) & last_drive_in_half == T ~ "No_Score",
      TRUE ~ score_event
    )
  ) |>
  # this fills in previous drives with no scoring event with the next observed scoring event in a half
  fill(next_score_event, .direction = "up") |>
  ungroup() |>
  # now classify next score event from the perspective of the offense
  mutate(next_score_event_offense = case_when(
    drive_is_home_offense == T & next_score_event == "HOME TD" ~ "TD",
    drive_is_home_offense == T & next_score_event == "AWAY TD" ~ "Opp_TD",
    drive_is_home_offense == T & next_score_event == "HOME FG" ~ "FG",
    drive_is_home_offense == T & next_score_event == "AWAY FG" ~ "Opp_FG",
    drive_is_home_offense == T & next_score_event == "HOME Safety" ~ "Safety",
    drive_is_home_offense == T & next_score_event == "AWAY Safety" ~ "Opp_Safety",
    drive_is_home_offense == F & next_score_event == "HOME TD" ~ "Opp_TD",
    drive_is_home_offense == F & next_score_event == "AWAY TD" ~ "TD",
    drive_is_home_offense == F & next_score_event == "HOME FG" ~ "Opp_FG",
    drive_is_home_offense == F & next_score_event == "AWAY FG" ~ "FG",
    drive_is_home_offense == F & next_score_event == "HOME Safety" ~ "Opp_Safety",
    drive_is_home_offense == F & next_score_event == "AWAY Safety" ~ "Safety",
    next_score_event == "No_Score" ~ "No_Score"
  )) |>
  # create a diff
  mutate(next_score_event_offense_diff = case_when(
    next_score_event_offense == 'TD' ~ 7,
    next_score_event_offense == 'FG' ~ 3,
    next_score_event_offense == 'Safety' ~ 2,
    next_score_event_offense == 'No_Score' ~ 0,
    next_score_event_offense == 'Opp_Safety' ~ -2,
    next_score_event_offense == 'Opp_FG' ~ -3,
    next_score_event_offense == 'Opp_TD' ~ -7)) |>
  # convert to factor
  mutate(
    next_score_event_offense = factor(next_score_event_offense,
      levels = c(
        "TD",
        "FG",
        "Safety",
        "No_Score",
        "Opp_Safety",
        "Opp_FG",
        "Opp_TD"
      )
    )
  )
}
