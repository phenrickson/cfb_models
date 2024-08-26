gt_tbl = function(data, 
                  ...) {

  data |>
    gt::gt() |>
    gt::sub_missing() |>
    gtExtras::gt_theme_espn() |>
    gt::tab_options(
      container.overflow.y = T,
      ...
    )
}

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}
