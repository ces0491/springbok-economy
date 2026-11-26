root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "prepare.R"))
source(file.path(root, "R", "models.R"))

#' A run of FRED-style rand fixes on the given dates.
toy_fx <- function(dates, zar, em = NULL) {
  zar_tbl <- tibble::tibble(date = as.Date(dates), value = zar)
  em <- em %||% zar
  bm <- list(AAA = tibble::tibble(date = as.Date(dates), value = em))
  fx_returns(zar_tbl, bm, start = min(as.Date(dates)))
}

#' A minimal Springbok Test table in the shape springbok_tests() returns.
toy_tests <- function(dates, results) {
  dates <- as.Date(dates)
  tibble::tibble(
    match_date = dates, opponent = "Opponent", sa_score = ifelse(results == "Win", 20L, 10L),
    opp_score = ifelse(results == "Win", 10L, 20L), margin = sa_score - opp_score,
    result = factor(results, levels = c("Win", "Loss", "Draw")),
    weekday = lubridate::wday(dates, label = TRUE, abbr = TRUE, locale = "C", week_start = 1),
    weekend = weekday %in% c("Sat", "Sun"), rwc = FALSE, rwc_year = NA_integer_,
    knockout_by_rule = FALSE, vs_nz = FALSE, corrected = FALSE, home = TRUE
  )
}
