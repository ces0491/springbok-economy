# Raw snapshots in, analysis tables out. Every function here is pure: it reads
# what it is given and returns a table, so tests/ can exercise it on toy data.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(jsonlite)
})

# ---- Rugby results -------------------------------------------------------------

#' Flatten World Rugby's match-list pages into one row per entry. Run by
#' fetch_data.R, which writes the result to WORLD_RUGBY_FACTS: World Rugby's
#' terms do not allow its pages to be republished, so they stay out of the
#' repository and the paper renders from these facts.
extract_world_rugby <- function(dir = WORLD_RUGBY_PAGES_DIR) {
  files <- sort(list.files(dir, pattern = "^springboks_\\d{4}_p\\d+\\.json$", full.names = TRUE))
  entries <- unlist(lapply(files, function(f) fromJSON(f, simplifyVector = FALSE)$content), recursive = FALSE)
  str_or_na <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  bind_rows(lapply(entries, function(e) {
    tibble(
      wr_match_id = str_or_na(e$matchId),
      match_date = as.Date(e$time$label),
      kickoff_utc = as.POSIXct(e$time$millis / 1000, origin = "1970-01-01", tz = "UTC"),
      team1 = e$teams[[1]]$name,
      team2 = e$teams[[2]]$name,
      score1 = as.integer(e$scores[[1]]),
      score2 = as.integer(e$scores[[2]]),
      status = str_or_na(e$status),
      competition = str_or_na(e$competition),
      phase = str_or_na(e$eventPhase),
      phase_type = str_or_na(e$eventPhaseId$type),
      venue_country = str_or_na(e$venue$country)
    )
  })) |>
    distinct(wr_match_id, .keep_all = TRUE) |>
    mutate(
      sa_first = team1 == "South Africa",
      opponent = if_else(sa_first, team2, team1),
      sa_score = if_else(sa_first, score1, score2),
      opp_score = if_else(sa_first, score2, score1)
    ) |>
    select(all_of(names(WORLD_RUGBY_FACT_TYPES$cols))) |>
    arrange(match_date)
}

#: Columns of the World Rugby facts table, and how to read them back.
WORLD_RUGBY_FACT_TYPES <- cols(
  wr_match_id = col_character(),
  match_date = col_date(),
  kickoff_utc = col_datetime(),
  opponent = col_character(),
  sa_score = col_integer(),
  opp_score = col_integer(),
  status = col_character(),
  competition = col_character(),
  phase = col_character(),
  phase_type = col_character(),
  venue_country = col_character()
)

#' The match facts the paper uses, one row per World Rugby entry, as written
#' by fetch_data.R from extract_world_rugby().
read_world_rugby <- function(path = WORLD_RUGBY_FACTS) {
  # trim_ws = FALSE: a few of World Rugby's stage labels carry stray spaces,
  # and the table should read back exactly what the pages say.
  read_csv(path, col_types = WORLD_RUGBY_FACT_TYPES, na = "", trim_ws = FALSE)
}

#: Pool matches each side played at a Rugby World Cup, by edition: four pools of
#: four in 1995, five pools of four in 1999, four pools of five from 2003.
RWC_POOL_MATCHES <- c(`1995` = 3L, `1999` = 3L)
RWC_POOL_MATCHES_DEFAULT <- 4L

#' Keep completed Tests, apply documented score corrections, and label each one.
springbok_tests <- function(wr, corrections) {
  tests <- wr |>
    filter(status == "C", opponent %in% TEST_OPPONENTS)

  fix <- corrections |>
    mutate(match_date = as.Date(match_date)) |>
    select(match_date, opponent, world_rugby_sa_score, world_rugby_opp_score,
           fixed_sa = sa_score, fixed_opp = opp_score)
  joined <- left_join(tests, fix, by = c("match_date", "opponent"))
  stale <- joined |>
    filter(!is.na(fixed_sa), sa_score != world_rugby_sa_score | opp_score != world_rugby_opp_score)
  if (nrow(stale) > 0) {
    stop("A correction no longer matches World Rugby's entry; re-check data/corrections: ",
         paste(stale$match_date, stale$opponent, collapse = "; "))
  }

  joined |>
    mutate(
      corrected = !is.na(fixed_sa),
      sa_score = coalesce(fixed_sa, sa_score),
      opp_score = coalesce(fixed_opp, opp_score),
      margin = sa_score - opp_score,
      result = factor(case_when(margin > 0 ~ "Win", margin < 0 ~ "Loss", TRUE ~ "Draw"),
                      levels = c("Win", "Loss", "Draw")),
      weekday = wday(match_date, label = TRUE, abbr = TRUE, locale = "C", week_start = 1),
      weekend = weekday %in% c("Sat", "Sun"),
      rwc = grepl("^Men's Rugby World Cup \\d{4}$", competition),
      rwc_year = as.integer(ifelse(rwc, sub("^Men's Rugby World Cup ", "", competition), NA)),
      vs_nz = opponent == "New Zealand"
    ) |>
    group_by(rwc_year) |>
    mutate(
      rwc_match_no = if_else(rwc, row_number(), NA_integer_),
      pool_size = if_else(
        rwc,
        coalesce(unname(RWC_POOL_MATCHES[as.character(rwc_year)]), RWC_POOL_MATCHES_DEFAULT),
        NA_integer_
      ),
      knockout_by_rule = rwc & rwc_match_no > pool_size,
      knockout_by_label = rwc & phase_type %in% c("Quarter", "Semi", "Final", "Play-Off")
    ) |>
    ungroup() |>
    select(-fixed_sa, -fixed_opp, -world_rugby_sa_score, -world_rugby_opp_score) |>
    arrange(match_date)
}

#' Where World Rugby labels a stage, the format rule must agree with it.
check_knockout_rule <- function(tests) {
  labelled <- tests |> filter(rwc, !is.na(phase_type))
  bad <- labelled |> filter(knockout_by_rule != knockout_by_label)
  if (nrow(bad) > 0) {
    stop("Knockout rule disagrees with World Rugby's stage label for: ",
         paste(bad$match_date, bad$opponent, collapse = "; "))
  }
  invisible(nrow(labelled))
}

#' The finals the configuration names must be exactly the World Cup finals
#' World Rugby lists as Springbok wins. Bronze finals are labelled apart.
check_rwc_finals <- function(tests) {
  finals <- tests |> filter(rwc, phase %in% "Final", result == "Win")
  if (!setequal(finals$match_date, RWC_FINALS_WON)) {
    stop("RWC_FINALS_WON (", paste(RWC_FINALS_WON, collapse = ", "),
         ") does not match World Rugby's finals won (", paste(finals$match_date, collapse = ", "), ")")
  }
  invisible(finals)
}

#' Pair World Rugby's Tests with the second database: same opponent, dates
#' within a day (an evening kick-off in the Americas falls on the next UTC day).
crosscheck_tests <- function(wr_tests_raw, db) {
  db <- db |>
    mutate(
      match_date = as.Date(match_date),
      opponent = if_else(home_team == "South Africa", away_team, home_team),
      db_sa_home = home_team == "South Africa",
      db_sa = if_else(home_team == "South Africa", home_score, away_score),
      db_opp = if_else(home_team == "South Africa", away_score, home_score)
    ) |>
    select(db_date = match_date, opponent, db_sa, db_opp, db_sa_home) |>
    mutate(db_row = row_number())

  pairs <- wr_tests_raw |>
    select(match_date, opponent, sa_score, opp_score) |>
    mutate(wr_row = row_number()) |>
    inner_join(db, by = "opponent", relationship = "many-to-many") |>
    filter(abs(as.numeric(db_date - match_date)) <= 1)
  if (anyDuplicated(pairs$wr_row) || anyDuplicated(pairs$db_row)) {
    stop("A Test pairs with more than one row in the other source")
  }
  list(
    n_world_rugby = nrow(wr_tests_raw),
    n_db = nrow(db),
    n_paired = nrow(pairs),
    pairs = pairs,
    only_world_rugby = wr_tests_raw[setdiff(seq_len(nrow(wr_tests_raw)), pairs$wr_row), ],
    only_db = db[setdiff(db$db_row, pairs$db_row), ],
    score_disagreements = pairs |> filter(sa_score != db_sa | opp_score != db_opp),
    date_offsets = sum(pairs$db_date != pairs$match_date)
  )
}

#' Whether each Test was played in South Africa. World Rugby's venue decides;
#' where it lists none, the second database's home side does. A Test at a
#' neutral venue abroad, such as a World Cup match, is not a home Test.
add_home <- function(tests, pairs) {
  out <- tests |>
    left_join(select(pairs, match_date, opponent, db_sa_home), by = c("match_date", "opponent")) |>
    mutate(home = if_else(is.na(venue_country), db_sa_home, venue_country == "South Africa")) |>
    select(-db_sa_home)
  if (anyNA(out$home)) {
    stop("No venue in either source for: ",
         paste(out$match_date[is.na(out$home)], out$opponent[is.na(out$home)], collapse = "; "))
  }
  out
}

# ---- Exchange rates -----------------------------------------------------------

read_fred <- function(series_id, dir = "data/raw/fred") {
  x <- read_csv(file.path(dir, paste0(series_id, ".csv")),
                col_types = cols(.default = col_double(), observation_date = col_date()),
                na = c("", "."))
  names(x) <- c("date", "value")
  x
}

#' Returns between consecutive rand fixes, in percent, signed so that a positive
#' number is a stronger currency against the US dollar. Benchmark currencies are
#' measured over exactly the same pair of dates; a currency missing at either
#' end drops out of that interval's equal-weighted average.
fx_returns <- function(zar, benchmark, start = FX_START) {
  zar <- zar |> filter(!is.na(value)) |> arrange(date)
  out <- zar |>
    transmute(date, prev_date = lag(date), zar = value, zar_prev = lag(value)) |>
    filter(!is.na(prev_date), prev_date >= start) |>
    mutate(r_zar = -100 * log(zar / zar_prev))
  em <- lapply(names(benchmark), function(code) {
    s <- benchmark[[code]] |> filter(!is.na(value)) |> select(date, value)
    out |>
      select(date, prev_date) |>
      left_join(rename(s, now = value), by = "date") |>
      left_join(rename(s, before = value), by = c("prev_date" = "date")) |>
      transmute(!!code := -100 * log(now / before))
  })
  em <- bind_cols(em)
  out |>
    bind_cols(em) |>
    mutate(
      n_em = rowSums(!is.na(em)),
      r_em = if_else(n_em > 0, rowMeans(as.matrix(em), na.rm = TRUE), NA_real_),
      gap_days = as.integer(date - prev_date)
    )
}

#' The window in which a Test's result is first priced: from the last fix
#' strictly before the match date to the first fix strictly after it. Correct
#' whatever the kick-off time, which World Rugby records unreliably before 2010.
safe_window <- function(match_date, fix_dates) {
  fix_dates <- sort(fix_dates)
  before <- vapply(match_date, function(d) {
    x <- fix_dates[fix_dates < d]
    if (length(x)) as.numeric(max(x)) else NA_real_
  }, numeric(1))
  after <- vapply(match_date, function(d) {
    x <- fix_dates[fix_dates > d]
    if (length(x)) as.numeric(min(x)) else NA_real_
  }, numeric(1))
  tibble(window_start = as.Date(before), window_end = as.Date(after))
}

#' Every date with a rand fix, including the one that opens the first return.
all_fix_dates <- function(fx) sort(unique(c(fx$prev_date, fx$date)))

#' One row per weekend: the return from the last fix before Saturday to the
#' first fix after Sunday, with the Test played in it, if any. Weekends that
#' overlap a weekday Test's window are dropped, being neither clean controls
#' nor clean treatments.
weekend_panel <- function(fx, tests, vol_window = VOL_WINDOW_DAYS) {
  fx <- fx |>
    arrange(date) |>
    mutate(sd_trail = lag(zoo::rollapplyr(r_zar, vol_window, sd, fill = NA)))
  saturday_after <- function(d) d + ((6L - wday(d, week_start = 1)) %% 7L)
  weekends <- fx |>
    mutate(saturday = saturday_after(prev_date + 1L)) |>
    filter(saturday < date, saturday + 1L < date) |>
    mutate(year = year(saturday), month = month(saturday),
           # Week 53 turns up in a handful of years and joins week 52.
           week_of_year = pmin(isoweek(saturday), 52L))

  windows <- bind_cols(tests, safe_window(tests$match_date, all_fix_dates(fx)))
  weekday_windows <- windows |> filter(!weekend, !is.na(window_start), !is.na(window_end))
  overlaps <- vapply(seq_len(nrow(weekends)), function(i) {
    any(weekday_windows$window_start < weekends$date[i] &
          weekday_windows$window_end > weekends$prev_date[i])
  }, logical(1))

  weekend_tests <- windows |>
    filter(weekend, !is.na(window_end)) |>
    select(window_end, match_date, opponent, sa_score, opp_score, margin, result,
           rwc, rwc_year, knockout = knockout_by_rule, vs_nz, corrected, home)
  if (anyDuplicated(weekend_tests$window_end)) stop("Two weekend Tests share a window")

  # The interval just before the weekend (normally Thursday to Friday), for the
  # falsification test: it closes before kick-off, so it cannot know the result.
  pre <- fx |> select(date, r_zar_pre = r_zar, r_em_pre = r_em)

  weekends |>
    mutate(overlaps_weekday_test = overlaps) |>
    left_join(pre, by = c("prev_date" = "date")) |>
    left_join(weekend_tests, by = c("date" = "window_end")) |>
    mutate(
      outcome = factor(if_else(is.na(result), "No Test", as.character(result)),
                       levels = c("No Test", "Win", "Loss", "Draw")),
      test = as.integer(outcome != "No Test"),
      win = as.integer(outcome == "Win"),
      loss = as.integer(outcome == "Loss"),
      draw = as.integer(outcome == "Draw"),
      away = as.integer(test == 1L & !coalesce(home, TRUE)),
      home_test = as.integer(test == 1L & away == 0L),
      z_zar = r_zar / sd_trail,
      z_em = r_em / sd_trail,
      rwc = coalesce(rwc, FALSE),
      knockout = coalesce(knockout, FALSE),
      vs_nz = coalesce(vs_nz, FALSE)
    )
}

#' The rand and basket returns over the fix interval `k` places from each
#' weekend's own: 0 is the weekend return, -1 the interval before it and 1 the
#' interval after. `dates` are the fixes that close each weekend.
offset_returns <- function(fx, dates, k) {
  fx <- fx |> arrange(date)
  i <- match(dates, fx$date) + k
  i[i < 1 | i > nrow(fx)] <- NA
  tibble(r_zar_k = fx$r_zar[i], r_em_k = fx$r_em[i])
}

#' Weekday Tests, measured over their safe windows (usually two fixes).
weekday_tests <- function(fx, tests) {
  windows <- bind_cols(tests, safe_window(tests$match_date, all_fix_dates(fx))) |>
    filter(!weekend, !is.na(window_start), !is.na(window_end), window_start >= FX_START)
  bind_rows(lapply(seq_len(nrow(windows)), function(i) {
    w <- windows[i, ]
    inside <- fx |> filter(date > w$window_start, date <= w$window_end)
    w |> mutate(r_zar = sum(inside$r_zar), r_em = sum(inside$r_em), n_fixes = nrow(inside))
  }))
}

#' A currency's change between two fix dates, in percent, positive = stronger.
change_between <- function(series, from, to) {
  v <- function(d) series$value[match(d, series$date)]
  -100 * log(v(to) / v(from))
}

#' What the rand and the benchmark did after each World Cup final: to the first
#' fix after the match (Monday), and over the week, to the last fix within six
#' days of the final (Friday). Both run from the last fix before the final.
finals_windows <- function(zar, benchmark, finals = RWC_FINALS_WON, beta = 1) {
  zar <- zar |> filter(!is.na(value))
  dates <- sort(zar$date)
  bind_rows(lapply(finals, function(f) {
    start <- max(dates[dates < f])
    monday <- min(dates[dates > f])
    week <- max(dates[dates <= f + 6L])
    em <- function(to) {
      mean(vapply(benchmark, function(s) change_between(filter(s, !is.na(value)), start, to), numeric(1)),
           na.rm = TRUE)
    }
    tibble(
      final = f, start = start, monday = monday, week_end = week,
      zar_monday = change_between(zar, start, monday), em_monday = em(monday),
      zar_week = change_between(zar, start, week), em_week = em(week)
    ) |>
      mutate(ar_monday = zar_monday - beta * em_monday, ar_week = zar_week - beta * em_week)
  }))
}

#' The rand and the basket at each fix from the last one before a final to the
#' last one within six days of it, as cumulative changes in percent.
final_week_path <- function(zar, benchmark, final, beta = 1) {
  zar <- zar |> filter(!is.na(value))
  benchmark <- lapply(benchmark, function(s) filter(s, !is.na(value)))
  start <- max(zar$date[zar$date < final])
  dates <- zar$date[zar$date > final & zar$date <= final + 6L]
  tibble(
    date = dates,
    zar_cum = vapply(dates, function(d) change_between(zar, start, d), numeric(1)),
    em_cum = vapply(dates, function(d) {
      mean(vapply(benchmark, function(s) change_between(s, start, d), numeric(1)), na.rm = TRUE)
    }, numeric(1))
  ) |>
    mutate(ar_cum = zar_cum - beta * em_cum)
}

# ---- Spending -----------------------------------------------------------------

#' Read a Stats SA time-series workbook (one row per series, one column per
#' month, MOmmyyyy) into one row per series and month.
read_statssa_timeseries <- function(path) {
  x <- readxl::read_excel(path, sheet = 1, col_types = "text")
  x |>
    select(code = H03, starts_with("MO")) |>
    pivot_longer(starts_with("MO"), names_to = "mo", values_to = "value") |>
    mutate(date = as.Date(sprintf("%s-%s-01", substr(mo, 5, 8), substr(mo, 3, 4))),
           value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    select(code, date, value)
}

#' Easter Sunday in the Gregorian calendar (the anonymous algorithm published
#' by Meeus, after Jones and Butcher).
easter_sunday <- function(year) {
  a <- year %% 19
  b <- year %/% 100
  c <- year %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day <- ((h + l - 7 * m + 114) %% 31) + 1
  as.Date(sprintf("%d-%02d-%02d", year, month, day))
}

#' Calendar composition of each month: days, Fridays, Saturdays and Sundays,
#' and whether Good Friday falls in it. South Africa's other public holidays
#' fall in the same month every year, so month effects absorb them; Easter
#' moves between March and April.
monthly_calendar <- function(months) {
  bind_rows(lapply(months, function(m) {
    days <- seq(m, m %m+% months(1) - 1L, by = "day")
    wd <- wday(days, week_start = 1)
    tibble(date = m, days = length(days), fridays = sum(wd == 5), saturdays = sum(wd == 6),
           sundays = sum(wd == 7),
           easter = as.integer(floor_date(easter_sunday(year(m)) - 2L, "month") == m))
  }))
}

#' Springbok Tests counted by month, overall and by kind. `home` on the way in
#' says whether each Test was played in South Africa (see add_home()); on the
#' way out it counts them.
monthly_tests <- function(tests) {
  tests |>
    mutate(date = floor_date(match_date, "month"), at_home = home) |>
    group_by(date) |>
    summarise(tests = n(), wins = sum(result == "Win"), losses = sum(result == "Loss"),
              draws = sum(result == "Draw"), home = sum(at_home), away = sum(!at_home),
              rwc_knockouts = sum(knockout_by_rule), .groups = "drop")
}

#' One row per month: log real income by type, calendar controls and Tests.
spending_panel <- function(statssa, tests, series = SPENDING_SERIES, covid = COVID_MONTHS) {
  wide <- statssa |>
    filter(code %in% series) |>
    mutate(series = names(series)[match(code, series)]) |>
    select(series, date, value) |>
    pivot_wider(names_from = series, values_from = value)
  counts <- monthly_tests(tests)
  wide |>
    left_join(monthly_calendar(wide$date), by = "date") |>
    left_join(counts, by = "date") |>
    mutate(across(c(tests, wins, losses, draws, home, away, rwc_knockouts), ~ coalesce(.x, 0L)),
           year = year(date), month = month(date),
           covid = date >= covid[1] & date <= covid[2],
           fifa_jun_2010 = as.integer(date == FIFA_2010_MONTHS[1]),
           fifa_jul_2010 = as.integer(date == FIFA_2010_MONTHS[2]),
           tests_next = lead(tests, default = NA_integer_)) |>
    arrange(date)
}

# ---- Output -------------------------------------------------------------------

#' Quarterly real GDP growth, quarter on quarter, in percent (log difference).
gdp_growth <- function(gdp) {
  gdp |>
    arrange(date) |>
    mutate(growth = 100 * (log(value) - log(lag(value))),
           quarter = paste0(year(date), "Q", quarter(date))) |>
    filter(!is.na(growth))
}

#' One row per quarter: real growth with the Springbok Tests played in it,
#' overall and by result, and the Tests of the quarter after as a placebo.
quarterly_panel <- function(growth, tests) {
  counts <- tests |>
    mutate(date = floor_date(match_date, "quarter")) |>
    group_by(date) |>
    summarise(tests = n(), wins = sum(result == "Win"), losses = sum(result == "Loss"),
              draws = sum(result == "Draw"), .groups = "drop")
  growth |>
    left_join(counts, by = "date") |>
    mutate(across(c(tests, wins, losses, draws), ~ coalesce(.x, 0L)),
           year = year(date), quarter_of_year = quarter(date)) |>
    arrange(date) |>
    mutate(tests_next = lead(tests))
}

#' Which quarters a World Cup final falls in, and the quarters after them.
rwc_quarters <- function(finals = RWC_FINALS_WON) {
  q <- floor_date(finals, "quarter")
  tibble(final = finals, final_quarter = q, next_quarter = q %m+% months(3))
}
