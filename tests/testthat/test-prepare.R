# 2024-06-06 is a Thursday; the 7th a Friday, the 10th a Monday.
fix_dates <- c("2024-06-06", "2024-06-07", "2024-06-10", "2024-06-11")

test_that("a stronger rand is a positive return", {
  fx <- toy_fx(fix_dates, zar = c(18, 18, 18 * exp(-0.01), 18 * exp(-0.01)))
  expect_equal(fx$r_zar[fx$date == as.Date("2024-06-10")], 1, tolerance = 1e-12)
})

test_that("benchmark returns use the same pair of dates and skip missing fixes", {
  zar <- tibble::tibble(date = as.Date(fix_dates), value = c(18, 18, 18, 18))
  bm <- list(
    A = tibble::tibble(date = as.Date(fix_dates), value = c(5, 5, 5 * exp(0.02), 5 * exp(0.02))),
    B = tibble::tibble(date = as.Date(fix_dates), value = c(7, NA, 7, 7))
  )
  fx <- fx_returns(zar, bm, start = as.Date("2024-06-06"))
  monday <- fx[fx$date == as.Date("2024-06-10"), ]
  # B has no Friday fix, so only A counts for Friday-to-Monday.
  expect_equal(monday$n_em, 1)
  expect_equal(monday$r_em, -2, tolerance = 1e-12)
})

test_that("the safe window brackets the match date whatever the kick-off time", {
  w <- safe_window(as.Date(c("2024-06-08", "2024-06-07")), as.Date(fix_dates))
  expect_equal(w$window_start, as.Date(c("2024-06-07", "2024-06-06")))
  expect_equal(w$window_end, as.Date(c("2024-06-10", "2024-06-10")))
})

test_that("a Saturday Test lands on the Friday-to-Monday weekend", {
  fx <- toy_fx(fix_dates, zar = c(18, 18.1, 18.2, 18.3))
  panel <- weekend_panel(fx, toy_tests("2024-06-08", "Win"), vol_window = 2L)
  expect_equal(nrow(panel), 1)
  expect_equal(panel$prev_date, as.Date("2024-06-07"))
  expect_equal(panel$date, as.Date("2024-06-10"))
  expect_equal(as.character(panel$outcome), "Win")
  expect_false(panel$overlaps_weekday_test)
  # The falsification window is the Thursday-to-Friday return.
  expect_equal(panel$r_zar_pre, fx$r_zar[fx$date == as.Date("2024-06-07")])
})

test_that("a Friday Test takes its weekend out of the clean sample", {
  fx <- toy_fx(fix_dates, zar = c(18, 18.1, 18.2, 18.3))
  panel <- weekend_panel(fx, toy_tests("2024-06-07", "Loss"), vol_window = 2L)
  expect_true(panel$overlaps_weekday_test)
  expect_equal(as.character(panel$outcome), "No Test")
})

test_that("a US holiday Monday stretches the weekend to Tuesday", {
  dates <- c("2024-05-23", "2024-05-24", "2024-05-28") # Memorial Day 27 May
  fx <- toy_fx(dates, zar = c(18, 18.1, 18.2))
  panel <- weekend_panel(fx, toy_tests("2024-05-26", "Win"), vol_window = 1L)
  expect_equal(panel$date, as.Date("2024-05-28"))
  expect_equal(as.character(panel$outcome), "Win")
})

wr_row <- function(date, opponent, sa, opp, phase = NA, phase_type = NA, competition = "Tour") {
  tibble::tibble(wr_match_id = paste(date, opponent), match_date = as.Date(date),
                 opponent = opponent, sa_score = as.integer(sa), opp_score = as.integer(opp),
                 status = "C", competition = competition, phase = phase,
                 phase_type = phase_type, venue_country = NA_character_)
}

test_that("corrections apply, and a stale correction stops the build", {
  wr <- dplyr::bind_rows(wr_row("2025-08-16", "Australia", 27, 38), wr_row("2025-08-23", "Barbarians", 50, 0))
  corr <- tibble::tibble(match_date = "2025-08-16", opponent = "Australia",
                         world_rugby_sa_score = 27L, world_rugby_opp_score = 38L,
                         sa_score = 22L, opp_score = 38L)
  tests <- springbok_tests(wr, corr)
  expect_equal(nrow(tests), 1) # the Barbarians are not a Test
  expect_equal(tests$sa_score, 22)
  expect_true(tests$corrected)
  corr$world_rugby_sa_score <- 30L
  expect_error(springbok_tests(wr, corr), "no longer matches")
})

test_that("World Cup knockouts follow the format rule", {
  wr <- dplyr::bind_rows(
    wr_row("1995-05-25", "Australia", 27, 18, competition = "Men's Rugby World Cup 1995"),
    wr_row("1995-05-30", "Romania", 21, 8, competition = "Men's Rugby World Cup 1995"),
    wr_row("1995-06-03", "Canada", 20, 0, competition = "Men's Rugby World Cup 1995"),
    wr_row("1995-06-10", "Samoa", 42, 14, competition = "Men's Rugby World Cup 1995")
  )
  tests <- springbok_tests(wr, tibble::tibble(match_date = character(), opponent = character(),
                                              world_rugby_sa_score = integer(), world_rugby_opp_score = integer(),
                                              sa_score = integer(), opp_score = integer()))
  expect_equal(tests$knockout_by_rule, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("the cross-check pairs Tests a day apart and reports disagreements", {
  wr_tests <- dplyr::bind_rows(wr_row("2010-06-12", "France", 42, 17), wr_row("2010-06-19", "Italy", 29, 13))
  db <- tibble::tibble(match_date = c("2010-06-13", "2010-06-19"), home_team = "South Africa",
                       away_team = c("France", "Italy"), home_score = c(42, 30), away_score = c(17, 13),
                       data_source = c("espn", "world_rugby"))
  cc <- crosscheck_tests(wr_tests, db)
  expect_equal(cc$n_paired, 2)
  expect_equal(cc$date_offsets, 1)
  expect_equal(nrow(cc$score_disagreements), 1)
  # Each pair keeps the database's record of where its row came from.
  expect_equal(cc$pairs$data_source[cc$pairs$opponent == "Italy"], "world_rugby")
})

test_that("World Rugby's pages become a facts table that reads back unchanged", {
  dir <- withr::local_tempdir()
  page <- list(content = list(
    list(matchId = 101, time = list(label = "2023-10-28", millis = 1698519600000),
         teams = list(list(name = "New Zealand"), list(name = "South Africa")), scores = list(11, 12),
         status = "C", competition = "Men's Rugby World Cup 2023", eventPhase = "Final",
         eventPhaseId = list(type = "Final"), venue = list(country = "France")),
    list(matchId = 102, time = list(label = "1992-08-15", millis = 713890800000),
         teams = list(list(name = "South Africa"), list(name = "Pontypridd")), scores = list(50, 0),
         status = "C", competition = "Tour 1992", eventPhase = "Pool ")
  ))
  jsonlite::write_json(page, file.path(dir, "springboks_2023_p0.json"), auto_unbox = TRUE)
  facts <- extract_world_rugby(dir)
  path <- file.path(dir, "facts.csv")
  readr::write_csv(facts, path, na = "")
  back <- read_world_rugby(path)
  expect_equal(as.data.frame(back), as.data.frame(facts), ignore_attr = TRUE)
  expect_equal(attr(back$kickoff_utc, "tzone"), "UTC")
  # Scores are from the Springboks' side whichever team World Rugby lists first.
  expect_equal(back$sa_score, c(50L, 12L))
  expect_equal(back$opp_score, c(0L, 11L))
  # A missing venue stays missing, and a stray space in a label survives.
  expect_true(is.na(back$venue_country[1]))
  expect_equal(back$phase[1], "Pool ")
})

test_that("randomisation inference is reproducible and calibrated under the null", {
  set.seed(1)
  n <- 400
  d <- tibble::tibble(year = rep(1:10, each = 40), month = rep(1:4, 100),
                      win = rbinom(n, 1, 0.2), loss = 0L, draw = 0L, r = rnorm(n))
  d$loss <- as.integer(d$win == 0 & runif(n) < 0.1)
  ct <- list(Win = c(win = 1))
  a <- randomisation_test(r ~ win + loss + draw, d, ct, n_perm = 200, seed = 7)
  b <- randomisation_test(r ~ win + loss + draw, d, ct, n_perm = 200, seed = 7)
  expect_identical(a$p_value, b$p_value)
  expect_gt(a$p_value[["Win"]], 0.01)
})

test_that("the minimum detectable effect is 2.8 standard errors at 5% and 80% power", {
  expect_equal(mde(1), qnorm(0.975) + qnorm(0.8))
})

test_that("Easter Sunday matches published dates", {
  expect_equal(easter_sunday(c(2010, 2019, 2024, 2025)),
               as.Date(c("2010-04-04", "2019-04-21", "2024-03-31", "2025-04-20")))
})

test_that("the calendar counts weekend days and finds Good Friday's month", {
  cal <- monthly_calendar(as.Date(c("2023-09-01", "2023-10-01", "2024-03-01", "2024-02-01")))
  expect_equal(cal$saturdays, c(5L, 4L, 5L, 4L))
  expect_equal(cal$easter, c(0L, 0L, 1L, 0L)) # Good Friday 2024 was 29 March
  expect_equal(cal$days, c(30L, 31L, 31L, 29L))
})

test_that("Tests are counted by month and kind", {
  tests <- toy_tests(c("2023-10-15", "2023-10-21", "2023-10-28", "2023-11-04"), c("Win", "Win", "Win", "Loss"))
  tests$home <- c(FALSE, FALSE, FALSE, TRUE)
  tests$knockout_by_rule <- c(TRUE, TRUE, TRUE, FALSE)
  m <- monthly_tests(tests)
  expect_equal(m$tests, c(3L, 1L))
  expect_equal(m$wins, c(3L, 0L))
  expect_equal(m$home, c(0L, 1L))
  expect_equal(m$away, c(3L, 0L))
  expect_equal(m$rwc_knockouts, c(3L, 0L))
})

test_that("a month with home and away Tests counts each kind", {
  tests <- toy_tests(c("2022-07-02", "2022-07-09", "2022-07-16"), c("Win", "Loss", "Win"))
  tests$home <- c(TRUE, TRUE, FALSE)
  m <- monthly_tests(tests)
  expect_equal(m$home, 2L)
  expect_equal(m$away, 1L)
})

test_that("the venue decides home Tests and the second database fills gaps", {
  tests <- tibble::tibble(
    match_date = as.Date(c("1997-11-15", "1998-06-13", "2019-11-02")),
    opponent = c("France", "Ireland", "England"),
    venue_country = c(NA, "South Africa", "Japan")
  )
  pairs <- tibble::tibble(match_date = tests$match_date, opponent = tests$opponent,
                          db_sa_home = c(FALSE, TRUE, TRUE))
  # The final in Japan is not a home Test, whatever side the database lists first.
  expect_equal(add_home(tests, pairs)$home, c(FALSE, TRUE, FALSE))
  pairs$db_sa_home[1] <- NA
  expect_error(add_home(tests, pairs), "No venue in either source")
})

test_that("an away Test is flagged on its weekend and nowhere else", {
  dates <- c("2024-06-06", "2024-06-07", "2024-06-10", "2024-06-13", "2024-06-14", "2024-06-17")
  fx <- toy_fx(dates, zar = c(18, 18.1, 18.2, 18.3, 18.2, 18.1))
  tests <- toy_tests("2024-06-08", "Loss")
  tests$home <- FALSE
  panel <- weekend_panel(fx, tests, vol_window = 2L)
  expect_equal(panel$test, c(1L, 0L))
  expect_equal(panel$away, c(1L, 0L))
})

test_that("a home Test is flagged as one and gets its week of the year", {
  dates <- c("2024-06-06", "2024-06-07", "2024-06-10", "2024-06-13", "2024-06-14", "2024-06-17")
  fx <- toy_fx(dates, zar = c(18, 18.1, 18.2, 18.3, 18.2, 18.1))
  panel <- weekend_panel(fx, toy_tests("2024-06-08", "Win"), vol_window = 2L)
  expect_equal(panel$home_test, c(1L, 0L))
  expect_equal(panel$away, c(0L, 0L))
  expect_equal(panel$week_of_year, c(23, 24)) # ISO weeks of 8 and 15 June 2024
})

test_that("Tests are counted by quarter, with the next quarter's as a lead", {
  growth <- tibble::tibble(date = as.Date(c("2023-07-01", "2023-10-01", "2024-01-01")),
                           growth = c(0.1, 0.2, 0.3), quarter = c("2023Q3", "2023Q4", "2024Q1"))
  tests <- toy_tests(c("2023-09-10", "2023-10-15", "2023-10-28", "2022-11-05"), c("Win", "Win", "Loss", "Win"))
  q <- quarterly_panel(growth, tests)
  expect_equal(q$tests, c(1L, 2L, 0L)) # the 2022 Test falls outside the growth sample
  expect_equal(q$wins, c(1L, 1L, 0L))
  expect_equal(q$losses, c(0L, 1L, 0L))
  expect_equal(q$tests_next, c(2L, 0L, NA))
  expect_equal(q$quarter_of_year, c(3, 4, 1))
})

test_that("shuffling the Test indicator itself is reproducible and moves it", {
  set.seed(6)
  n <- 240
  d <- tibble::tibble(year = rep(1:6, each = 40), month = rep(1:4, 60), r = rnorm(n),
                      test = rbinom(n, 1, 0.3))
  ct <- list(`Test played` = c(test = 1))
  a <- randomisation_test(r ~ test, d, ct, n_perm = 100, seed = 11, labels = "test")
  b <- randomisation_test(r ~ test, d, ct, n_perm = 100, seed = 11, labels = "test")
  expect_identical(a$p_value, b$p_value)
  expect_gt(sd(a$draws), 0)
  expect_equal(unname(a$observed), unname(coef(lm(r ~ test, data = d))["test"]))
})

test_that("offset returns step along the fixes from each weekend's close", {
  dates <- c("2024-06-05", "2024-06-06", "2024-06-07", "2024-06-10", "2024-06-11")
  fx <- toy_fx(dates, zar = c(18, 18.1, 18.3, 18.6, 19))
  monday <- as.Date("2024-06-10")
  at <- function(k) offset_returns(fx, monday, k)$r_zar_k
  expect_equal(at(0), fx$r_zar[fx$date == monday])
  expect_equal(at(-1), fx$r_zar[fx$date == as.Date("2024-06-07")])
  expect_equal(at(1), fx$r_zar[fx$date == as.Date("2024-06-11")])
  expect_true(is.na(at(2))) # past the end of the series
})

test_that("the week after a final accumulates from the fix before it", {
  dates <- as.Date(c("2007-10-19", "2007-10-22", "2007-10-23", "2007-10-26", "2007-10-29"))
  zar <- tibble::tibble(date = dates, value = 7 * exp(-c(0, 0.01, 0.02, 0.04, 0.05)))
  flat <- list(A = tibble::tibble(date = dates, value = 5))
  path <- final_week_path(zar, flat, as.Date("2007-10-20"))
  expect_equal(path$date, dates[2:4]) # the 29th is more than six days after the final
  expect_equal(path$zar_cum, c(1, 2, 4), tolerance = 1e-12)
  expect_equal(path$ar_cum, path$zar_cum)
})

test_that("winsorising pulls the tails in and leaves the middle alone", {
  x <- c(-100, 1:98, 1000)
  w <- winsorise(x, tail = 0.01)
  expect_equal(w[2:99], x[2:99])
  expect_lt(max(w), 1000)
  expect_gt(min(w), -100)
})

test_that("a one-month indicator gets the prediction error, not a sandwich error", {
  set.seed(3)
  d <- data.frame(x = rnorm(60), e = rnorm(60))
  d$one <- as.integer(seq_len(60) == 20)
  d$y <- 1 + d$x + d$e
  fit <- lm(y ~ x + one, data = d)
  expect_equal(unname(hatvalues(fit)[20]), 1)
  out <- single_month_effects(fit, list(Month = c(one = 1)))
  # The classical error is at least the residual standard deviation.
  expect_gt(out$se, sigma(fit))
  # Newey-West has no residual for that month and comes out far smaller.
  expect_lt(sqrt(vcov_nw(fit)["one", "one"]), out$se / 2)
  # It is the t-test on the observation's deleted residual.
  expect_equal(abs(out$estimate / out$se), abs(unname(rstudent(lm(y ~ x, data = d))[20])), tolerance = 1e-8)
})

test_that("the surprise rank finds the planted outlier", {
  set.seed(4)
  d <- data.frame(x = rnorm(50))
  d$y <- d$x + rnorm(50, sd = 0.5)
  d$y[7] <- d$y[7] + 10
  expect_equal(surprise_rank(y ~ x, d, seq_len(50) == 7), list(rank = 1, n = 50L))
})

test_that("shuffling among Tests leaves weekends without a Test alone", {
  set.seed(5)
  n <- 200
  d <- tibble::tibble(year = rep(1:5, each = 40), month = rep(1:4, 50), test = rbinom(n, 1, 0.4))
  d$win <- as.integer(d$test == 1 & runif(n) < 0.6)
  d$loss <- as.integer(d$test == 1 & d$win == 0)
  d$draw <- 0L
  # A return that depends only on whether a Test was played. While the Test
  # weekends stay put, wins and losses both fit it exactly, however shuffled.
  d$r <- 2 * d$test
  ct <- list(`Win - Loss` = c(win = 1, loss = -1))
  among <- randomisation_test(r ~ win + loss + draw, d, ct, n_perm = 50, seed = 9, strata = "year", among = "test")
  expect_equal(max(abs(among$draws)), 0, tolerance = 1e-10)
  # Shuffled among all weekends, the labels leave the Test weekends and the fit breaks.
  everywhere <- randomisation_test(r ~ win + loss + draw, d, ct, n_perm = 50, seed = 9, strata = "year")
  expect_gt(sd(everywhere$draws), 0.01)
})
