# Everything the paper reports, computed in one place from the raw snapshots.
# The .qmd calls run_analysis() once and only formats what comes back.
#
# One question runs through it: does anything in the economy move when the
# Springboks play? The same regression is fitted on three timescales, the rand over
# a weekend, real sales over a month and real GDP over a quarter, each with the
# number of Tests in the period as the variable of interest ("played") and,
# second, with the Tests split by result ("won rather than lost").

run_analysis <- function(n_perm = N_PERMUTATIONS) {
  verify_manifest()

  # ---- Rugby -----------------------------------------------------------------
  wr <- read_world_rugby()
  corrections <- read_csv("data/corrections/world_rugby_scores.csv", show_col_types = FALSE)
  tests_all <- springbok_tests(wr, corrections)
  n_rwc_labelled <- check_knockout_rule(tests_all)
  finals <- check_rwc_finals(tests_all)

  db <- read_csv("data/raw/crosscheck/rugby_db_springboks.csv", show_col_types = FALSE)
  crosscheck <- crosscheck_tests(wr |> filter(status == "C", opponent %in% TEST_OPPONENTS), db)
  unexplained <- crosscheck$score_disagreements |>
    anti_join(mutate(corrections, match_date = as.Date(match_date)), by = c("match_date", "opponent"))
  if (nrow(crosscheck$only_world_rugby) + nrow(crosscheck$only_db) + nrow(unexplained) > 0) {
    stop("World Rugby and the second database disagree beyond the documented corrections")
  }
  n_venue_filled <- sum(is.na(tests_all$venue_country))
  tests_all <- add_home(tests_all, crosscheck$pairs)

  tests <- tests_all |> filter(match_date >= FX_START)

  # ---- The rand, by weekend --------------------------------------------------
  zar <- read_fred(FX_ZAR)
  benchmark <- lapply(FX_BENCHMARK, read_fred)
  fx <- fx_returns(zar, benchmark)
  panel_all <- weekend_panel(fx, tests)
  panel <- panel_all |> filter(!overlaps_weekday_test)
  tests_priced <- tests |> filter(match_date < max(fx$date))

  played_only <- list(`Test played` = c(test = 1))
  by_result <- list(Win = c(win = 1), Loss = c(loss = 1), `Win - Loss` = c(win = 1, loss = -1))
  difference <- by_result["Win - Loss"]
  fe <- "+ factor(year) + factor(month)"
  # A finer season: an effect for each week of the year, so that a Test weekend
  # is compared with the same calendar week in years it had no Test.
  fe_week <- "+ factor(year) + factor(week_of_year)"
  # Each column of the main table, with the Test indicator and with it split
  # by result.
  columns <- list(
    `(1)` = list(y = "r_zar", rhs = ""),
    `(2)` = list(y = "r_zar", rhs = "+ r_em"),
    `(3)` = list(y = "r_zar", rhs = paste("+ r_em", fe)),
    `(4)` = list(y = "z_zar", rhs = paste("+ z_em", fe))
  )
  played_formula <- function(col) as.formula(paste(col$y, "~ test", col$rhs))
  result_formula <- function(col) as.formula(paste(col$y, "~ win + loss + draw", col$rhs))
  played <- lapply(columns, function(col) fit_contrasts(played_formula(col), panel, played_only))
  main <- lapply(columns, function(col) fit_contrasts(result_formula(col), panel, by_result))
  played_preferred <- played[["(3)"]]
  preferred <- main[["(3)"]]
  f_played <- played_formula(columns[["(3)"]])
  f_result <- result_formula(columns[["(3)"]])

  # Two shuffles, one for each contrast. For "played", the Test indicator moves
  # among the weekends of the same month and year. For "won rather than lost",
  # results move among the Tests of the same year, so that which weekends had a
  # Test stays as it was and only who won them changes.
  ri_played <- randomisation_test(f_played, panel, played_only, n_perm = n_perm, labels = "test")
  ri_result <- randomisation_test(f_result, panel, by_result, n_perm = n_perm,
                                  strata = "year", among = "test")

  first <- year(FX_START)
  last <- year(DATA_END)
  periods <- list(c(first, SUBPERIOD_SPLIT_YEAR - 1L), c(SUBPERIOD_SPLIT_YEAR, last))
  names(periods) <- vapply(periods, paste, character(1), collapse = "-")
  in_period <- function(d, p) filter(d, year >= p[1], year <= p[2])
  subperiods <- lapply(periods, function(p) {
    list(played = fit_contrasts(f_played, in_period(panel, p), played_only),
         result = fit_contrasts(f_result, in_period(panel, p), by_result))
  })

  # ---- Windows either side of the weekend ------------------------------------
  # The interval before each weekend closes before kick-off.
  falsification <- fit_contrasts(
    as.formula(paste("r_zar_pre ~ win + loss + draw + r_em_pre", fe)), panel, by_result
  )

  # Both contrasts in each fix interval around the weekend.
  profile <- bind_rows(lapply(PROFILE_OFFSETS, function(k) {
    d <- bind_cols(panel, offset_returns(fx, panel$date, k))
    bind_rows(
      fit_contrasts(as.formula(paste("r_zar_k ~ test + r_em_k", fe)), d, played_only)$table,
      fit_contrasts(as.formula(paste("r_zar_k ~ win + loss + draw + r_em_k", fe)), d, by_result)$table
    ) |>
      mutate(offset = k)
  }))

  # Test weekends against the rest in the interval before the weekend, and how
  # far that difference survives.
  placebo_fit <- function(d, y = "r_zar_pre", x = "r_em_pre", extra = "", effects = fe, rhs = "test",
                          contrasts = played_only) {
    fit_contrasts(as.formula(paste(y, "~", rhs, "+", x, extra, effects)), d, contrasts)
  }
  placebo_data <- panel |>
    mutate(r_zar_pre_w = winsorise(r_zar_pre), r_em_pre_w = winsorise(r_em_pre),
           cell = interaction(year, month, drop = TRUE)) |>
    bind_cols(offset_returns(fx, panel$date, min(PROFILE_OFFSETS)))
  mixed_cells <- placebo_data |>
    group_by(cell) |>
    filter(n_distinct(test) == 2) |>
    ungroup()
  by_venue <- list(`Home Test` = c(home_test = 1), `Away Test` = c(away = 1),
                   `Home - Away` = c(home_test = 1, away = -1))
  placebo <- c(
    list(preferred = placebo_fit(placebo_data),
         winsorised = placebo_fit(placebo_data, y = "r_zar_pre_w", x = "r_em_pre_w"),
         week_of_year = placebo_fit(placebo_data, effects = fe_week)),
    lapply(periods, function(p) placebo_fit(in_period(placebo_data, p))),
    list(cells = placebo_fit(mixed_cells, effects = "+ factor(cell)"),
         earlier = placebo_fit(placebo_data, y = "r_zar_k", x = "r_em_k"),
         weekend = played_preferred)
  )
  placebo_venue <- placebo_fit(placebo_data, rhs = "home_test + away", contrasts = by_venue)
  # The interval before and the weekend taken together: Thursday to Monday.
  through_weekend <- fit_contrasts(
    as.formula(paste("I(r_zar_pre + r_zar) ~ test + I(r_em_pre + r_em)", fe)), panel, played_only
  )

  # ---- Which Tests: venue, stakes, opponent, margin ---------------------------
  venue <- fit_contrasts(as.formula(paste("r_zar ~ home_test + away + r_em", fe)), panel, by_venue)
  # The weekend contrasts again with week-of-year effects in place of months.
  finer <- list(
    played = fit_contrasts(as.formula(paste("r_zar ~ test + r_em", fe_week)), panel, played_only),
    result = fit_contrasts(as.formula(paste("r_zar ~ win + loss + draw + r_em", fe_week)), panel, by_result),
    venue = fit_contrasts(as.formula(paste("r_zar ~ home_test + away + r_em", fe_week)), panel, by_venue)
  )

  # Subgroups: split the indicators in two so the comparison group (weekends
  # without a Test) and the controls stay the same.
  subgroup <- function(flag, label) {
    d <- panel |>
      mutate(test_in = test * .data[[flag]], test_out = test * !.data[[flag]],
             win_in = win * .data[[flag]], win_out = win * !.data[[flag]],
             loss_in = loss * .data[[flag]], loss_out = loss * !.data[[flag]])
    one <- fit_contrasts(as.formula(paste("r_zar ~ test_in + test_out + r_em", fe)), d,
                         list(`Test played` = c(test_in = 1)))
    two <- fit_contrasts(
      as.formula(paste("r_zar ~ win_in + win_out + loss_in + loss_out + draw + r_em", fe)), d,
      list(Win = c(win_in = 1), Loss = c(loss_in = 1), `Win - Loss` = c(win_in = 1, loss_in = -1))
    )
    list(table = mutate(bind_rows(one$table, two$table), group = label),
         counts = c(test = sum(d$test_in), win = sum(d$win_in), loss = sum(d$loss_in)))
  }
  subgroups <- list(
    subgroup("home_test", "Tests in South Africa"),
    subgroup("away", "Tests elsewhere"),
    subgroup("rwc", "World Cup matches"),
    subgroup("knockout", "World Cup knockouts"),
    subgroup("vs_nz", "Against New Zealand")
  )

  # Wins and losses differ in where and against whom they were played. An
  # away-Test dummy takes out the first; opponent effects the second. Weekends
  # without a Test sit in the reference opponent's group, where every opponent
  # dummy is zero, so the win-minus-loss contrast is unchanged in meaning.
  venue_fit <- fit_contrasts(as.formula(paste("r_zar ~ win + loss + draw + away + r_em", fe)), panel, difference)
  opponent_counts <- sort(table(panel$opponent[panel$test == 1L]), decreasing = TRUE)
  common_opponents <- names(opponent_counts)[opponent_counts >= OPPONENT_MIN_TESTS]
  reference <- common_opponents[1]
  opponent_data <- panel |>
    mutate(opponent_group = relevel(factor(case_when(
      test == 0L ~ reference,
      opponent %in% common_opponents ~ opponent,
      TRUE ~ "Other"
    )), ref = reference))
  opponent_fit <- fit_contrasts(
    as.formula(paste("r_zar ~ win + loss + draw + away + opponent_group + r_em", fe)), opponent_data, difference
  )

  margin_fit <- fit_contrasts(
    as.formula(paste("r_zar ~ test + margin + r_em", fe)),
    panel |> mutate(margin = coalesce(margin, 0)),
    list(margin = c(margin = 1))
  )

  beta_em <- coef(preferred$fit)[["r_em"]]
  # Whether a Test moves the rand at all: the size of the weekend return net of
  # the basket. A Test that pushed the rand up after some matches and down after
  # others would show here and average away in the Test indicator.
  size_data <- panel |> mutate(abs_ar = abs(r_zar - beta_em * r_em))
  size <- list(
    fit = fit_contrasts(as.formula(paste("abs_ar ~ test", fe)), size_data, played_only),
    mean_without = mean(size_data$abs_ar[size_data$test == 0L])
  )
  weekday <- weekday_tests(fx, tests_priced) |>
    mutate(ar = r_zar - beta_em * r_em)
  finals_fx <- finals_windows(zar, benchmark, beta = beta_em)
  # The week of the final that the Standard Bank deal followed, fix by fix.
  icbc_final <- max(RWC_FINALS_WON[RWC_FINALS_WON < ICBC_DEAL_ANNOUNCED])
  icbc_path <- final_week_path(zar, benchmark, icbc_final, beta = beta_em)

  # ---- Spending, by month ----------------------------------------------------
  statssa <- bind_rows(read_statssa_timeseries(STATSSA_FOOD_BEV), read_statssa_timeseries(STATSSA_RETAIL))
  if (anyDuplicated(statssa[c("code", "date")])) stop("A Stats SA series code appears in both workbooks")
  spend_all <- spending_panel(statssa, tests_all)
  spend <- spend_all |>
    filter(!covid) |>
    mutate(y_bar = 100 * log(bar), y_food = 100 * log(food), y_total = 100 * log(total),
           y_specialised = 100 * log(specialised), y_general = 100 * log(general))
  calendar <- "+ fridays + saturdays + sundays + days + easter"
  fifa_terms <- "+ fifa_jun_2010 + fifa_jul_2010"
  month_effects <- "+ factor(year) + factor(month)"
  spend_specs <- list(
    tests = list(rhs = "tests",
                 contrasts = list(`Per Test` = c(tests = 1), `Extra Saturday` = c(saturdays = 1),
                                  `Saturday - Test` = c(saturdays = 1, tests = -1),
                                  `Good Friday month` = c(easter = 1))),
    result = list(rhs = "wins + losses + draws",
                  contrasts = list(`Per win` = c(wins = 1), `Per loss` = c(losses = 1),
                                   `Win - Loss` = c(wins = 1, losses = -1))),
    venue = list(rhs = "home + away",
                 contrasts = list(`Per home Test` = c(home = 1), `Per away Test` = c(away = 1))),
    knockout = list(rhs = "tests + rwc_knockouts",
                    contrasts = list(`World Cup knockout, extra` = c(rwc_knockouts = 1))),
    lead = list(rhs = "tests + tests_next",
                contrasts = list(`Tests next month` = c(tests_next = 1)))
  )
  fifa <- list(`2010 World Cup, June` = c(fifa_jun_2010 = 1), `2010 World Cup, July` = c(fifa_jul_2010 = 1))
  # Newey-West for the Test and calendar terms; classical errors for the two
  # one-month indicators, where a sandwich estimator has nothing to work with.
  fit_spend <- function(outcome, spec, data = spend) {
    f <- as.formula(paste(outcome, "~", spec$rhs, calendar, fifa_terms, month_effects))
    out <- fit_contrasts(f, data, spec$contrasts, vcov_fn = vcov_nw)
    out$table <- rbind(out$table, single_month_effects(out$fit, fifa))
    out
  }
  outcomes <- c(bar = "y_bar", food = "y_food", total = "y_total",
                specialised = "y_specialised", general = "y_general")
  spending <- lapply(outcomes, function(o) {
    lapply(spend_specs, function(s) fit_spend(o, s))
  })
  spending_short <- fit_spend("y_bar", spend_specs$tests,
                              data = filter(spend, date <= SPENDING_ALT_END))
  # Where June 2010 ranks among every month's surprise when nothing marks it.
  fifa_rank <- lapply(outcomes, function(o) {
    d <- spend[!is.na(spend[[o]]), ]
    surprise_rank(as.formula(paste(o, "~ tests", calendar, month_effects)), d,
                  d$date == FIFA_2010_MONTHS[1])
  })
  # What the per-Test estimate rests on: the Test count once the controls and
  # effects are taken out, and how much of that sits in World Cup years.
  bar_months <- spend |> filter(!is.na(y_bar))
  leftover <- residuals(lm(as.formula(paste("tests ~ 1", calendar, fifa_terms, month_effects)), data = bar_months))
  rwc_years <- intersect(sort(unique(na.omit(tests_all$rwc_year))), bar_months$year)
  spend_ident <- list(
    n_months = nrow(bar_months),
    n_without_test = sum(bar_months$tests == 0L),
    months_never = setdiff(1:12, unique(bar_months$month[bar_months$tests > 0L])),
    rwc_years = rwc_years,
    share_rwc_years = sum(leftover[bar_months$year %in% rwc_years]^2) / sum(leftover^2)
  )
  # Recent monthly sales in each series, R million at constant prices, to put a
  # percentage of a month's sales into rand.
  spend_level <- vapply(names(outcomes), function(o) {
    mean(tail(spend[[o]][!is.na(spend[[o]])], SPEND_LEVEL_MONTHS))
  }, numeric(1))

  # ---- Output, by quarter ----------------------------------------------------
  gdp <- read_fred(GDP_SERIES)
  gdp_nominal <- read_fred(GDP_NOMINAL_SERIES)
  growth <- gdp_growth(gdp)
  covid <- GDP_COVID_QUARTERS
  gdp_panel <- quarterly_panel(growth, tests_all)
  gdp_sample <- gdp_panel |> filter(!date %in% covid)
  quarter_effects <- "+ factor(year) + factor(quarter_of_year)"
  fit_gdp <- function(rhs, contrasts, data = gdp_sample) {
    fit_contrasts(as.formula(paste("growth ~", rhs, quarter_effects)), data, contrasts, vcov_fn = vcov_nw)
  }
  per_test <- list(`Per Test` = c(tests = 1))
  gdp_fits <- list(
    tests = fit_gdp("tests", per_test),
    result = fit_gdp("wins + losses + draws",
                     list(`Per win` = c(wins = 1), `Per loss` = c(losses = 1),
                          `Win - Loss` = c(wins = 1, losses = -1))),
    lead = fit_gdp("tests + tests_next", list(`Tests next quarter` = c(tests_next = 1))),
    all_quarters = fit_gdp("tests", per_test, data = gdp_panel)
  )

  # The four titles: growth in the quarters of the finals and the ones after.
  rq <- rwc_quarters()
  gdp_tests <- list(
    final_all = quarter_difference_test(growth$growth, growth$date %in% rq$final_quarter, n_perm),
    next_all = quarter_difference_test(growth$growth, growth$date %in% rq$next_quarter, n_perm),
    final_ex = with(filter(growth, !date %in% covid),
                    quarter_difference_test(growth, date %in% rq$final_quarter, n_perm)),
    next_ex = with(filter(growth, !date %in% covid),
                   quarter_difference_test(growth, date %in% rq$next_quarter, n_perm))
  )

  # ---- The same two contrasts on every outcome -------------------------------
  # Estimates are in percent of the outcome (percentage points of growth for
  # GDP). `sd` is what the fitted model leaves unexplained in an ordinary
  # period, which puts the outcomes on one scale.
  headline_row <- function(key, outcome, clock, played_fit, played_term, result_fit, n_tests) {
    one <- played_fit$table[played_fit$table$term == played_term, ]
    two <- result_fit$table[result_fit$table$term == "Win - Loss", ]
    tibble(key = key, outcome = outcome, clock = clock, periods = played_fit$n, tests = n_tests,
           sd = sigma(played_fit$fit),
           played = one$estimate, played_se = one$se, played_low = one$conf_low,
           played_high = one$conf_high, played_p = one$p_value,
           won = two$estimate, won_se = two$se, won_low = two$conf_low,
           won_high = two$conf_high, won_p = two$p_value)
  }
  spend_tests <- function(o) sum(spend$tests[!is.na(spend[[o]])])
  spend_row <- function(o, label) {
    headline_row(o, label, "Month", spending[[o]]$tests, "Per Test", spending[[o]]$result, spend_tests(o))
  }
  headline <- bind_rows(
    headline_row("rand", "Rand against the dollar", "Weekend", played_preferred, "Test played",
                 preferred, sum(panel$test)),
    spend_row("bar", "Bar sales"),
    spend_row("food", "Food sales"),
    spend_row("specialised", "Butchers, bottle stores"),
    spend_row("general", "General dealers"),
    headline_row("gdp", "Real GDP growth", "Quarter", gdp_fits$tests, "Per Test", gdp_fits$result,
                 sum(gdp_sample$tests))
  )

  # ---- A regression in levels, for the section on what to ignore -------------
  annual <- zar |>
    filter(!is.na(value), date >= FX_START, year(date) < year(DATA_END)) |>
    group_by(year = year(date)) |>
    summarise(zar = mean(value), .groups = "drop") |>
    mutate(titles = vapply(year, function(y) sum(year(RWC_FINALS_WON) <= y), integer(1)))
  levels_fit <- lm(log(zar) ~ titles, data = annual)
  diffs_fit <- lm(diff(log(zar)) ~ diff(titles), data = annual)

  list(
    wr = wr, tests_all = tests_all, tests = tests, tests_priced = tests_priced, finals = finals,
    crosscheck = crosscheck, corrections = corrections, n_rwc_labelled = n_rwc_labelled,
    n_venue_filled = n_venue_filled,
    zar = zar, fx = fx, panel_all = panel_all, panel = panel,
    headline = headline,
    played = played, played_preferred = played_preferred, ri_played = ri_played,
    main = main, preferred = preferred, ri_result = ri_result,
    falsification = falsification, profile = profile, placebo = placebo, placebo_venue = placebo_venue,
    through_weekend = through_weekend, periods = periods,
    subperiods = subperiods, subgroups = subgroups, venue = venue, finer = finer,
    venue_fit = venue_fit, opponent_fit = opponent_fit, common_opponents = common_opponents,
    margin_fit = margin_fit, size = size, weekday = weekday, beta_em = beta_em, finals_fx = finals_fx,
    icbc_path = icbc_path,
    statssa = statssa, spend_all = spend_all, spend = spend, spending = spending,
    spending_short = spending_short, fifa_rank = fifa_rank, spend_ident = spend_ident,
    spend_level = spend_level,
    gdp = gdp, gdp_nominal = gdp_nominal, growth = growth, covid = covid,
    gdp_panel = gdp_panel, gdp_sample = gdp_sample, gdp_fits = gdp_fits,
    rwc_quarters = rq, gdp_tests = gdp_tests,
    annual = annual, levels_fit = levels_fit, diffs_fit = diffs_fit
  )
}
