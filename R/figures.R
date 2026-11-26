# Figures. One axis each, a recessive grid, and colour that follows the entity:
# Springbok green for wins, gold for losses, grey for context. The pair passes
# the colour-vision and contrast checks of the dataviz palette validator
# (CVD Delta E 9.6, normal-vision Delta E 20.8, both above 3:1 on white).
# Where a figure shows the paper's two contrasts side by side, "a Test is
# played" is ink and "won rather than lost" is green, with different marks.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

COL_WIN <- "#007A4D"
COL_LOSS <- "#b8800a"
COL_CONTEXT <- "#8a8984"
COL_INK <- "#333333"

#: The paper's two contrasts, in the order they are asked.
CONTRAST_PLAYED <- "A Test is played"
CONTRAST_WON <- "Won rather than lost"

theme_paper <- function() {
  cqThemes::theme_cq_pdf(base_size = 11) +
    theme(
      panel.grid.major = element_line(colour = "#ececec", linewidth = 0.3),
      panel.border = element_blank(),
      plot.margin = margin(6, 10, 6, 6),
      axis.title = element_text(size = 10, colour = COL_INK),
      plot.title = element_blank()
    )
}

#' Weekend rand returns net of the benchmark, by result: every weekend as a
#' dot, with the group mean and its 95% confidence interval.
fig_weekend_returns <- function(panel, beta) {
  d <- panel |>
    filter(outcome != "Draw") |>
    mutate(ar = 100 * (r_zar - beta * r_em),
           outcome = factor(outcome, levels = c("Loss", "Win", "No Test")))
  summ <- d |>
    group_by(outcome) |>
    summarise(n = n(), mean = mean(ar), se = sd(ar) / sqrt(n), .groups = "drop") |>
    mutate(lo = mean - qnorm(0.975) * se, hi = mean + qnorm(0.975) * se,
           label = sprintf("%s (n = %d)", outcome, n))
  labels <- setNames(summ$label, summ$outcome)
  cols <- c(Win = COL_WIN, Loss = COL_LOSS, `No Test` = COL_CONTEXT)
  set.seed(SEED)
  ggplot(d, aes(x = ar, y = outcome, colour = outcome)) +
    geom_vline(xintercept = 0, colour = "#bdbdbd", linewidth = 0.4) +
    geom_jitter(height = 0.22, width = 0, size = 0.9, alpha = 0.35, shape = 16) +
    geom_errorbar(data = summ, aes(xmin = lo, xmax = hi, y = outcome, x = NULL),
                  width = 0.25, orientation = "y", colour = COL_INK, linewidth = 0.6) +
    geom_point(data = summ, aes(x = mean, y = outcome), colour = COL_INK, size = 2.4) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_y_discrete(labels = labels) +
    labs(x = "Rand return net of emerging-market basket, Friday to Monday (basis points)", y = NULL) +
    theme_paper()
}

#' The paper's two contrasts side by side for each row. `rows` carries a
#' `label`, in the order to show from top to bottom, a `contrast` and an
#' estimate with its interval, already in the units of `xlab`.
fig_contrasts <- function(rows, xlab) {
  rows <- rows |>
    mutate(label = factor(label, levels = rev(unique(label))),
           contrast = factor(contrast, levels = c(CONTRAST_PLAYED, CONTRAST_WON)))
  dodge <- position_dodge2(width = 0.6, reverse = TRUE)
  ggplot(rows, aes(x = estimate, y = label, colour = contrast, shape = contrast)) +
    geom_vline(xintercept = 0, colour = "#bdbdbd", linewidth = 0.4) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.3, orientation = "y",
                  linewidth = 0.5, position = dodge) +
    geom_point(size = 2.5, position = dodge) +
    scale_colour_manual(values = setNames(c(COL_INK, COL_WIN), c(CONTRAST_PLAYED, CONTRAST_WON)), name = NULL) +
    scale_shape_manual(values = setNames(c(16, 17), c(CONTRAST_PLAYED, CONTRAST_WON)), name = NULL) +
    labs(x = xlab, y = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")
}

#' Where the observed estimate sits among estimates from shuffled labels.
fig_randomisation <- function(draws, observed, xlab) {
  d <- data.frame(x = 100 * draws)
  ggplot(d, aes(x = x)) +
    geom_histogram(bins = 60, fill = COL_CONTEXT, colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = 100 * observed, colour = COL_WIN, linewidth = 0.9) +
    annotate("text", x = 100 * observed, y = Inf, vjust = 1.5, hjust = -0.08, size = 3.4,
             colour = COL_INK, label = sprintf("Observed: %.1f bp", 100 * observed)) +
    labs(x = xlab, y = "Shuffles") +
    theme_paper()
}

#' The effect of one more Springbok Test in a month beside the effect of the
#' 2010 football World Cup, for each spending series, on one axis.
fig_spending <- function(rows) {
  rows <- rows |>
    mutate(outcome = factor(outcome, levels = rev(unique(outcome))),
           what = factor(what, levels = c("One more Springbok Test", "June 2010 (football World Cup)")))
  ggplot(rows, aes(x = estimate, y = outcome, colour = what, shape = what)) +
    geom_vline(xintercept = 0, colour = "#bdbdbd", linewidth = 0.4) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.25, orientation = "y",
                  linewidth = 0.5, position = position_dodge(width = 0.55)) +
    geom_point(size = 2.6, position = position_dodge(width = 0.55)) +
    scale_colour_manual(values = c(`One more Springbok Test` = COL_WIN,
                                   `June 2010 (football World Cup)` = COL_LOSS), name = NULL) +
    scale_shape_manual(values = c(`One more Springbok Test` = 16, `June 2010 (football World Cup)` = 17),
                       name = NULL) +
    guides(colour = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
    labs(x = "Effect on the month's real sales (percent, 95% CI)", y = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")
}

#' Quarterly real GDP growth, with the quarters holding a World Cup final and
#' the quarters after them marked. The axis is cut at +/- 3% so 2020 does not
#' flatten everything else; the two quarters beyond it are labelled.
fig_gdp <- function(growth, rq, limit = 3) {
  marks <- bind_rows(
    tibble(date = rq$final_quarter, which = "Quarter of the final"),
    tibble(date = rq$next_quarter, which = "Quarter after the final")
  ) |>
    mutate(which = factor(which, levels = c("Quarter of the final", "Quarter after the final"))) |>
    left_join(growth, by = "date")
  off <- growth |>
    filter(abs(growth) > limit) |>
    mutate(y = sign(growth) * limit, label = sprintf("%s: %+.1f%%", quarter, growth))
  ggplot(growth, aes(x = date, y = growth)) +
    geom_hline(yintercept = 0, colour = "#bdbdbd", linewidth = 0.4) +
    geom_line(colour = COL_CONTEXT, linewidth = 0.5) +
    geom_point(data = marks, aes(colour = which, shape = which), size = 2.8) +
    geom_text(data = off, aes(y = y, label = label), size = 3, hjust = -0.08,
              vjust = ifelse(off$growth > 0, 1.2, -0.4), colour = COL_INK) +
    scale_colour_manual(values = c(`Quarter of the final` = COL_WIN, `Quarter after the final` = COL_LOSS),
                        name = NULL) +
    scale_shape_manual(values = c(`Quarter of the final` = 16, `Quarter after the final` = 17), name = NULL) +
    coord_cartesian(ylim = c(-limit, limit)) +
    labs(x = NULL, y = "Real GDP growth, quarter on quarter (%)") +
    theme_paper() +
    theme(legend.position = "bottom")
}

#' The rand against the fitted values of a regression of its level on the
#' running count of World Cup titles.
fig_levels <- function(annual, fit) {
  d <- annual |> mutate(fitted = exp(fitted(fit)))
  titles <- d |> filter(titles != lag(titles, default = 0))
  ggplot(d, aes(x = year)) +
    geom_step(aes(y = fitted, colour = "Fitted: titles won"), linewidth = 0.9, direction = "hv") +
    geom_line(aes(y = zar, colour = "Rand per US dollar, annual mean"), linewidth = 0.7) +
    geom_point(aes(y = zar, colour = "Rand per US dollar, annual mean"), size = 1.4) +
    annotate("text", x = titles$year, y = max(d$zar, d$fitted) * 1.07, label = titles$year,
             size = 3, colour = COL_WIN) +
    scale_colour_manual(values = c(`Rand per US dollar, annual mean` = COL_INK,
                                   `Fitted: titles won` = COL_WIN), name = NULL) +
    scale_y_continuous(labels = label_number(prefix = "R", accuracy = 1)) +
    labs(x = NULL, y = "Rand per US dollar") +
    theme_paper() +
    theme(legend.position = "bottom")
}
