# Estimation and inference. Weekend regressions use heteroskedasticity-robust
# (HC3) standard errors and the monthly ones Newey-West; one-month indicators
# take classical errors (see single_month_effects()). Randomisation inference
# and Holm adjustment sit on top.

suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
})

#' Minimum detectable effect for a two-sided test at ALPHA with power POWER.
mde <- function(se, alpha = ALPHA, power = POWER) {
  (qnorm(1 - alpha / 2) + qnorm(power)) * se
}

#' Two one-sided tests: the smallest symmetric bound the data can rule out at
#' ALPHA, i.e. the larger absolute end of the 1 - 2*ALPHA confidence interval.
equivalence_bound <- function(est, se, alpha = ALPHA) {
  pmax(abs(est - qnorm(1 - alpha) * se), abs(est + qnorm(1 - alpha) * se))
}

#' Pull each tail of `x` in to its `tail` and `1 - tail` quantiles.
winsorise <- function(x, tail = WINSOR_TAIL) {
  q <- quantile(x, c(tail, 1 - tail), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

vcov_hc3 <- function(fit) vcovHC(fit, type = "HC3")

#' Newey-West standard errors with the automatic bandwidth, for monthly series
#' whose errors are autocorrelated.
vcov_nw <- function(fit) NeweyWest(fit, prewhite = FALSE, adjust = TRUE)

#' Estimate, standard error, confidence interval and p-value for each named
#' contrast of the coefficients `b` with covariance `V`. `df` is the residual
#' degrees of freedom for t-based inference; the default is the normal.
contrast_table <- function(b, V, contrasts, df = Inf) {
  rows <- lapply(names(contrasts), function(label) {
    w <- contrasts[[label]]
    L <- setNames(numeric(length(b)), names(b))
    L[names(w)] <- w
    est <- sum(L * b)
    se <- sqrt(as.numeric(t(L) %*% V %*% L))
    data.frame(term = label, estimate = est, se = se,
               conf_low = est - qt(1 - ALPHA / 2, df) * se,
               conf_high = est + qt(1 - ALPHA / 2, df) * se,
               p_value = 2 * pt(-abs(est / se), df))
  })
  do.call(rbind, rows)
}

#' Fit a model and report each named contrast. `contrasts` maps a label to a
#' coefficient-weight vector, e.g.
#' list(Win = c(win = 1), `Win - Loss` = c(win = 1, loss = -1)).
#' Standard errors are HC3 unless `vcov_fn` says otherwise.
fit_contrasts <- function(formula, data, contrasts, vcov_fn = vcov_hc3) {
  fit <- lm(formula, data = data)
  V <- vcov_fn(fit)
  list(fit = fit, vcov = V, table = contrast_table(coef(fit), V, contrasts), n = nobs(fit))
}

#' Effects of indicators that switch on for a single observation. Such an
#' indicator fits its observation exactly: the residual there is zero and the
#' leverage one, so a sandwich estimator has no residual variance to work with
#' for that month (Newey-West understates the error and HC3 is undefined). The
#' classical standard error is the error of predicting that one observation
#' from the rest, which is the uncertainty that matters, and the test is the
#' t-test on the deleted residual.
single_month_effects <- function(fit, contrasts) {
  contrast_table(coef(fit), vcov(fit), contrasts, df = fit$df.residual)
}

#' Where one observation ranks among all of them by the size of its
#' studentised residual: 1 is the biggest surprise in the sample.
surprise_rank <- function(formula, data, which_row) {
  r <- abs(rstudent(lm(formula, data = data)))
  stopifnot(length(r) == nrow(data), sum(which_row) == 1)
  list(rank = unname(rank(-r)[which_row]), n = length(r))
}

#' Randomisation inference: shuffle the Test labels within strata, refit, and
#' see how often the shuffled contrast is at least as far from zero as the real
#' one. `labels` names the model's label columns: the three results by default,
#' or the single Test indicator when the question is whether a Test was played.
#' By default every weekend is shuffled within its calendar month and year,
#' which keeps the season and the era of each Test fixed, so the null is "what
#' happened this weekend did not matter", not "Tests are random". `among` names
#' a 0/1 column that limits the shuffle to its rows: shuffling among Test
#' weekends only also holds fixed which weekends had a Test.
randomisation_test <- function(formula, data, contrasts, n_perm = N_PERMUTATIONS,
                               seed = SEED, strata = c("year", "month"), among = NULL,
                               labels = c("win", "loss", "draw")) {
  set.seed(seed)
  X <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  keep <- as.integer(rownames(X))
  d <- data[keep, ]
  eligible <- if (is.null(among)) seq_len(nrow(d)) else which(d[[among]] == 1)
  groups <- split(eligible, interaction(d[eligible, strata], drop = TRUE))
  held <- as.matrix(d[, labels])
  observed <- contrast_values(lm.fit(X, y)$coefficients, contrasts)
  draws <- matrix(NA_real_, n_perm, length(contrasts), dimnames = list(NULL, names(contrasts)))
  for (k in seq_len(n_perm)) {
    perm <- seq_len(nrow(d))
    for (g in groups) if (length(g) > 1) perm[g] <- g[sample.int(length(g))]
    X[, labels] <- held[perm, , drop = FALSE]
    draws[k, ] <- contrast_values(lm.fit(X, y)$coefficients, contrasts)
  }
  p <- vapply(names(contrasts), function(nm) {
    (1 + sum(abs(draws[, nm]) >= abs(observed[[nm]]))) / (1 + n_perm)
  }, numeric(1))
  list(observed = observed, draws = draws, p_value = p)
}

contrast_values <- function(b, contrasts) {
  vapply(contrasts, function(w) sum(w * b[names(w)]), numeric(1))
}

#' Difference in mean growth between a set of treated quarters and the rest,
#' with a Monte Carlo randomisation p-value over equally sized random sets.
quarter_difference_test <- function(growth, treated, n_perm = N_PERMUTATIONS, seed = SEED) {
  set.seed(seed)
  k <- sum(treated)
  observed <- mean(growth[treated]) - mean(growth[!treated])
  draws <- replicate(n_perm, {
    idx <- sample.int(length(growth), k)
    mean(growth[idx]) - mean(growth[-idx])
  })
  se <- sqrt(var(growth[treated]) / k + var(growth[!treated]) / sum(!treated))
  pooled_se <- sd(growth) * sqrt(1 / k + 1 / sum(!treated))
  list(observed = observed, p_value = (1 + sum(abs(draws) >= abs(observed))) / (1 + n_perm),
       draws = draws, k = k, mde = mde(pooled_se), welch_se = se)
}
