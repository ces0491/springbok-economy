# object_usage_linter is off: the pipeline is sourced scripts using dplyr's
# column names, so it reports every column and every cross-file function as an
# undefined global (357 false positives on 25 November 2026, none real).
linters <- lintr::linters_with_defaults(
  line_length_linter(120),
  object_name_linter(styles = c("snake_case", "symbols", "SNAKE_CASE")),
  object_usage_linter = NULL
)
exclusions <- list("renv")
