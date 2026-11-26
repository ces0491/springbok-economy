# Download every public raw input and record where it came from.
#
# Run from the project root:   Rscript R/fetch_data.R
#
# The paper never touches the network. It renders from the snapshots this
# script writes to data/raw/, and checks them against data/raw/MANIFEST.csv
# before using them. For World Rugby the snapshot is the table of match facts
# extracted from its pages; the pages themselves stay local. Re-running this script replaces the snapshots; FRED
# revises GDP and World Rugby edits old entries, so a re-fetch can change
# results, and the manifest diff shows which file moved.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

source("R/config.R")
source("R/manifest.R")
source("R/prepare.R")

RAW_DIR <- "data/raw"
dir.create(file.path(RAW_DIR, "fred"), recursive = TRUE, showWarnings = FALSE)
dir.create(WORLD_RUGBY_PAGES_DIR, recursive = TRUE, showWarnings = FALSE)

#: libcurl's CURL_HTTP_VERSION_1_1. FRED's HTTP/2 endpoint drops streams mid-download.
#: Requests go out with httr2's default user agent: FRED stalls, without an error,
#: requests that carry a custom one.
HTTP_1_1 <- 2L

# ---- FRED ---------------------------------------------------------------------

fetch_fred <- function(series_id, start = as.Date("1980-01-01")) {
  req <- request(FRED_CSV_URL) |>
    req_url_query(id = series_id, cosd = format(start), coed = format(DATA_END)) |>
    req_options(http_version = HTTP_1_1) |>
    req_retry(max_tries = 4, retry_on_failure = TRUE)
  path <- file.path(RAW_DIR, "fred", paste0(series_id, ".csv"))
  resp <- req_perform(req, path = path)
  first <- readLines(path, n = 1)
  if (!grepl(series_id, first, fixed = TRUE)) {
    stop(series_id, ": unexpected header '", first, "'")
  }
  manifest_upsert(path, resp$url)
  message(series_id, ": ", length(readLines(path)) - 1, " rows")
}

for (id in c(FX_ZAR, FX_BENCHMARK, GDP_SERIES, GDP_NOMINAL_SERIES)) fetch_fred(id)

# ---- World Rugby ----------------------------------------------------------------

fetch_world_rugby_year <- function(year) {
  start <- as.Date(sprintf("%d-01-01", year))
  end <- min(as.Date(sprintf("%d-12-31", year)), DATA_END)
  page <- 0L
  repeat {
    Sys.sleep(WORLD_RUGBY_PAUSE_S)
    req <- request(WORLD_RUGBY_MATCH_URL) |>
      req_url_query(
        startDate = format(start), endDate = format(end), sort = "asc",
        pageSize = 100, page = page, sport = "mru", teams = WORLD_RUGBY_SA_ID
      ) |>
      req_headers(Accept = "application/json") |>
      req_options(http_version = HTTP_1_1) |>
      req_retry(max_tries = 4, retry_on_failure = TRUE)
    path <- file.path(WORLD_RUGBY_PAGES_DIR, sprintf("springboks_%d_p%d.json", year, page))
    req_perform(req, path = path)
    payload <- fromJSON(path, simplifyVector = FALSE)
    if (is.null(payload$pageInfo)) stop(year, ": response has no pageInfo")
    n_pages <- payload$pageInfo$numPages
    message(year, " page ", page, ": ", length(payload$content), " entries")
    if (page >= n_pages - 1L) break
    page <- page + 1L
  }
}

for (y in RUGBY_START_YEAR:as.integer(format(DATA_END, "%Y"))) fetch_world_rugby_year(y)

# World Rugby's terms do not allow its pages to be republished, so the pages
# stay local (see .gitignore) and the manifest records the facts table the
# paper renders from.
write_csv(extract_world_rugby(), WORLD_RUGBY_FACTS, na = "")
manifest_drop("^data/raw/world_rugby/.*\\.json$")
manifest_upsert(WORLD_RUGBY_FACTS, WORLD_RUGBY_FACTS_SOURCE)
message(WORLD_RUGBY_FACTS, ": ", nrow(read_world_rugby()), " entries")
