# Export the Springbok Tests held in a second, independently compiled results
# database, so the paper can check World Rugby's list against it.
#
# The database is rugby-tournament-predictor's rugby.db (ESPN scoreboard plus a
# historical import, audited against both). It is not public, so its export is
# committed as a snapshot and this script only needs to run where it exists:
#
#   Rscript R/export_crosscheck.R path/to/rugby.db

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

source("R/config.R")
source("R/manifest.R")

args <- commandArgs(trailingOnly = TRUE)
db_path <- if (length(args) >= 1) args[[1]] else "../rugby-tournament-predictor/data/rugby.db"
if (!file.exists(db_path)) stop("No database at ", db_path)

con <- dbConnect(SQLite(), db_path, flags = SQLITE_RO)
on.exit(dbDisconnect(con))

tests <- dbGetQuery(con, "
  SELECT substr(m.match_date, 1, 10) AS match_date,
         h.name AS home_team, a.name AS away_team,
         m.home_score, m.away_score, m.is_world_cup, m.data_source
  FROM matches m
  JOIN teams h ON h.id = m.home_team_id
  JOIN teams a ON a.id = m.away_team_id
  WHERE (h.name = 'South Africa' OR a.name = 'South Africa')
    AND m.match_date >= ? AND m.match_date <= ?
  ORDER BY m.match_date",
  params = list(sprintf("%d-01-01", RUGBY_START_YEAR), format(DATA_END))
)

dir.create("data/raw/crosscheck", recursive = TRUE, showWarnings = FALSE)
path <- "data/raw/crosscheck/rugby_db_springboks.csv"
write.csv(tests, path, row.names = FALSE)
manifest_upsert(path, "rugby-tournament-predictor data/rugby.db (local; not public)")
message("Wrote ", nrow(tests), " Tests to ", path)
