# Record hand-downloaded snapshots in data/raw/MANIFEST.csv.
#
# statssa.gov.za puts its files behind a CAPTCHA, so scripts cannot fetch
# them. Download the zip from the Time Series Data page in a browser
# (https://www.statssa.gov.za/?page_id=1847), unzip it into its own folder
# under data/raw/statssa/, then run from the project root:
#
#   Rscript R/register_manual.R
#
# The render then refuses to run if either file changes.

source("R/config.R")
source("R/manifest.R")

manual <- list(
  list(
    path = "data/raw/statssa/P6420_202608/P6420 Food and beverages (202608).zip",
    source = paste("https://www.statssa.gov.za/timeseriesdata/Excel/P6420%20Food%20and%20beverages%20(202608).zip",
                   "(downloaded in a browser; statssa.gov.za is behind a CAPTCHA)")
  ),
  list(
    path = STATSSA_FOOD_BEV,
    source = "Extracted from data/raw/statssa/P6420_202608/P6420 Food and beverages (202608).zip"
  ),
  list(
    path = file.path(
      "data/raw/statssa/P62421_202609",
      "P6242.1 Retail trade sales (New time series) from January 2002_202609.zip"
    ),
    source = paste0(
      "https://www.statssa.gov.za/timeseriesdata/Excel/",
      "P6242.1%20Retail%20trade%20sales%20(New%20time%20series)%20from%20January%202002_202609.zip",
      " (downloaded in a browser; statssa.gov.za is behind a CAPTCHA)"
    )
  ),
  list(
    path = STATSSA_RETAIL,
    source = paste0(
      "Extracted from data/raw/statssa/P62421_202609/",
      "P6242.1 Retail trade sales (New time series) from January 2002_202609.zip"
    )
  )
)

for (m in manual) {
  if (!file.exists(m$path)) stop("Missing ", m$path)
  manifest_upsert(m$path, m$source)
  message("Registered ", m$path)
}
