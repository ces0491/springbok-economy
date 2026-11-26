# Inputs for the whole pipeline. Every date, series and threshold the analysis
# depends on is set here, with its reason, so nothing is buried in a chunk.

# ---- Publication --------------------------------------------------------------

#: The public repository the paper points readers to for reproducing it.
REPO_URL <- "https://github.com/ces0491/springbok-economy"

# ---- Sample -------------------------------------------------------------------

#: Last day of data requested from any source. Fixed so a re-fetch asks for the
#: same window; the committed snapshots in data/raw are what the paper renders.
DATA_END <- as.Date("2026-11-24")

#: First day of the exchange-rate sample: the unification of the commercial and
#: financial rand. Before it, South Africa ran a dual exchange rate under capital
#: controls and the published rate is not comparable with what followed.
FX_START <- as.Date("1995-03-13")

#: First year requested from World Rugby. Readmission Tests began in 1992; the
#: years before FX_START are kept for the data-integrity cross-check only.
RUGBY_START_YEAR <- 1992L

# ---- Exchange rates (FRED, H.10 noon buying rates in New York) ----------------

#: Rand per US dollar.
FX_ZAR <- "DEXSFUS"

#: Emerging-market benchmark: currencies quoted per US dollar whose countries
#: play no tier-one rugby, so a Springbok result cannot move them through their
#: own team's fortunes. Australia, New Zealand, Japan, the UK and the euro area
#: are left out for that reason.
FX_BENCHMARK <- c(
  BRL = "DEXBZUS",
  MXN = "DEXMXUS",
  INR = "DEXINUS",
  KRW = "DEXKOUS",
  THB = "DEXTHUS"
)

# ---- Output (IMF International Financial Statistics, via FRED) ---------------

#: Real GDP, South Africa, domestic currency, seasonally adjusted, quarterly.
GDP_SERIES <- "NGDPRSAXDCZAQ"

#: Nominal GDP, same source and adjustment, to put a rand cost estimate in scale.
GDP_NOMINAL_SERIES <- "NGDPSAXDCZAQ"

FRED_CSV_URL <- "https://fred.stlouisfed.org/graph/fredgraph.csv"

# ---- Rugby results (World Rugby match list) -----------------------------------

WORLD_RUGBY_MATCH_URL <- "https://api.wr-rims-prod.pulselive.com/rugby/v3/match"

#: World Rugby's team id for the Springboks, read off its own match entries.
WORLD_RUGBY_SA_ID <- "39"

#: Seconds between requests to World Rugby, to stay polite.
WORLD_RUGBY_PAUSE_S <- 1.2

#: Where fetch_data.R saves World Rugby's match-list pages. World Rugby's terms
#: do not allow them to be republished, so .gitignore keeps them out of the
#: repository.
WORLD_RUGBY_PAGES_DIR <- "data/raw/world_rugby"

#: The match facts extracted from those pages, which the paper renders from and
#: the repository holds in their place, and how the manifest describes them.
WORLD_RUGBY_FACTS <- "data/raw/world_rugby/springbok_matches.csv"
WORLD_RUGBY_FACTS_SOURCE <- paste(
  "Extracted by R/fetch_data.R from World Rugby's match list,", WORLD_RUGBY_MATCH_URL,
  sprintf("(teams=%s, sport=mru, %d to %s)", WORLD_RUGBY_SA_ID, RUGBY_START_YEAR, format(DATA_END))
)

#: Opponents that make a match a Test: senior national fifteens of World Rugby
#: member unions, plus the British & Irish Lions. World Rugby's list also holds
#: tour matches against provinces, clubs, A sides, the Barbarians, World XVs and
#: the 2004 Pacific Islanders composite; none of those are counted.
TEST_OPPONENTS <- c(
  "Argentina", "Australia", "British & Irish Lions", "Canada", "England",
  "Fiji", "France", "Georgia", "Ireland", "Italy", "Japan", "Namibia",
  "New Zealand", "Portugal", "Romania", "Samoa", "Scotland", "Spain", "Tonga",
  "Uruguay", "USA", "Wales"
)

# ---- Inference ----------------------------------------------------------------

#: Seed for every resampling procedure in the paper.
SEED <- 19950624L # the date of the 1995 Rugby World Cup final

#: Draws for randomisation inference.
N_PERMUTATIONS <- 10000L

#: Significance level and power used for minimum detectable effects.
ALPHA <- 0.05
POWER <- 0.80

#: First year of the second half of the rand sample in the sub-period check.
SUBPERIOD_SPLIT_YEAR <- 2010L

#: Trailing window, in trading days, for the volatility used to standardise
#: weekend returns in the robustness check.
VOL_WINDOW_DAYS <- 60L

#: Share of each tail pulled in to the quantile when returns are winsorised,
#: to check that a result is not the work of a few extreme days.
WINSOR_TAIL <- 0.01

#: Fix intervals around each weekend in the event-time profile. 0 is the
#: weekend return itself, -1 the interval before it (normally Thursday to
#: Friday) and 1 the interval after it (normally Monday to Tuesday). Two either
#: side is as far as the windows of neighbouring weekends stay apart.
PROFILE_OFFSETS <- -2:2

#: Opponents with fewer weekend Tests than this share one effect in the
#: opponent-adjusted check. A dummy for a single Test has leverage one, where
#: HC3 standard errors are undefined.
OPPONENT_MIN_TESTS <- 10L

# ---- Spending (Statistics South Africa, downloaded by hand) ------------------

#: Stats SA's time-series workbook for the monthly Food and Beverages survey
#: (P6420). statssa.gov.za sits behind a CAPTCHA, so this file was downloaded
#: in a browser and is checked against its manifest hash like the rest.
STATSSA_FOOD_BEV <- "data/raw/statssa/P6420_202608/EXCEL/Food and beverages from 2005.xlsx"

#: The monthly retail trade sales workbook (P6242.1), obtained the same way.
STATSSA_RETAIL <- "data/raw/statssa/P62421_202609/EXCEL/Retail trade sales from 2002.xlsx"

#: Series codes across the two workbooks, all at constant prices and not
#: seasonally adjusted. Seasonality is handled in the model, alongside
#: calendar controls that Stats SA's adjusted series would hide.
SPENDING_SERIES <- c(
  bar = "B64200_CON",         # food and beverages: income from bar sales
  food = "A64200_CON",        # food and beverages: income from food sales
  total = "D64200_CON",       # food and beverages: total income
  specialised = "con_S6220",  # retail: food, beverages and tobacco in specialised stores
  general = "con_S621C"       # retail: general dealers
)

#: The three types of business whose bar income makes up the industry total in
#: SPENDING_SERIES["bar"]: restaurants and coffee shops, takeaway and fast-food
#: outlets, and catering services. The survey has no pub or tavern type, so the
#: paper calls the series bar sales at these outlets, and asserts the sum.
FOOD_BEV_BAR_PARTS <- c(
  restaurants = "B64201_CON",
  takeaways = "B64203_CON",
  caterers = "B64209_CON"
)

#: Months dropped from the spending regressions: COVID-19 trading restrictions,
#: curfews and bans on alcohol sales, during which bar income fell to zero.
COVID_MONTHS <- c(as.Date("2020-03-01"), as.Date("2021-12-01"))

#: Months of the 2010 football World Cup, hosted in South Africa. The positive
#: control: an event the bar-sales series ought to be able to see.
FIFA_2010_MONTHS <- as.Date(c("2010-06-01", "2010-07-01"))

#: Robustness: end the spending sample here.
SPENDING_ALT_END <- as.Date("2024-03-01")

#: Months of recent sales averaged to turn a percentage of a month's sales
#: into rand. A year, so the seasonal pattern washes out.
SPEND_LEVEL_MONTHS <- 12L

#: GDP quarters dropped in the robustness check: the 2020 lockdown collapse and
#: the rebound that followed it.
GDP_COVID_QUARTERS <- as.Date(c("2020-04-01", "2020-07-01"))

# ---- The 2023 public holiday ----------------------------------------------------

#: Declared after the 2023 final (Proclamation No. 143 of 2023).
HOLIDAY_2023 <- as.Date("2023-12-15")

#: Dawie Roodt's estimate of the cost of a public holiday, in R million per day
#: (The Citizen, 1 November 2023: "between R10 to R20 billion per day").
ROODT_COST_PER_DAY <- c(low = 10e3, high = 20e3)

# ---- News in the weeks after two finals ----------------------------------------

#: The day the Industrial and Commercial Bank of China announced it would buy
#: a fifth of Standard Bank (allAfrica, 25 October 2007). Standard Bank had
#: issued a cautionary announcement on the Tuesday of that week.
ICBC_DEAL_ANNOUNCED <- as.Date("2007-10-25")
ICBC_CAUTIONARY <- as.Date("2007-10-23")

#: The Friday on which Moody's affirmed South Africa's rating and cut its
#: outlook (National Treasury statement, 1 November 2019).
MOODYS_DECISION <- as.Date("2019-11-01")

#: The Wednesday of the 2019 medium-term budget, after which the rand fell
#: (Reuters, 4 November 2019: "On Wednesday, the rand saw its largest daily
#: fall in over a year").
MTBPS_2019 <- as.Date("2019-10-30")

#: The day Inside Education published the North-West University remarks quoted
#: in the introduction.
NWU_REMARKS_PUBLISHED <- as.Date("2023-09-18")

# ---- Rugby World Cup titles ---------------------------------------------------

#: Dates of the four finals South Africa won. Checked against World Rugby's
#: list in prepare.R, which fails the build if a date or result disagrees.
RWC_FINALS_WON <- as.Date(c("1995-06-24", "2007-10-20", "2019-11-02", "2023-10-28"))
