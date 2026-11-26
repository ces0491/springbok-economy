# The Bok Stops Here

A light-hearted economics paper with a straight-faced method: does anything in the South African economy move when the Springboks play? One regression is run on three timescales, with the number of Tests in the period as the variable of interest: the rand over the weekend of a Test, real sales over the month at restaurants, takeaways and caterers (food and bar takings) and at butchers and bottle stores, and real GDP over the quarter. A second version asks whether winning matters more than playing.

On the data snapshot of 25 November 2026 the answer is no on each timescale. A Test weekend's return on the rand is within about 12 basis points of other weekends', and the rand moves no further in either direction. One more Test in a month adds nothing measurable to real bar sales at restaurants, takeaways and caterers or to sales at specialised food and drink stores, and a quarter with more Tests grows no faster. Winning rather than losing changes none of it. Restaurant and takeaway sales do pick up the 2010 football World Cup. The one estimate the paper cannot explain is a weaker rand on the Friday before a Test weekend. The paper itself has the details, the caveats, and one regression that should be ignored.

- `springbok-economy.qmd`: the paper (Quarto, R). Renders to HTML and to PDF via Typst.
- `R/`: the pipeline, one file per stage.
- `data/raw/`: snapshots of every input, with `MANIFEST.csv` recording each file's source URL, retrieval time and SHA-256. World Rugby's snapshot is a table of the match facts the paper uses; its pages stay out of the repository (see [Licence and data terms](#licence-and-data-terms)).
- `data/corrections/`: documented fixes to source data, each with a citation.
- `tests/testthat/`: unit tests for the data preparation and inference code.

## Reproducing the paper

Requires R 4.5 and Quarto 1.10 or later. Typst ships with Quarto, so no LaTeX installation is needed for the PDF.

```sh
Rscript -e "renv::restore()"                       # install the locked package versions
quarto render springbok-economy.qmd                # HTML and PDF, from data/raw only
Rscript -e "testthat::test_dir('tests/testthat')"  # unit tests
Rscript -e "lintr::lint_dir('R')"                 # style checks, configured in .lintr.R
```

Rendering never touches the network. It checks each snapshot against `data/raw/MANIFEST.csv` and stops if any file has changed. Randomisation uses a fixed seed set in `R/config.R`, so a render reproduces every number in the paper.

## Refreshing the data

```sh
Rscript R/fetch_data.R                                  # FRED, and World Rugby's match facts
Rscript R/export_crosscheck.R path/to/rugby.db          # second results source, if available
Rscript R/register_manual.R                             # after replacing the Stats SA files by hand
```

Statistics South Africa's site sits behind a CAPTCHA, so its two monthly time series can't be fetched by script. Download the Excel zips for P6420 (Food and beverages) and P6242.1 (Retail trade sales, new time series) from the [Time Series Data page](https://www.statssa.gov.za/?page_id=1847), unzip each into its own folder under `data/raw/statssa/`, update the paths in `R/config.R` and `R/register_manual.R` if the release month has changed, and run `register_manual.R` to record them in the manifest.

`fetch_data.R` requests data up to `DATA_END` in `R/config.R`. It saves World Rugby's match-list pages under `data/raw/world_rugby/`, where `.gitignore` keeps them out of git, and writes the facts the paper uses to `data/raw/world_rugby/springbok_matches.csv`. FRED revises GDP and World Rugby edits old entries, so a re-fetch can change results; diff `MANIFEST.csv` to see which files moved. The setup chunk of the paper asserts the claims its prose makes in words (for example, that the quarters around three of the finals grew), and stops the render if new data makes one false.

## Sources

| Data | Source |
| --- | --- |
| Springbok results | World Rugby match list (`api.wr-rims-prod.pulselive.com/rugby/v3/match`, team 39); the repository holds the match facts extracted from it |
| Cross-check results, and the home side where World Rugby lists no venue | `rugby-tournament-predictor` database (ESPN and a historical import); not public, exported to `data/raw/crosscheck/` |
| Exchange rates | Federal Reserve H.10 noon buying rates via FRED: DEXSFUS, DEXBZUS, DEXMXUS, DEXINUS, DEXKOUS, DEXTHUS |
| GDP | IMF International Financial Statistics via FRED: NGDPRSAXDCZAQ (real), NGDPSAXDCZAQ (nominal) |
| Bar and food income | Stats SA Food and beverages survey (P6420), constant prices, monthly, downloaded by hand |
| Retail sales | Stats SA Retail trade sales (P6242.1): specialised food, beverages and tobacco stores; general dealers; constant prices, monthly, downloaded by hand |

The literature, press and official sources are in `references.bib`, each with a DOI or the URL it was read at.

## Licence and data terms

The code (`R/`, `tests/` and the R chunks of `springbok-economy.qmd`) is under the MIT licence in [`LICENSE`](LICENSE). The paper's text and figures, including the rendered HTML and PDF, are under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

Neither licence covers the data in `data/raw/`, which remain their sources'. Credit them as the paper does:

- **World Rugby.** Its [terms](https://www.world.rugby/terms-and-conditions) limit its content, data and statistics to personal, non-commercial use and do not allow it to be republished. The repository therefore holds only the match facts the paper uses (dates, teams, scores, venue country, competition, stage and kick-off time), not World Rugby's pages; `R/fetch_data.R` rebuilds the table from source.
- **Statistics South Africa.** Stats SA's standard notice (for example in [*Census 2001: Census in brief*](https://www.gov.za/sites/default/files/gcis_document/201409/census01brief0.pdf)) allows its data to be used and processed provided Stats SA is acknowledged as the original source and the analysis is stated to be the user's own independent processing, and does not allow the data or any reprocessed version to be sold without its permission. The analysis in this repository is my own independent processing of Stats SA's data.
- **IMF International Financial Statistics,** via FRED. The IMF's [terms](https://www.imf.org/en/about/copyright-and-terms) allow its statistical data to be reused, copied and redistributed with the IMF as the source ("Source: International Monetary Fund, International Financial Statistics"); commercial reuse needs its permission. FRED tags the two GDP series "Copyrighted: Citation Required": they may be used with the source credited and FRED acknowledged.
- **Federal Reserve Board H.10 exchange rates,** via FRED, which tags the six series "Public Domain: Citation Requested".

Under FRED's [terms](https://fred.stlouisfed.org/legal/) each series needs a citation (requested for the public-domain ones, required for the IMF's), in the form its series page suggests:

- Board of Governors of the Federal Reserve System (US), South African Rand to U.S. Dollar Spot Exchange Rate [DEXSFUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXSFUS>
- Board of Governors of the Federal Reserve System (US), Brazilian Reals to U.S. Dollar Spot Exchange Rate [DEXBZUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXBZUS>
- Board of Governors of the Federal Reserve System (US), Mexican Pesos to U.S. Dollar Spot Exchange Rate [DEXMXUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXMXUS>
- Board of Governors of the Federal Reserve System (US), Indian Rupees to U.S. Dollar Spot Exchange Rate [DEXINUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXINUS>
- Board of Governors of the Federal Reserve System (US), South Korean Won to U.S. Dollar Spot Exchange Rate [DEXKOUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXKOUS>
- Board of Governors of the Federal Reserve System (US), Thai Baht to U.S. Dollar Spot Exchange Rate [DEXTHUS], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/DEXTHUS>
- International Monetary Fund, Real Gross Domestic Product (GDP) for South Africa [NGDPRSAXDCZAQ], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/NGDPRSAXDCZAQ>
- International Monetary Fund, Nominal Gross Domestic Product (GDP) for South Africa [NGDPSAXDCZAQ], retrieved from FRED, Federal Reserve Bank of St. Louis; <https://fred.stlouisfed.org/series/NGDPSAXDCZAQ>
- **Cross-check results** (`data/raw/crosscheck/`): Springbok match results exported from my own results database, compiled from ESPN's scoreboard and a historical import.

`harvard-cite-them-right.csl` is from the Zotero Style Repository, under [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/).
