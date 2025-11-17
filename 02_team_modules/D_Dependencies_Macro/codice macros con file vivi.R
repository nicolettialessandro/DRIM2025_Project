# ============================================================
# PACKAGES (lean)
# ============================================================
pkgs <- c("dplyr", "tidyr", "lubridate", "purrr", "ggplot2",
          "eurostat", "quantmod", "zoo", "scales", "readr", "TTR", "rlang",
          "chromote", "rvest", "xml2", "stringr", "tibble")
new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

options(eurostat_cache = TRUE)

try_silent <- function(expr) try(expr, silent = TRUE)

# ============================================================
# COUNTRIES (EU + USA)
# ============================================================
country_list <- c("AUT", "BEL", "CYP", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR",
                  "GRC", "HRV", "IRL", "ITA", "LUX", "MLT", "NLD", "NOR", "POL", "PRT",
                  "SVK", "SVN", "SWE", "CHE", "USA")

# ============================================================
# READ PD CSV (fixed) + DATE NORMALIZATION
# ============================================================
# Use file.path or a raw string for Windows:
# data_path <- r"(C:\Users\Gaia\Documents\magistrale\AMUniversite\credit risk\DRIM2025_Project0\02_team_modules\B_Descriptive_Analysis\pd_country_monthly.csv)"
data_path <- file.path("C:/Users/Gaia/Documents/magistrale/AMUniversite/credit risk/DRIM2025_Project0",
                       "02_team_modules", "B_Descriptive_Analysis", "pd_country_monthly.csv")

stopifnot(file.exists(data_path))
pd_raw <- readr::read_csv(data_path, show_col_types = FALSE)

# detect date column + normalize to month
date_col <- dplyr::case_when(
  "date"       %in% names(pd_raw) ~ "date",
  "data_date"  %in% names(pd_raw) ~ "data_date",
  "month_year" %in% names(pd_raw) ~ "month_year",
  TRUE ~ NA_character_
)
if (is.na(date_col)) stop("No date column found. Expect `date`, `data_date`, or `month_year`.")

pd <- pd_raw %>%
  mutate(date_raw = .data[[date_col]]) %>%
  mutate(
    date = dplyr::case_when(
      inherits(date_raw,"Date")                 ~ as.Date(date_raw),
      is.character(date_raw) & nchar(date_raw)==7 ~ lubridate::ym(date_raw),
      TRUE                                        ~ suppressWarnings(as.Date(date_raw))
    )
  ) %>%
  mutate(date = lubridate::floor_date(date, "month")) %>%
  filter(country %in% country_list)

# pick a PD horizon column (prefer 1Y if present)
pd_names_l <- tolower(names(pd))
pref <- c("q_1y", "kdp_1yr", "q_6m", "kdp_6mo", "q_3m", "kdp_3mo", "q_1m", "kdp_1mo", "q_3y", "kdp_3yr", "q_5y", "kdp_5yr")
cand <- intersect(pref, pd_names_l)
if (length(cand) == 0) stop("No PD tenor columns found (q_* or KDP_*).")
PD_COL <- names(pd)[match(cand[1], pd_names_l)]
message("Using PD column: ", PD_COL)

# aggregate to country–month (median across firms)
pd_country_agg <- pd %>%
  filter(!is.na(country), !is.na(date)) %>%
  group_by(country, date) %>%
  summarize(PD = median(.data[[PD_COL]], na.rm = TRUE),
            n_companies = dplyr::n(), .groups = "drop")

keep_countries <- intersect(country_list, unique(pd_country_agg$country))

# ============================================================
# ISO3 -> Eurostat GEO mapping
# ============================================================
iso3_to_eu2 <- tibble::tribble(
  ~iso3, ~eurostat_geo,
  "AUT", "AT", "BEL", "BE", "CYP", "CY", "DEU", "DE", "DNK", "DK", "ESP", "ES", "EST", "EE", "FIN", "FI",
  "FRA", "FR", "GRC", "EL", "HRV", "HR", "IRL", "IE", "ITA", "IT", "LUX", "LU", "MLT", "MT", "NLD", "NL",
  "POL", "PL", "PRT", "PT", "SVK", "SK", "SVN", "SI", "SWE", "SE", "GBR", "UK", "NOR", "NO", "CHE", "CH"
)
iso_map <- iso3_to_eu2 %>% filter(iso3 %in% keep_countries)
eu_geo  <- iso_map$eurostat_geo

# ============================================================
# EUROSTAT (simple selects): Unemployment, CPI YoY, 10Y
# ============================================================
get_eu_unemp <- function(geo_vec){
  df <- eurostat::get_eurostat("une_rt_m", time_format="date", cache=TRUE)
  df %>%
    filter(geo %in% geo_vec, sex == "T", age == "Y15-74", s_adj == "SA") %>%
    transmute(eurostat_geo = geo, date = time, Unemployment = values)
}
get_eu_cpi_yoy <- function(geo_vec){
  df <- eurostat::get_eurostat("prc_hicp_manr", time_format="date", cache=TRUE)
  df %>%
    filter(geo %in% geo_vec, coicop == "CP00") %>%
    transmute(eurostat_geo = geo, date = time, CPI_YoY = values)
}
get_eu_10y <- function(geo_vec){
  df <- eurostat::get_eurostat("irt_lt_mcby_m", time_format="date", cache=TRUE)
  df %>%
    filter(geo %in% geo_vec, unit == "PC_PA") %>%
    transmute(eurostat_geo = geo, date = time, Y10_Gov = values)
}

eu_unemp <- try_silent(get_eu_unemp(eu_geo)); if(inherits(eu_unemp, "try-error")) eu_unemp <- tibble()
eu_cpi   <- try_silent(get_eu_cpi_yoy(eu_geo)); if(inherits(eu_cpi, "try-error")) eu_cpi <- tibble()
eu_y10   <- try_silent(get_eu_10y(eu_geo)); if(inherits(eu_y10, "try-error")) eu_y10 <- tibble()

eu_macro <- list(eu_unemp, eu_cpi, eu_y10) %>%
  purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0) %>%
  purrr::reduce(dplyr::full_join, by = c("eurostat_geo", "date")) %>%
  dplyr::left_join(iso_map, by = "eurostat_geo") %>%
  dplyr::rename(country = iso3) %>%
  dplyr::select(country, date,
                Unemployment = dplyr::any_of("Unemployment"),
                CPI_YoY      = dplyr::any_of("CPI_YoY"),
                Y10_Gov      = dplyr::any_of("Y10_Gov"))

# ============================================================
# USA MACROS (FRED): Unemployment, CPI YoY, 10Y, GDP

