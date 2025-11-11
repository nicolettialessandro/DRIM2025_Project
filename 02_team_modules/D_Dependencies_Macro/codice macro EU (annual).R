## =========================================================
## Multi-country macro download (monthly where possible)
## Clean version with robust 10Y yield filter
## =========================================================

libs <- c(
  "dplyr", "tidyr", "purrr", "tibble",
  "lubridate", "stringr",
  "eurostat",
  "readr"
)
to_install <- setdiff(libs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(libs, library, character.only = TRUE))

iso_vec <- c(
  "AUT","BEL","CYP","DEU","DNK","ESP","EST","FIN","FRA","GBR",
  "GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL","PRT",
  "SVK","SVN","SWE","CHE"
)

iso3_to_geo <- c(
  AUT = "AT", BEL = "BE", CYP = "CY", DEU = "DE", DNK = "DK",
  ESP = "ES", EST = "EE", FIN = "FI", FRA = "FR", GBR = "UK",
  GRC = "EL", HRV = "HR", IRL = "IE", ITA = "IT", LUX = "LU",
  MLT = "MT", NLD = "NL", NOR = "NO", POL = "PL", PRT = "PT",
  SVK = "SK", SVN = "SI", SWE = "SE", CHE = "CH"
)
geo_vec <- unname(iso3_to_geo)

start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2024-12-31")

clip_period <- function(df, date_col = "date") {
  df %>%
    dplyr::filter(.data[[date_col]] >= start_date,
                  .data[[date_col]] <= end_date)
}

normalize_time <- function(df) {
  if ("date" %in% names(df)) {
    df
  } else if ("time" %in% names(df)) {
    df %>% dplyr::rename(date = time)
  } else if ("TIME_PERIOD" %in% names(df)) {
    df %>% dplyr::mutate(date = as.Date(TIME_PERIOD))
  } else if ("time_period" %in% names(df)) {
    df %>% dplyr::mutate(date = as.Date(time_period))
  } else {
    stop("No recognizable time column. Columns: ",
         paste(names(df), collapse = ", "))
  }
}

add_iso3 <- function(df) {
  df %>%
    dplyr::mutate(
      iso = names(iso3_to_geo)[match(geo, iso3_to_geo)]
    ) %>%
    dplyr::filter(!is.na(iso), iso %in% iso_vec)
}

## 1) UNEMPLOYMENT (monthly)

unemp_df <- eurostat::get_eurostat("une_rt_m") %>%
  normalize_time() %>%
  dplyr::filter(
    geo %in% geo_vec,
    sex %in% c("T", "M", "F"),
    age %in% c("Y15-74", "TOTAL", "Y_GE15"),
    s_adj %in% c("SA", "NSA", "SCA")
  ) %>%
  add_iso3() %>%
  dplyr::transmute(
    iso,
    date,
    unemployment_rate = values
  ) %>%
  clip_period("date") %>%
  dplyr::arrange(iso, date)

message("Unemployment data loaded: ", nrow(unemp_df), " observations")

## 2) CPI / HICP YoY (monthly)

cpi_df <- eurostat::get_eurostat("prc_hicp_manr") %>%
  normalize_time() %>%
  dplyr::filter(
    geo %in% geo_vec,
    coicop == "CP00",
    unit == "RCH_A"
  ) %>%
  add_iso3() %>%
  dplyr::transmute(
    iso,
    date,
    hicp_yoy = values
  ) %>%
  clip_period("date") %>%
  dplyr::arrange(iso, date)

## 3) 10Y GOV BOND YIELD (monthly, robust selection)

yield_raw <- eurostat::get_eurostat("ei_mfir_m") %>%
  normalize_time()

## Look at structure (uncomment in your console if curious)
## names(yield_raw); dplyr::count(yield_raw, unit); head(yield_raw)

## Keep only our countries
yield_raw <- yield_raw %>%
  dplyr::filter(geo %in% geo_vec)

## If there is an 'int_rt' column, select the 10Y gov bond code
if ("int_rt" %in% names(yield_raw)) {
  # Typical code for long-term government bond yields
  if ("LR_LONG_G_B_Y" %in% unique(yield_raw$int_rt)) {
    yield_raw <- yield_raw %>% dplyr::filter(int_rt == "LR_LONG_G_B_Y")
  }
}

## If there is a 'maturity' column indicating 10Y, use that (fallback)
if (!"int_rt" %in% names(yield_raw) && "maturity" %in% names(yield_raw)) {
  ten_codes <- unique(yield_raw$maturity[grepl("10", yield_raw$maturity)])
  if (length(ten_codes) == 1) {
    yield_raw <- yield_raw %>% dplyr::filter(maturity == ten_codes)
  }
}

## Now attach iso3 and keep the values
if (!all(c("geo","date","values") %in% names(yield_raw))) {
  warning("Unexpected structure in ei_mfir_m; yields_10y_df set empty. Check names(yield_raw).")
  yields_10y_df <- tibble::tibble(
    iso = character(),
    date = as.Date(character()),
    yield_10y = numeric()
  )
} else {
  yields_10y_df <- yield_raw %>%
    add_iso3() %>%
    dplyr::transmute(
      iso,
      date,
      yield_10y = values
    ) %>%
    clip_period("date") %>%
    dplyr::arrange(iso, date)
}

## 4) CREDIT SPREAD vs 10Y BUND

if (nrow(yields_10y_df) > 0 && any(yields_10y_df$iso == "DEU")) {
  
  bund_df <- yields_10y_df %>%
    dplyr::filter(iso == "DEU") %>%
    dplyr::select(date, bund_10y = yield_10y)
  
  credit_spread_df <- yields_10y_df %>%
    dplyr::filter(iso != "DEU") %>%
    dplyr::left_join(bund_df, by = "date") %>%
    dplyr::filter(!is.na(bund_10y)) %>%
    dplyr::mutate(
      credit_spread_bp = (yield_10y - bund_10y) * 100
    ) %>%
    dplyr::arrange(iso, date)
  
} else {
  
  warning("No valid DEU 10Y yield found; credit_spread_df left empty.")
  credit_spread_df <- tibble::tibble(
    iso = character(),
    date = as.Date(character()),
    yield_10y = numeric(),
    bund_10y = numeric(),
    credit_spread_bp = numeric()
  )
}

## 5) GDP (quarterly -> monthly proxy)

gdp_q_df <- eurostat::get_eurostat("namq_10_gdp") %>%
  normalize_time() %>%
  dplyr::filter(
    geo %in% geo_vec,
    na_item == "B1GQ",
    unit == "CLV10_MEUR"
  ) %>%
  add_iso3() %>%
  dplyr::transmute(
    iso,
    date,
    gdp = values
  ) %>%
  clip_period("date") %>%
  dplyr::arrange(iso, date)

if (nrow(gdp_q_df) > 0) {
  gdp_m_df <- gdp_q_df %>%
    dplyr::group_by(iso) %>%
    tidyr::complete(
      date = seq.Date(from = min(date), to = max(date), by = "month")
    ) %>%
    tidyr::fill(gdp, .direction = "down") %>%
    dplyr::ungroup() %>%
    clip_period("date") %>%
    dplyr::arrange(iso, date)
} else {
  gdp_m_df <- tibble::tibble(
    iso = character(),
    date = as.Date(character()),
    gdp = numeric()
  )
}

## 6) GPR placeholder

library(readxl)
library(dplyr)

gpr_path <- "01_data_clean/GPR_data.xls"  # percorso corretto al file

if (file.exists(gpr_path)) {
  gpr_df <- read_excel(gpr_path) %>%
    rename(date = month, gpr = GPR) %>%        # uniforma nomi
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%  # converte stringa in data
    filter(!is.na(gpr)) %>%
    mutate(iso = "GLOBAL")                     # se è un GPR aggregato globale
  
  message("GPR data loaded from Excel: ", nrow(gpr_df), " observations")
} else {
  message("GPR Excel file not found, leaving gpr_df empty.")
}

## Summary: key objects
## - unemp_df         (monthly)
## - cpi_df           (monthly)
## - yields_10y_df    (monthly, if structure matched)
## - credit_spread_df (monthly, if DEU yield available)
## - gdp_m_df         (monthly proxy from quarterly)
## - gpr_df           (placeholder)

# === SAVE CLEANED MACRO DATA TO PARQUET ===
library(arrow)

dir.create("01_data_clean/macro", recursive = TRUE, showWarnings = FALSE)

write_parquet(unemp_df,         "01_data_clean/macro/unemployment.parquet")
write_parquet(cpi_df,           "01_data_clean/macro/inflation.parquet")
write_parquet(yields_10y_df,    "01_data_clean/macro/yields_10y.parquet")
write_parquet(credit_spread_df, "01_data_clean/macro/credit_spreads.parquet")
write_parquet(gdp_m_df,         "01_data_clean/macro/gdp.parquet")

if (exists("gpr_df") && nrow(gpr_df) > 0) {
  write_parquet(gpr_df, "01_data_clean/macro/gpr.parquet")
}

message("✅ All macro datasets saved successfully in 01_data_clean/macro/")