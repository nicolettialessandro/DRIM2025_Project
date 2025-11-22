## Multi-country macro download (monthly where possible)

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

## 1) UNEMPLOYMENT (monthly) - Modificato per una singola serie
unemp_df <- eurostat::get_eurostat("une_rt_m") %>%
  normalize_time() %>%
  dplyr::filter(
    geo %in% geo_vec,
    sex == "T",       # Totale Sesso
    age %in% c("Y15-74", "TOTAL"), # La categoria più comune, puoi scegliere solo una
    s_adj == "SA"      # Stagionalmente Aggiustato (l'indicatore più usato)
  ) %>%
  add_iso3() %>%
  dplyr::transmute(
    iso,
    date,
    unemployment_rate = values
  ) %>%
  clip_period("date") %>%
  dplyr::arrange(iso, date)
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

## 6) GPR - Dati GPRC per Paese (Sostituisce il Placeholder Globale)
# --- MODIFICATO: Legge il CSV, usa GPR_GLOBAL come fallback ---
library(readr) # Necessario per read_csv

gpr_path <- "GPR_dataclean.xlsx - Sheet1.csv" # Usiamo il CSV caricato. Modifica questo se necessario.

if (file.exists(gpr_path) && requireNamespace("readr", quietly = TRUE)) {
  
  # 1. Carica i dati GPR
  gpr_raw_df <- readr::read_csv(gpr_path, show_col_types = FALSE) %>%
    dplyr::rename(date = 1) %>% # Rinomina la prima colonna in 'date'
    dplyr::mutate(date = as.Date(date))
  
  # 2. Estrai la serie GPR_GLOBAL
  gpr_global_df <- gpr_raw_df %>%
    dplyr::select(date, gpr_global = GPR_GLOBAL) %>%
    clip_period("date") # Applica il filtro del periodo al globale
  
  # 3. Trasforma i dati GPRC da largo a lungo
  gprc_df <- gpr_raw_df %>%
    # Seleziona la colonna data e tutte le colonne GPRC (non GPR_GLOBAL)
    dplyr::select(date, starts_with("GPRC_")) %>%
    
    # Trasforma da formato largo (country in colonne) a formato lungo (country in righe)
    tidyr::pivot_longer(
      cols = starts_with("GPRC_"),
      names_to = "country_gpr",
      values_to = "gpr_c" # gpr_c per indicare il GPR "Country"
    ) %>%
    
    # Estrai il codice paese ISO3 (es. da GPRC_BEL a BEL)
    dplyr::mutate(
      iso = stringr::str_remove(country_gpr, "GPRC_")
    ) %>%
    
    # Filtra per mantenere solo i paesi del tuo vettore iso_vec
    dplyr::filter(iso %in% iso_vec) %>%
    
    # Applica il filtro del periodo
    clip_period("date") %>%
    
    # Rimuovi le colonne intermedie
    dplyr::select(iso, date, gpr_c)
  
  # 4. Crea una griglia completa di date e paesi
  all_dates <- unique(gpr_global_df$date)
  full_grid <- expand.grid(date = all_dates, iso = iso_vec, stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()
  
  # 5. Unisci i dati GPRC e il GPR_GLOBAL alla griglia completa
  gpr_df <- full_grid %>%
    # Unisci i dati GPRC disponibili
    dplyr::left_join(gprc_df, by = c("iso", "date")) %>%
    
    # Unisci i dati GPR_GLOBAL
    dplyr::left_join(gpr_global_df, by = "date") %>%
    
    # 6. Implementa la logica di fallback
    # Se gpr_c è NA (paese senza dati specifici), usa gpr_global
    dplyr::mutate(
      gpr = dplyr::if_else(is.na(gpr_c), gpr_global, gpr_c)
    ) %>%
    
    # 7. Pulisci e ordina
    dplyr::select(iso, date, gpr) %>%
    dplyr::arrange(iso, date)
  
  message("GPR (GPRC con fallback GPR_GLOBAL) data loaded for individual countries: ", nrow(gpr_df), " observations")
  
} else {
  message("GPR CSV file not found or 'readr' not installed, leaving gpr_df empty.")
  gpr_df <- tibble::tibble(
    iso = character(),
    date = as.Date(character()),
    gpr = numeric()
  )
}

# === SAVE CLEANED MACRO DATA TO PARQUET ===
library(arrow)

dir.create("01_data_clean/macro", recursive = TRUE, showWarnings = FALSE)

write_parquet(unemp_df,      "01_data_clean/macro/unemployment.parquet")
write_parquet(cpi_df,        "01_data_clean/macro/inflation.parquet")
write_parquet(yields_10y_df, "01_data_clean/macro/yields_10y.parquet")
write_parquet(credit_spread_df, "01_data_clean/macro/credit_spreads.parquet")
write_parquet(gdp_m_df,      "01_data_clean/macro/gdp.parquet")

if (exists("gpr_df") && nrow(gpr_df) > 0) {
  write_parquet(gpr_df, "01_data_clean/macro/gpr.parquet")
}

message("✅ All macro datasets saved successfully in 01_data_clean/macro/")