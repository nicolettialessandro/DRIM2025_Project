# ==========================================================
# 06_macro_merge.R  —  Build panel (PD + Macro)
# ==========================================================
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(lubridate); library(readr)
})

dir.create("02_team_modules/D_Dependencies_Macro", recursive = TRUE, showWarnings = FALSE)

# ---------- 1) Carica macro ----------
unemp_df         <- tryCatch(read_parquet("01_data_clean/macro/unemployment.parquet"),      error = function(e) tibble())
cpi_df           <- tryCatch(read_parquet("01_data_clean/macro/inflation.parquet"),         error = function(e) tibble())
yields_10y_df    <- tryCatch(read_parquet("01_data_clean/macro/yields_10y.parquet"),        error = function(e) tibble())
credit_spread_df <- tryCatch(read_parquet("01_data_clean/macro/credit_spreads.parquet"),    error = function(e) tibble())
gdp_m_df         <- tryCatch(read_parquet("01_data_clean/macro/gdp.parquet"),               error = function(e) tibble())
gpr_df           <- tryCatch(read_parquet("01_data_clean/macro/gpr.parquet"),               error = function(e) tibble())

# ---------- 2) Macro unite ----------
macro_df <- unemp_df %>%
  full_join(cpi_df,           by = c("iso", "date")) %>%
  full_join(yields_10y_df,    by = c("iso", "date")) %>%
  full_join(credit_spread_df, by = c("iso", "date")) %>%
  full_join(gdp_m_df,         by = c("iso", "date"))

# --- Fix nomi duplicati nei rendimenti ---
if ("yield_10y.x" %in% names(macro_df)) {
  macro_df <- macro_df %>%
    rename(yield_10y = yield_10y.x) %>%
    select(-yield_10y.y)
}

# --- Deduplica macro per (iso, date) ---
macro_df <- macro_df %>%
  mutate(
    iso  = toupper(iso),
    date = as.Date(date)
  ) %>%
  group_by(iso, date) %>%
  summarise(
    across(.fns = ~ suppressWarnings(if (is.numeric(.x)) mean(.x, na.rm = TRUE) else dplyr::first(.x))),
    .groups = "drop"
  )

# --- Aggiungi GPR country-specific (da formato wide a long) ---
if (nrow(gpr_df) > 0) {
  
  message("📋 Colonne originali in gpr_df: ", paste(names(gpr_df)[1:15], collapse = ", "), " ...")
  
  # Porta tutto in minuscolo per evitare errori di case sensitivity
  names(gpr_df) <- tolower(names(gpr_df))
  
  # Controllo presenza colonna data (date o time)
  if ("time" %in% names(gpr_df) && !"date" %in% names(gpr_df)) {
    gpr_df <- gpr_df %>% rename(date = time)
  }
  if (!"date" %in% names(gpr_df)) {
    stop("❌ Nessuna colonna 'date' trovata nel file GPR (nemmeno 'time').")
  }
  
  # Seleziona solo le colonne di interesse: date + GPRC_*
  gpr_long <- gpr_df %>%
    select(date, starts_with("gprc_")) %>%
    pivot_longer(
      cols = starts_with("gprc_"),
      names_to = "iso",
      values_to = "gpr"
    ) %>%
    mutate(
      iso = str_remove(iso, "^gprc_"),
      iso = toupper(iso),
      date = as.Date(date)
    )
  
  message("✅ GPR trasformato in formato long: ", nrow(gpr_long), " righe")
  
  # Unisci al macro_df
  macro_df <- macro_df %>%
    left_join(gpr_long, by = c("iso", "date"))
  
  message("✅ Join GPR completato: ", sum(!is.na(macro_df$gpr)), " osservazioni con valori GPR")
}

# ---------- 3) PD medie mensili per country–sector ----------
pd_base <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

pd_df <- pd_base %>%
  transmute(
    iso = toupper(country),
    gdesc = as.character(gdesc),
    # Rinomina i cluster per leggibilità
    Cluster = recode(as.character(Cluster),
                     "1" = "Low risk",
                     "2" = "High risk"),
    date = floor_date(as.Date(data_date), "month"),
    q_1y = as.numeric(q_1y)
  ) %>%
  
  group_by(iso, gdesc, Cluster, date) %>%
  summarise(PD_mean = mean(q_1y, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(iso), !is.na(gdesc), !is.na(date)) %>%
  distinct(iso, gdesc, Cluster, date, .keep_all = TRUE)

# ---------- 4) Join PD + Macro ----------
panel_df <- pd_df %>%
  left_join(macro_df, by = c("iso", "date"))

# Controllino
stopifnot(inherits(panel_df$date, "Date"))

# ---------- 5) Controlli ----------
message("Rows in panel_df: ", nrow(panel_df))
na_rate <- colSums(is.na(panel_df)) / nrow(panel_df)
write_csv(
  tibble(variable = names(na_rate), na_share = as.numeric(na_rate)),
  "02_team_modules/D_Dependencies_Macro/na_report_panel.csv"
)

# ---------- 6) Salva ----------
write_parquet(panel_df, "02_team_modules/D_Dependencies_Macro/panel_df.parquet")
message("✅ Saved: 02_team_modules/D_Dependencies_Macro/panel_df.parquet")

write_parquet(panel_df, "02_team_modules/D_Dependencies_Macro/panel_df_risk_named.parquet")
message("✅ Saved: panel_df_risk_named.parquet")

# ---------- 7) Riepilogo per cluster ----------
summ <- panel_df %>%
  group_by(Cluster) %>%
  summarise(
    n = n(),
    PD_mean = mean(PD_mean, na.rm = TRUE),
    unemp = mean(unemployment_rate, na.rm = TRUE),
    hicp = mean(hicp_yoy, na.rm = TRUE),
    yield = mean(yield_10y, na.rm = TRUE),
    spread = mean(credit_spread_bp, na.rm = TRUE),
    gdp = mean(gdp, na.rm = TRUE),
    gpr = mean(gpr, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summ, "02_team_modules/D_Dependencies_Macro/summary_panel_by_cluster.csv")
message("✅ Saved: summary_panel_by_cluster.csv")