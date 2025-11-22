# ==========================================================
# 06_macro_merge.R  —  Build panel (PD + Macro)
# ==========================================================
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(lubridate); library(readr); library(stringr)
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

# Aggiungi GPR country-specific (da formato wide a long) ---
if (nrow(gpr_df) > 0) {
  
  # La lettura del messaggio è cambiata, mostra le colonne già in formato long
  message("📋 Colonne originali in gpr_df: ", paste(names(gpr_df), collapse = ", "))
  
  # Porta i nomi delle colonne a minuscolo per evitare problemi di join
  names(gpr_df) <- tolower(names(gpr_df))
  
  # Verifica che le colonne chiave necessarie siano presenti dopo la lettura da Parquet
  required_cols <- c("iso", "date", "gpr")
  if (!all(required_cols %in% names(gpr_df))) {
    stop(paste0("❌ File GPR non nel formato corretto. Mancano le colonne: ", 
                paste(setdiff(required_cols, names(gpr_df)), collapse = ", ")))
  }
  
  # Prepara gpr_df per il join (assicurati che i tipi siano corretti)
  gpr_final <- gpr_df %>%
    mutate(
      iso = toupper(iso), # Armonizza le maiuscole
      date = as.Date(date) # Armonizza il tipo di data
    ) %>%
    # Se ci sono duplicati (non dovrebbero esserci), deduplica prendendo la media
    group_by(iso, date) %>%
    summarise(gpr = mean(gpr, na.rm = TRUE), .groups = "drop")
  
  # *** RIMOSSA LA SEZIONE PIVOT_LONGER INUTILE ***
  
  message("✅ GPR pronto per il join: ", nrow(gpr_final), " righe")
  
  # Unisci al macro_df
  macro_df <- macro_df %>%
    left_join(gpr_final, by = c("iso", "date"))
  
  message("✅ Join GPR completato: ", sum(!is.na(macro_df$gpr)), " osservazioni con valori GPR")
}
# ---------- 2bis) Aggiungi macro USA da Excel ----------
suppressPackageStartupMessages({ library(readxl); library(stringr) })

# Usa slash in avanti per evitare problemi di escape su Windows
usa_path <- "01_data_clean/macro"

# --- utility: trova e normalizza la colonna data ---
pick_date_col <- function(df) {
  nms <- tolower(names(df))
  cand <- c("date","observation_date","time","data")
  hit  <- match(cand, nms)
  hit  <- hit[!is.na(hit)]
  if (length(hit) == 0) stop("Nessuna colonna data trovata.")
  as.Date(df[[hit[1]]])
}

# 1) Disoccupazione
usa_unemp <- tryCatch(
  read_excel(file.path(usa_path,"UNRATE_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_unemp)) {
  names(usa_unemp) <- tolower(names(usa_unemp))
  # scegli la colonna con il tasso (unemployment, unemployment_rate, unrate, ecc.)
  rate_col <- names(usa_unemp)[str_detect(names(usa_unemp), "unemp|unrate|rate")]
  stopifnot(length(rate_col) >= 1)
  usa_unemp <- tibble::tibble(
    iso = "USA",
    date = pick_date_col(usa_unemp),
    unemployment_rate = as.numeric(usa_unemp[[rate_col[1]]])
  )
}

# 2) Inflazione (CPI): se non c'è YoY, calcoliamo YoY da indice CPI
usa_cpi <- tryCatch(
  read_excel(file.path(usa_path,"inflation_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_cpi)) {
  names(usa_cpi) <- tolower(names(usa_cpi))
  date_vec <- pick_date_col(usa_cpi)
  # preferisci direttamente hicp_yoy / cpi_yoy se presente
  yoy_col <- names(usa_cpi)[str_detect(names(usa_cpi), "yoy")]
  if (length(yoy_col) >= 1) {
    hicp_yoy <- as.numeric(usa_cpi[[yoy_col[1]]])
  } else {
    # altrimenti usa una colonna "cpi" (indice) e calcola YoY in %
    cpi_col <- names(usa_cpi)[str_detect(names(usa_cpi), "^cpi$|consumer|price")]
    stopifnot(length(cpi_col) >= 1)
    cpi <- as.numeric(usa_cpi[[cpi_col[1]]])
    hicp_yoy <- (cpi / dplyr::lag(cpi, 12) - 1) * 100
  }
  usa_cpi <- tibble::tibble(
    iso = "USA",
    date = date_vec,
    hicp_yoy = hicp_yoy
  )
}

# 3) Rendimenti 10Y
usa_yield <- tryCatch(
  read_excel(file.path(usa_path,"yield_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_yield)) {
  names(usa_yield) <- tolower(names(usa_yield))
  date_vec <- pick_date_col(usa_yield)
  ycol <- names(usa_yield)[str_detect(names(usa_yield), "yield|bond")]
  stopifnot(length(ycol) >= 1)
  usa_yield <- tibble::tibble(
    iso = "USA",
    date = date_vec,
    yield_10y = as.numeric(usa_yield[[ycol[1]]])
  )
}

# 4) Credit spread (in basis points se possibile)
usa_cs <- tryCatch(
  read_excel(file.path(usa_path,"credit_spread_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_cs)) {
  names(usa_cs) <- tolower(names(usa_cs))
  date_vec <- pick_date_col(usa_cs)
  scol <- names(usa_cs)[str_detect(names(usa_cs), "credit|spread")]
  stopifnot(length(scol) >= 1)
  usa_cs <- tibble::tibble(
    iso = "USA",
    date = date_vec,
    credit_spread_bp = as.numeric(usa_cs[[scol[1]]])
  )
}

# 5) GDP (mensile o ripetuto su mese se già fornito)
usa_gdp <- tryCatch(
  read_excel(file.path(usa_path,"GDP_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_gdp)) {
  names(usa_gdp) <- tolower(names(usa_gdp))
  date_vec <- pick_date_col(usa_gdp)
  gcol <- names(usa_gdp)[str_detect(names(usa_gdp), "^gdp$|domestic|product")]
  stopifnot(length(gcol) >= 1)
  usa_gdp <- tibble::tibble(
    iso = "USA",
    date = date_vec,
    gdp = as.numeric(usa_gdp[[gcol[1]]])
  )
}

# 6) GPR (serie country-specific per USA)
usa_gpr <- tryCatch(
  read_excel(file.path(usa_path,"gpr_USA.xlsx")),
  error = function(e) NULL
)
if (!is.null(usa_gpr)) {
  names(usa_gpr) <- tolower(names(usa_gpr))
  date_vec <- pick_date_col(usa_gpr)
  gcol <- names(usa_gpr)[str_detect(names(usa_gpr), "^gpr$|geopolitical|risk")]
  stopifnot(length(gcol) >= 1)
  usa_gpr <- tibble::tibble(
    iso = "USA",
    date = date_vec,
    gpr = as.numeric(usa_gpr[[gcol[1]]])
  )
}

# Riduci tutto con full join su chiavi (iso, date)
usa_macro_df <- list(usa_unemp, usa_cpi, usa_yield, usa_cs, usa_gdp, usa_gpr) |>
  purrr::compact() |>
  purrr::reduce(dplyr::full_join, by = c("iso","date")) |>
  dplyr::mutate(
    iso  = toupper(iso),
    date = lubridate::floor_date(as.Date(date), "month")  # armonizza a mensile
  ) |>
  dplyr::group_by(iso, date) |>
  dplyr::summarise(
    dplyr::across(.fns = ~ suppressWarnings(if (is.numeric(.x)) mean(.x, na.rm = TRUE) else dplyr::first(.x))),
    .groups = "drop"
  )

message("✅ USA macro rows: ", nrow(usa_macro_df))

# Appendi le macro USA alle macro EU già costruite
macro_df <- dplyr::bind_rows(macro_df, usa_macro_df) |>
  dplyr::arrange(iso, date)
message("✅ macro_df totale (EU + USA): ", nrow(macro_df))

# ---------- 3) PD medie mensili per country–sector ----------
pd_base <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

pd_df <- pd_base %>%
  transmute(
    iso = toupper(country),
    gdesc = as.character(gdesc),
    # Codice Corretto (forza l'uso della funzione dplyr)
    Cluster = dplyr::recode(as.character(Cluster), 
                            "1" = "Low risk", "2" = "High risk"),
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

# ==========================================================
# 8) GENERAZIONE GRAFICI DI VALIDAZIONE (VISUAL CHECKS)
# ==========================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(corrplot)
  library(tidyr)
})

# --- Configurazione Grafica Base ---
theme_set(theme_minimal(base_size = 12))
PLOT_PATH <- "02_team_modules/D_Dependencies_Macro/plots"
dir.create(PLOT_PATH, showWarnings = FALSE)

## Mappa di Correlazione (Heatmap)

# Seleziona le colonne numeriche di interesse
corr_data <- panel_df %>%
  select(PD_mean, unemployment_rate, hicp_yoy, yield_10y, credit_spread_bp, gdp, gpr) %>%
  drop_na() # Rimuove le righe con NA per la correlazione

# Calcola la matrice di correlazione
M <- cor(corr_data)

# Crea l'immagine della Heatmap
png(file.path(PLOT_PATH, "macro_correlation_heatmap.png"), width = 800, height = 800)
corrplot(M, method = "color", 
         type = "upper", # Mostra solo la metà superiore
         order = "hclust", # Raggruppa le variabili simili
         addCoef.col = "black", # Aggiunge il valore del coefficiente
         tl.cex = 1, tl.col = "black", 
         main = "Matrice di Correlazione tra PD e Variabili Macro")
dev.off()
message("✅ Grafico 3 salvato: macro_correlation_heatmap.png")

# ==========================================================
# 9) SERIE STORICHE MACRO/PD PER PAESE E SETTORE (2015-2024)
# ==========================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(scales)
})

# --- Configurazione e Filtro Temporale ---
PLOT_PATH <- "02_team_modules/D_Dependencies_Macro/plots/ts_by_country_sector"
dir.create(PLOT_PATH, recursive = TRUE, showWarnings = FALSE)

# Filtra il panel per l'intervallo 2015-2024
panel_filtered <- panel_df %>%
  filter(year(date) >= 2015, year(date) <= 2024) %>%
  # Rimuove le righe con Cluster NA che possono causare rumore
  filter(!is.na(Cluster) & Cluster != "NA")


## ----------------------------------------------------------
## A. SERIE STORICHE MACROECONOMICHE (Dati per Paese/ISO)
## ----------------------------------------------------------
# Queste variabili sono le stesse per tutte le industrie dello stesso Paese/Mese.
# Vengono aggregate per ISO/Date.

# Variabili da plottare
macro_vars <- c("unemployment_rate", "hicp_yoy", "yield_10y", "gdp")

# Crea un dataframe pulito per le macro per ISO/Date
macro_ts_df <- panel_filtered %>%
  # Seleziona le colonne macro e le chiavi
  select(iso, date, all_of(macro_vars)) %>%
  # Deduplica, prendendo il valore medio (dovrebbe essere unico per iso/date)
  group_by(iso, date) %>%
  summarise(across(.fns = mean, na.rm = TRUE), .groups = "drop") %>%
  # Trasforma da wide a long per plottare in facette
  pivot_longer(
    cols = all_of(macro_vars),
    names_to = "variable",
    values_to = "value"
  )

# Genera e salva il grafico
p_macro_ts <- macro_ts_df %>%
  ggplot(aes(x = date, y = value, color = iso)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2,
             labeller = as_labeller(c(
               "unemployment_rate" = "Tasso Disoccupazione (%)",
               "hicp_yoy" = "Inflazione YoY (%)",
               "yield_10y" = "Rendimento 10Y (%)",
               "gdp" = "GDP Index/Growth"
             ))) +
  labs(
    title = "Serie Storiche Variabili Macro Economiche per Paese (2015-2024)",
    x = "Data",
    y = "Valore",
    color = "Paese (ISO)"
  ) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

print(p_macro_ts)
ggsave(file.path(PLOT_PATH, "01_macro_ts_by_iso.png"), plot = p_macro_ts, width = 12, height = 8)
message("✅ Grafico Dettagliato 1 salvato: 01_macro_ts_by_iso.png")


## ----------------------------------------------------------
## B. SERIE STORICHE PD MEDIA (Dati per Settore/gdesc)
## ----------------------------------------------------------
# Questo grafico mostra come l'aggregazione di rischio varia tra i settori.

p_pd_sector_ts <- panel_filtered %>%
  # Calcola la PD media per Settore e Paese (mediando tra i cluster)
  group_by(iso, gdesc, date) %>%
  summarise(PD_avg = mean(PD_mean, na.rm = TRUE), .groups = "drop") %>%
  
  ggplot(aes(x = date, y = PD_avg, group = interaction(iso, gdesc), color = gdesc)) +
  geom_line(alpha = 0.7) +
  # Suddivide i grafici per Paese (Paese sulla facetta, Settore sul colore)
  facet_wrap(~ iso, scales = "free_y", ncol = 3) + 
  labs(
    title = "Andamento Storico della PD Media per Settore",
    subtitle = "Rappresenta il rischio specifico di ogni 'gdesc' all'interno dei paesi",
    x = "Data",
    y = "PD Media (q_1y, %)",
    color = "Settore (gdesc)"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

print(p_pd_sector_ts)
ggsave(file.path(PLOT_PATH, "02_pd_ts_by_sector_country.png"), plot = p_pd_sector_ts, width = 14, height = 10)
message("✅ Grafico Dettagliato 2 salvato: 02_pd_ts_by_sector_country.png")