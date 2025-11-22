# ==========================================================
# 07_macro_models.R — Two-Way Fixed Effects (Panel Model)
# ==========================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(broom)
  library(plm)
  library(readr)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
  library(readxl)    # <-- per gli Excel USA
  library(stringr)   # <-- per matching nomi colonne
  library(purrr)     # <-- per reduce/compact
})

# === 1️⃣ Load panel ===
panel_df <- read_parquet("02_team_modules/D_Dependencies_Macro/panel_df.parquet")

# === 2️⃣ Clean ===
panel_df <- panel_df %>%
  mutate(
    iso = toupper(as.character(iso)),
    Cluster = as.factor(Cluster),
    date = as.Date(date),
    PD_mean = as.numeric(PD_mean),
    unemployment_rate = as.numeric(unemployment_rate),
    hicp_yoy = as.numeric(hicp_yoy),
    yield_10y = as.numeric(yield_10y),
    credit_spread_bp = as.numeric(credit_spread_bp),
    gdp = as.numeric(gdp),
    gpr = as.numeric(gpr)
  ) %>%
  filter(!is.na(PD_mean), !is.na(iso), !is.na(date))

# === 2bis) Integra le macro USA da Excel e fai join su (iso,date) ===
# (Usa questo blocco quando le macro USA non erano state caricate nel panel precedente)
usa_path <- "01_data_clean/macro"

# Utility per riconoscere la colonna data anche se ha nomi atipici
pick_date_col <- function(df) {
  nms <- tolower(names(df))
  cand <- c("date","observation_date","time","data","month","period")
  hit  <- match(cand, nms)
  hit  <- hit[!is.na(hit)]
  if (length(hit) == 0) stop("Nessuna colonna data trovata.")
  as.Date(df[[hit[1]]])
}

# Carica e normalizza una singola serie da Excel
load_excel_series <- function(file, value_patterns, out_col) {
  df <- tryCatch(read_excel(file), error = function(e) NULL)
  if (is.null(df)) return(NULL)
  names(df) <- tolower(names(df))
  date_vec <- pick_date_col(df)
  vcol <- names(df)[str_detect(names(df), paste0("(", paste(value_patterns, collapse="|"), ")"))]
  stopifnot("Colonna valori non trovata" = length(vcol) >= 1)
  tibble(
    iso = "USA",
    date = date_vec,
    !!out_col := as.numeric(df[[vcol[1]]])
  )
}

# 1) Unemployment
usa_unemp <- load_excel_series(
  file = file.path(usa_path,"UNRATE_USA.xlsx"),
  value_patterns = c("unemp","unrate","rate"),
  out_col = "unemployment_rate"
)

# 2) CPI YoY (se non c'è YoY, calcola da indice CPI)
usa_cpi_raw <- tryCatch(read_excel(file.path(usa_path,"inflation_USA.xlsx")), error = function(e) NULL)
if (!is.null(usa_cpi_raw)) {
  names(usa_cpi_raw) <- tolower(names(usa_cpi_raw))
  dvec <- pick_date_col(usa_cpi_raw)
  yoy_col <- names(usa_cpi_raw)[str_detect(names(usa_cpi_raw), "yoy")]
  if (length(yoy_col) >= 1) {
    usa_cpi <- tibble(iso="USA", date=dvec, hicp_yoy = as.numeric(usa_cpi_raw[[yoy_col[1]]]))
  } else {
    cpi_col <- names(usa_cpi_raw)[str_detect(names(usa_cpi_raw), "^cpi$|consumer|price|index")]
    stopifnot("Colonna CPI non trovata" = length(cpi_col) >= 1)
    cpi <- as.numeric(usa_cpi_raw[[cpi_col[1]]])
    usa_cpi <- tibble(iso="USA", date=dvec, hicp_yoy = (cpi/lag(cpi,12) - 1)*100)
  }
} else {
  usa_cpi <- NULL
}

# 3) 10Y yield
usa_yield <- load_excel_series(
  file = file.path(usa_path,"yield_USA.xlsx"),
  value_patterns = c("yield","bond","10"),
  out_col = "yield_10y"
)

# 4) Credit spread (in bp)
usa_cs <- load_excel_series(
  file = file.path(usa_path,"credit_spread_USA.xlsx"),
  value_patterns = c("credit","spread"),
  out_col = "credit_spread_bp"
)

# 5) GDP
usa_gdp <- load_excel_series(
  file = file.path(usa_path,"GDP_USA.xlsx"),
  value_patterns = c("^gdp$","domestic","product"),
  out_col = "gdp"
)

# 6) GPR
usa_gpr <- load_excel_series(
  file = file.path(usa_path,"gpr_USA.xlsx"),
  value_patterns = c("^gpr$","geopolitical","risk"),
  out_col = "gpr"
)

# Unisci tutte le serie USA e porta a frequenza mensile (primo giorno del mese)
usa_macro_df <- list(usa_unemp, usa_cpi, usa_yield, usa_cs, usa_gdp, usa_gpr) %>%
  purrr::compact() %>%
  purrr::reduce(dplyr::full_join, by = c("iso","date")) %>%
  mutate(
    iso  = toupper(iso),
    date = floor_date(as.Date(date), "month")
  ) %>%
  group_by(iso, date) %>%
  summarise(
    across(.fns = ~ suppressWarnings(if (is.numeric(.x)) mean(.x, na.rm = TRUE) else dplyr::first(.x))),
    .groups = "drop"
  )

message("✅ USA macro rows loaded: ", nrow(usa_macro_df))

# Fai il join dentro il panel già caricato (riempie le macro NA degli USA)
panel_df <- panel_df %>%
  left_join(usa_macro_df, by = c("iso","date"), suffix = c("", ".usa")) %>%
  mutate(
    unemployment_rate = if_else(iso=="USA" & is.na(unemployment_rate), `unemployment_rate.usa`, unemployment_rate),
    hicp_yoy          = if_else(iso=="USA" & is.na(hicp_yoy),          `hicp_yoy.usa`,          hicp_yoy),
    yield_10y         = if_else(iso=="USA" & is.na(yield_10y),         `yield_10y.usa`,         yield_10y),
    credit_spread_bp  = if_else(iso=="USA" & is.na(credit_spread_bp),  `credit_spread_bp.usa`,  credit_spread_bp),
    gdp               = if_else(iso=="USA" & is.na(gdp),               `gdp.usa`,               gdp),
    gpr               = if_else(iso=="USA" & is.na(gpr),               `gpr.usa`,               gpr)
  ) %>%
  select(-ends_with(".usa"))

# === 2c) Overwrite GPR with your country-level GPR series ===
gpr_df <- read_parquet("01_data_clean/macro/gpr.parquet")

panel_df <- panel_df %>%
  select(-gpr) %>%                                  # rimuove il placeholder
  left_join(gpr_df, by = c("iso","date")) %>%       # merge ufficiale del tuo GPR
  rename(gpr = gpr)                                  # garantisce il nome corretto

# === 3️⃣ Z-score delle variabili macro ===
scale_safe <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) rep(NA_real_, length(x)) else as.numeric(scale(x))
}

panel_df <- panel_df %>%
  mutate(
    z_unemp  = scale_safe(unemployment_rate),
    z_infl   = scale_safe(hicp_yoy),
    z_yield  = scale_safe(yield_10y),
    z_spread = scale_safe(credit_spread_bp),
    z_gdp    = scale_safe(gdp),
    z_gpr    = scale_safe(gpr)
  )

# === 4️⃣ Crea panel data frame (plm) ===
panel_data <- pdata.frame(panel_df, index = c("iso", "date"))

# === 5️⃣ Two-Way Fixed Effects (country + time) ===
fe_tw <- plm(
  PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
  data = panel_data,
  model = "within",
  effect = "twoways"
)

panel_df %>%
  group_by(iso) %>%
  summarise(sd_spread = sd(credit_spread_bp, na.rm=TRUE)) %>%
  arrange(sd_spread)

# === 6️⃣ Coefficienti robusti (clustered by country) ===
coefs_tw <- coeftest(fe_tw, vcov = vcovHC(fe_tw, type = "HC1", cluster = "group"))

# === 7️⃣ Salvataggio risultati ===
tidy_fe <- tidy(coefs_tw)
write_csv(tidy_fe, "02_team_modules/D_Dependencies_Macro/panel_FE_twoways_coefs.csv")

fit_fe <- glance(fe_tw)
write_csv(fit_fe, "02_team_modules/D_Dependencies_Macro/panel_FE_twoways_fit.csv")

message("✅ Two-Way Fixed Effects model completed")

# === 7bis) Salva il modello e gli oggetti principali in .rds ===
saveRDS(
  list(
    model_plm   = fe_tw,                              # oggetto plm
    vcov_robust = vcovHC(fe_tw, type = "HC1", cluster = "group"),  # matrice vcov robusta
    coeftest    = coefs_tw,                           # test coefficienti con SE robusti
    tidy_coefs  = tidy_fe,                            # tabella tidy dei coefficienti
    fit_glance  = fit_fe,                             # statistiche di fit (glance)
    n_obs       = nobs(fe_tw),
    call        = fe_tw$call,
    sessionInfo = utils::sessionInfo()
  ),
  file = "02_team_modules/D_Dependencies_Macro/panel_FE_twoways_model.rds"
)
message("💾 Saved RDS: panel_FE_twoways_model.rds")


# === 8️⃣ Plot dei coefficienti ===
ggplot(tidy_fe %>% filter(term != "(Intercept)"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Macro Drivers of Default Probability (Two-Way FE)",
       x = NULL, y = "Coefficient (Robust SE)")

ggsave("02_team_modules/D_Dependencies_Macro/fig_FE_twoways_betas.png",
       width = 7, height = 5, dpi = 150)

message("🎉 Finished successfully — Two-Way FE results saved.")


m_ols <- lm(PD_mean ~ z_gpr + z_unemp + z_infl + z_yield + z_spread + z_gdp,
            data = panel_df)

coeftest(m_ols, vcov = vcovHC(m_ols, type="HC1"))
print(coefs_tw)



panel_df %>% 
  group_by(iso) %>% 
  summarise(unique_gpr = n_distinct(gpr)) %>% 
  arrange(unique_gpr)

panel_df %>%
  group_by(iso) %>% 
  summarise(n_unique = n_distinct(gpr)) %>%
  arrange(n_unique)


panel_df %>%
  group_by(iso) %>%
  summarise(n_unique = n_distinct(gpr))


print(coefs_tw)


pFtest(fe_tw, plm(PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
                  data=panel_data, model="pooling"))


re_mod <- plm(PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
              data=panel_data, model="random")
phtest(fe_tw, re_mod)

pFtest(fe_tw, update(fe_tw, effect="individual"))