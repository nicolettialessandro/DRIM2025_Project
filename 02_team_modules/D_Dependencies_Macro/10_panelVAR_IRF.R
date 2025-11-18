# ==========================================================
# GRANGER CAUSALITY SEMPLICE — PD vs variabili macro
# ==========================================================

library(arrow)
library(dplyr)
library(lubridate)
library(tseries)   # per grangertest
library(tidyr)

panel <- read_parquet("02_team_modules/D_Dependencies_Macro/panel_df.parquet")

# 1) Aggrega il panel a livello MENSILE (mean di tutte le country-sector PD)
agg_df <- panel %>%
  group_by(date = floor_date(as.Date(date), "month")) %>%
  summarise(
    PD = mean(PD_mean, na.rm = TRUE),
    unemp = mean(unemployment_rate, na.rm = TRUE),
    hicp = mean(hicp_yoy, na.rm = TRUE),
    yield = mean(yield_10y, na.rm = TRUE),
    spread = mean(credit_spread_bp, na.rm = TRUE),
    gdp = mean(gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

# 2) Converte in ts (frequenza mensile)
start_year <- year(min(agg_df$date))
start_month <- month(min(agg_df$date))

PD_ts     <- ts(agg_df$PD,     start = c(start_year, start_month), frequency = 12)
unemp_ts  <- ts(agg_df$unemp,  start = c(start_year, start_month), frequency = 12)
hicp_ts   <- ts(agg_df$hicp,   start = c(start_year, start_month), frequency = 12)
yield_ts  <- ts(agg_df$yield,  start = c(start_year, start_month), frequency = 12)
spread_ts <- ts(agg_df$spread, start = c(start_year, start_month), frequency = 12)
gdp_ts    <- ts(agg_df$gdp,    start = c(start_year, start_month), frequency = 12)

# 3) Define helper function for Granger
granger_test <- function(x, y, lags = 3) {
  # x Granger-causes y ?
  gt <- grangertest(y ~ x, order = lags)
  data.frame(
    Cause = deparse(substitute(x)),
    Effect = deparse(substitute(y)),
    F = gt$F[2],
    pvalue = gt$`Pr(>F)`[2]
  )
}

# 4) Esegui Granger PD <- macro
res <- bind_rows(
  granger_test(unemp_ts,  PD_ts, lags = 3),
  granger_test(hicp_ts,   PD_ts, lags = 3),
  granger_test(yield_ts,  PD_ts, lags = 3),
  granger_test(spread_ts, PD_ts, lags = 3),
  granger_test(gdp_ts,    PD_ts, lags = 3)
)

print(res)

# Salva risultati
write.csv(res, "02_team_modules/D_Dependencies_Macro/granger_results_simple.csv",
          row.names = FALSE)

message("🎉 Granger test completato e salvato!")