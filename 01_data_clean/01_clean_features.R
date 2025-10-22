#install.packages(c("data.table", "arrow", "lubridate"))
#include libraries
#install.packages("ggplot2")
library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)

raw_path <- "/Users/nicol/Documents/Documenti/Master/Aix Marseille /Credit Risk/Group Project/DRIM2025_Project/00_data_raw/DRIM_VF.txt"
DT <- fread(raw_path)

str(DT)
head(DT)

# 5️⃣ Converti tipi e nomi di colonne
setnames(DT, tolower(names(DT)))           # tutto minuscolo
DT[, data_date := as.IDate(data_date)]     # converte la colonna data
num_cols <- grep("^kdp", names(DT), value = TRUE)  # individua colonne PD
DT[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# === 6) Pulizia + filtro geografico + range check (per kdp_* in [0,1]) ===

# 6.1 Duplicati per firm-data
DT <- unique(DT, by = c("tic", "data_date"))

# 6.2 Lista delle colonne KDP presenti (intersezione per sicurezza)
kdp_cols_expected <- c("kdp_1mo","kdp_3mo","kdp_6mo",
                       "kdp_1yr","kdp_2yr","kdp_3yr","kdp_4yr","kdp_5yr","kdp_7yr","kdp_10yr")
kdp_cols <- intersect(kdp_cols_expected, names(DT))
if (length(kdp_cols) == 0) stop("Non trovo colonne KDP nel dataset dopo il rename.")

# 6.3 Rimuovi righe con NA nelle KDP (se vuoi essere meno severa, usa any invece di all)
DT <- DT[
  DT[, Reduce(`&`, lapply(.SD, function(x) is.finite(x))), .SDcols = kdp_cols]
]

# 6.4 Mantieni solo righe con tutte le KDP nel range [0,1]
DT <- DT[
  DT[, Reduce(`&`, lapply(.SD, function(x) x >= 0 & x <= 1)), .SDcols = kdp_cols]
]

# 6.5 Filtro geografico: Europa + USA (Canada escluso)
keep_countries <- c(
  "AUT","BEL","DNK","FIN","FRA","DEU","ITA","NLD","NOR","ESP",
  "SWE","CHE","GBR","IRL","PRT","GRC","LUX","USA"
)

DT <- DT[country %in% keep_countries]

pd_monthly <- function(p, months) {
  1 - (1 - p)^(months / 12)
}

# Calcola PD mensili equivalenti per tutte le scadenze principali
DT[, q_1m  := pd_monthly(kdp_1mo, 1)]
DT[, q_3m  := pd_monthly(kdp_3mo, 3)]
DT[, q_6m  := pd_monthly(kdp_6mo, 6)]
DT[, q_1y  := pd_monthly(kdp_1yr, 12)]
DT[, q_2y  := pd_monthly(kdp_2yr, 24)]
DT[, q_3y  := pd_monthly(kdp_3yr, 36)]
DT[, q_4y  := pd_monthly(kdp_4yr, 48)]
DT[, q_5y  := pd_monthly(kdp_5yr, 60)]
DT[, q_7y  := pd_monthly(kdp_7yr, 84)]
DT[, q_10y := pd_monthly(kdp_10yr, 120)]

# slope = differenza tra PD a 1 anno e a 1 mese
DT[, slope_1y_1m := q_1y - q_1m]

# slope 6 mesi - 1 mese
DT[, slope_6m_1m := q_6m - q_1m]

# slope 1 anno - 6 mesi

DT[, slope_1y_6m := q_1y - q_6m]

# slope 5 anni - 1 anno (lungo termine)

DT[, slope_5y_1y := q_5y - q_1y]

# slope 10 anni - 1 anno (extra lungo)

DT[, slope_10y_1y := q_10y - q_1y]

# curvature = deviazione del punto medio (6m) dalla media dei due estremi
DT[, curvature := q_6m - (q_1m + q_1y) / 2]

# slope di lungo termine: 5 anni vs 1 anno
DT[, slope_5y_1y := q_5y - q_1y]

# curvature a lungo termine (3y rispetto a media di 1y e 5y)
DT[, curvature_5y := q_3y - (q_1y + q_5y) / 2]

summary(DT[, .(q_1m, q_3m, q_6m, q_1y, slope_1y_1m, curvature)])


library(ggplot2)

DT_sample <- DT[sample(.N, 5000)]

ggplot(DT_sample, aes(x = slope_1y_1m, y = curvature, color = gdesc)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
  labs(
    title = "Relationship between slopes and curvature (sample 5000 companies)",
    x = "Slope (1Y - 1M)",
    y = "Curvature (6M vs avarage of 1M and 1Y)",
    color = "Sector"
  ) +
  theme_minimal(base_size = 13)

ggplot(DT, aes(x = slope_1y_1m, y = curvature, color = gdesc)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
  labs(
    title = "Relazione tra slope e curvature della term structure delle PD",
    subtitle = "Analisi cross-section delle imprese (1M–1Y)",
    x = "Slope (1Y - 1M)",
    y = "Curvature (6M vs media di 1M e 1Y)",
    color = "Settore"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  )
#stampa output
write_parquet(
  DT,
  "01_data_clean/output_features.parquet"
)