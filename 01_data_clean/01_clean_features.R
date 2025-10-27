# === CLEANING & FEATURE CREATION ===
# install.packages(c("data.table", "arrow", "lubridate", "ggplot2"))

library(data.table)
library(arrow)
library(lubridate)
library(ggplot2)


DT_raw <- fread("/Users/nicol/Documents/Documenti/Master/Aix Marseille /Credit Risk/Group Project/DRIM2025_Project/00_data_raw/DRIM_VF.txt",
                dec = ".", showProgress = TRUE)

unique(DT_raw$country)


raw_path <- "/Users/nicol/Documents/Documenti/Master/Aix Marseille /Credit Risk/Group Project/DRIM2025_Project/00_data_raw/DRIM_VF.txt"

# === 1️⃣ Importazione robusta ===
DT <- fread(
  raw_path,
  dec = ".",
  na.strings = c("", "NA", "N/A", ".", "n.a.", "NaN", "999", "9999"),
  showProgress = TRUE
)
setnames(DT, tolower(names(DT)))

# === 2️⃣ Conversione numerica (da % a decimali) ===
num_cols <- grep("^kdp", names(DT), value = TRUE)

DT[, (num_cols) := lapply(.SD, function(x) {
  x <- trimws(x)                  # rimuove spazi
  x <- gsub("%", "", x)           # toglie eventuali simboli %
  x <- gsub(",", ".", x)          # converte virgole in punti
  x <- suppressWarnings(as.numeric(x))
  x <- x / 100                    # ✅ conversione da percentuale a decimale
  pmin(pmax(x, 0), 1)             # forza nel range [0,1]
}), .SDcols = num_cols]

# === 3️⃣ Diagnosi iniziale ===
cat("\n✅ Colonne KDP trovate:", paste(num_cols, collapse = ", "), "\n")
cat("\nRange dopo conversione (min, max):\n")
print(DT[, lapply(.SD, function(x)
  c(min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE))), .SDcols = num_cols])

# === 4️⃣ Definizione scadenze ===
key_pd   <- c("kdp_1mo","kdp_3mo","kdp_6mo","kdp_1yr","kdp_2yr","kdp_3yr","kdp_4yr","kdp_5yr")
other_pd <- c("kdp_7yr","kdp_10yr")

n_start <- nrow(DT)

# === 5️⃣ Filtro geografico: EUROPA + USA ===
# Manteniamo tutti i paesi europei effettivamente presenti nel file + USA
keep_countries <- c(
  "AUT","BEL","CHE","CYP","DEU","DNK","ESP","EST","FIN","FRA",
  "GBR","GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL",
  "PRT","SVK","SVN","SWE","USA"
)

DT <- DT[country %in% keep_countries]
n_geo <- nrow(DT)

# === 6️⃣ Clipping morbido per scadenze lunghe ===
if (length(other_pd) > 0) {
  DT[, (other_pd) := lapply(.SD, function(x) pmin(pmax(x, 0), 1)), .SDcols = other_pd]
}

# === 7️⃣ Tolleranza max 2 NA sulle scadenze chiave ===
missing_matrix <- DT[, lapply(.SD, function(x) !is.finite(x)), .SDcols = key_pd]
DT[, missing_key := rowSums(missing_matrix)]
DT <- DT[missing_key <= 2]
DT[, missing_key := NULL]
n_key <- nrow(DT)

# === 8️⃣ Range check rigido (ora tutto in [0,1]) ===
key_pd <- intersect(key_pd, names(DT))
ok_rows_key <- DT[, apply(.SD, 1, function(x)
  all(is.finite(x) & x >= 0 & x <= 1)), .SDcols = key_pd]
DT <- DT[ok_rows_key]
n_range <- nrow(DT)

# === 🔟 Report ===
cat("\n--- CLEANING REPORT ---\n")
cat("Totale iniziale:", n_start, "\n")
cat("Dopo filtro geografico:", n_geo, " (", round(100*n_geo/n_start,2), "%)\n", sep="")
cat("Dopo filtro scadenze chiave (max 2 NA):", n_key, " (", round(100*n_key/n_start,2), "%)\n", sep="")
cat("Dopo range check [0,1]:", n_range, " (", round(100*n_range/n_start,2), "%)\n", sep="")
cat("Totale finale:", nrow(DT), "\n")

# === 11️⃣ Funzione PD mensile ===
pd_monthly <- function(p, months) 1 - (1 - p)^(months / 12)

# === 12️⃣ Crea PD equivalenti mensili ===
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

# === 13️⃣ Costruisci slope e curvature ===
DT[, slope_1y_1m := q_1y - q_1m]
DT[, slope_6m_1m := q_6m - q_1m]
DT[, slope_1y_6m := q_1y - q_6m]
DT[, slope_5y_1y := q_5y - q_1y]
DT[, slope_10y_1y := q_10y - q_1y]
DT[, curvature := q_6m - (q_1m + q_1y) / 2]
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

nrow(DT)