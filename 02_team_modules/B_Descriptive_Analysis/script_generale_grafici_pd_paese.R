##############################################
# PD assignment — Parte B (Descriptive Analysis)
# Input:  01_data_clean/output_features.parquet
# Output: (salvati in 02_team_modules/B_Descriptive_Analysis/)
#   - pd_average_over_time.png
##############################################

# --- 0) Settings ------------------------------------------
TEST_MODE <- FALSE     # <-- metti FALSE per scrivere i file su disco
set.seed(1)

# Dove salvare gli output (DIRECT nella cartella del modulo B)
OUTPUT_DIR <- file.path("02_team_modules", "B_Descriptive_Analysis")
if (!TEST_MODE) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- 1) Packages ------------------------------------------
req <- c("arrow", "dplyr", "tidyr", "stringr", "lubridate",
         "readr", "ggplot2", "scales", "broom")
new <- req[!(req %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# --- 2) Read cleaned parquet ------------------------------
parquet_path <- "01_data_clean/output_features.parquet"
stopifnot(file.exists(parquet_path))
message("Reading: ", parquet_path)
raw <- arrow::read_parquet(parquet_path)

# Normalizzo nomi
names(raw) <- tolower(names(raw))

# --- 3) Colonne chiave & coercizioni sicure ---------------
date_col    <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^data_date$|date|time")])
country_col <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^country$|country_code|ctry")])
sector_col  <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^gdesc$|sector|industry|sic_desc")])
stopifnot(!is.na(date_col), !is.na(country_col))

to_Date_safe <- function(x){
  if (inherits(x,"Date")) return(x)
  if (inherits(x,"POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin="1970-01-01"))
  if (is.character(x)) {
    y <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(y))) y <- suppressWarnings(as.Date(x))
    return(y)
  }
  as.Date(x)
}

df0 <- raw |>
  dplyr::rename(date = !!date_col, country = !!country_col) |>
  dplyr::mutate(
    date    = to_Date_safe(date),
    country = toupper(country),
    sector  = if (!is.null(sector_col)) as.character(.data[[sector_col]]) else "UNKNOWN"
  )

# --- 4) Replace "XX" with the desired country code ------
country_code <- "SWE"  # Replace this with the desired country code (e.g., "ITA" for Italy, "USA" for the United States)

# Filtro per il paese desiderato
df_country <- df0 |> dplyr::filter(country == country_code)

# --- 5) Calcolare le medie PD (1M, 1Y, 5Y) nel tempo -------
pd_country_over_time <- df_country |>
  dplyr::select(country, sector, date, q_1m, q_1y, q_5y) |>
  dplyr::mutate(
    q_1m = as.numeric(q_1m),
    q_1y = as.numeric(q_1y),
    q_5y = as.numeric(q_5y),
    month = floor_date(date, "month")  # Aggiungiamo il mese per il calcolo
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    avg_pd_1m = mean(q_1m, na.rm = TRUE),
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    avg_pd_5y = mean(q_5y, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6) Grafico delle medie PD (1M, 1Y, 5Y) nel tempo ------
pd_long <- pd_country_over_time |>
  tidyr::pivot_longer(cols = starts_with("avg_pd"),
                      names_to = "pd_type", values_to = "pd_value") |>
  dplyr::mutate(pd_type = factor(pd_type, levels = c("avg_pd_1m", "avg_pd_1y", "avg_pd_5y"),
                                 labels = c("1-Month PD", "1-Year PD", "5-Year PD")))

g_pd_country_time <- ggplot(pd_long, aes(x = month, y = pd_value, color = pd_type)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(title = paste("PD Over Time for", country_code), x = "Date", y = "Average PD") +
  theme_minimal() +
  theme(legend.title = element_blank())

# --- 7) Salvataggio grafico -------------------------------
if (!TEST_MODE) {
  ggsave(file.path(OUTPUT_DIR, paste0("pd_", country_code, "_average_over_time.png")), g_pd_country_time, width = 10, height = 6, dpi = 150)
  message("Saved to: ", OUTPUT_DIR)
}

# --- 8) Console preview ----------------------------------
message("\n==== HEAD(pd_country_over_time) ===="); print(utils::head(pd_country_over_time, 8))

