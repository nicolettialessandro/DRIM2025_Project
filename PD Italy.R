##############################################
# PD assignment — Parte B (Descriptive Analysis)
# Input:  01_data_clean/output_features.parquet
# Output: (salvati in 02_team_modules/B_Descriptive_Analysis/)
#   - pd_1m_italy_over_time.png
##############################################

# --- 0) Settings ------------------------------------------
TEST_MODE <- FALSE     # <-- metti FALSE per scrivere i file su disco
set.seed(1)

# Dove salvare gli output (DIRECT nella cartella del modulo B)
OUTPUT_DIR <- file.path("02_team_modules", "B_Descriptive_Analysis")
if (!TEST_MODE) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- 1) Packages ------------------------------------------
req <- c("arrow","dplyr","tidyr","stringr","lubridate",
         "readr","ggplot2","scales","broom")
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

# --- 4) Filtro per Italia (ITA) --------------------------
df_italy <- df0 |> dplyr::filter(country == "ITA")

# --- 5) Calcolare la media mensile di PD 1M in Italia -------
pd_1m_italy_over_time <- df_italy |>
  dplyr::select(country, sector, date, q_1m) |>
  dplyr::mutate(
    q_1m = as.numeric(q_1m),
    month = floor_date(date, "month")  # Aggiungiamo il mese per il calcolo
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    avg_pd_1m = mean(q_1m, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6) Grafico della media PD (1M) in Italia nel tempo ------
g_pd_1m_italy_time <- ggplot(pd_1m_italy_over_time, aes(x = month, y = avg_pd_1m)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(title = "1-Month PD Over Time for Italy", x = "Date", y = "Average 1-Month PD") +
  theme_minimal()

# --- 7) Salvataggio grafico -------------------------------
if (!TEST_MODE) {
  ggsave(file.path(OUTPUT_DIR, "pd_1m_italy_over_time.png"), g_pd_1m_italy_time, width = 10, height = 6, dpi = 150)
  message("Saved to: ", OUTPUT_DIR)
}

# --- 8) Console preview ----------------------------------
message("\n==== HEAD(pd_1m_italy_over_time) ===="); print(utils::head(pd_1m_italy_over_time, 8))

plot(g_pd_1m_italy_time)

##############################################
# PD assignment — Parte B (Descriptive Analysis)
# Input:  01_data_clean/output_features.parquet
# Output: (salvati in 02_team_modules/B_Descriptive_Analysis/)
#   - pd_1y_italy_average_over_time.png
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

# --- 4) Filtro per Italia (ITA) --------------------------
df_italy <- df0 |> dplyr::filter(country == "ITA")

# --- 5) Calcolare la media annuale di PD 1Y in Italia -------
pd_1y_italy_over_time <- df_italy |>
  dplyr::select(country, sector, date, q_1y) |>
  dplyr::mutate(
    q_1y = as.numeric(q_1y),
    month = floor_date(date, "month")  # Aggiungiamo il mese per il calcolo
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6) Grafico della media PD (1Y) in Italia nel tempo ------
g_pd_1y_italy_time <- ggplot(pd_1y_italy_over_time, aes(x = month, y = avg_pd_1y)) +
  geom_line(color = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(title = "1-Year PD Over Time for Italy", x = "Date", y = "Average 1-Year PD") +
  theme_minimal()

# --- 7) Salvataggio grafico -------------------------------
if (!TEST_MODE) {
  ggsave(file.path(OUTPUT_DIR, "pd_1y_italy_average_over_time.png"), g_pd_1y_italy_time, width = 10, height = 6, dpi = 150)
  message("Saved to: ", OUTPUT_DIR)
}

# --- 8) Console preview ----------------------------------
message("\n==== HEAD(pd_1y_italy_over_time) ===="); print(utils::head(pd_1y_italy_over_time, 8))

plot(g_pd_1y_italy_time)

##############################################
# PD assignment — Parte B (Descriptive Analysis)
# Input:  01_data_clean/output_features.parquet
# Output: (salvati in 02_team_modules/B_Descriptive_Analysis/)
#   - pd_5y_italy_average_over_time.png
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

# --- 4) Filtro per Italia (ITA) --------------------------
df_italy <- df0 |> dplyr::filter(country == "ITA")

# --- 5) Calcolare la media quinquennale di PD 5Y in Italia --
pd_5y_italy_over_time <- df_italy |>
  dplyr::select(country, sector, date, q_5y) |>
  dplyr::mutate(
    q_5y = as.numeric(q_5y),
    month = floor_date(date, "month")  # Aggiungiamo il mese per il calcolo
  ) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    avg_pd_5y = mean(q_5y, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6) Grafico della media PD (5Y) in Italia nel tempo ------
g_pd_5y_italy_time <- ggplot(pd_5y_italy_over_time, aes(x = month, y = avg_pd_5y)) +
  geom_line(color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(title = "5-Year PD Over Time for Italy", x = "Date", y = "Average 5-Year PD") +
  theme_minimal()

# --- 7) Salvataggio grafico -------------------------------
if (!TEST_MODE) {
  ggsave(file.path(OUTPUT_DIR, "pd_5y_italy_average_over_time.png"), g_pd_5y_italy_time, width = 10, height = 6, dpi = 150)
  message("Saved to: ", OUTPUT_DIR)
}

# --- 8) Console preview ----------------------------------
message("\n==== HEAD(pd_5y_italy_over_time) ===="); print(utils::head(pd_5y_italy_over_time, 8))

plot(g_pd_5y_italy_time)
