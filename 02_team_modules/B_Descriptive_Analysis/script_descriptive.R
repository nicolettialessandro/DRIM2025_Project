##############################################
# PD assignment — Parte B (Descriptive Analysis)
# Input:  01_data_clean/output_features.parquet
# Output: (salvati in 02_team_modules/B_Descriptive_Analysis/)
#   - output_agg.csv
#   - PD_trends.png
#   - Slope_distribution.png
#   - Box_PD_by_sector.png
#   - (opzionale) Descriptive_stats.txt
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

# --- 4) q_* presenti? se no derivali da kdp_* --------------
have_q <- all(c("q_1m","q_6m","q_1y","q_3y","q_5y") %in% names(df0))
if (!have_q) {
  message("q_* not found. Deriving monthly-equivalent PD from kdp_*.")
  pd_monthly <- function(p, months) 1 - (1 - p)^(months/12)
  for (nm in c("kdp_1mo","kdp_6mo","kdp_1yr","kdp_3yr","kdp_5yr")) {
    if (nm %in% names(df0)) df0[[nm]] <- readr::parse_number(df0[[nm]])
  }
  df0 <- df0 |>
    dplyr::mutate(
      q_1m = if ("kdp_1mo" %in% names(.)) pd_monthly(kdp_1mo, 1) else NA_real_,
      q_6m = if ("kdp_6mo" %in% names(.)) pd_monthly(kdp_6mo, 6) else NA_real_,
      q_1y = if ("kdp_1yr" %in% names(.)) pd_monthly(kdp_1yr,12) else NA_real_,
      q_3y = if ("kdp_3yr" %in% names(.)) pd_monthly(kdp_3yr,36) else NA_real_,
      q_5y = if ("kdp_5yr" %in% names(.)) pd_monthly(kdp_5yr,60) else NA_real_
    )
}

# --- 5) Focus geografico: Europa + USA --------------------
EU_ISO3 <- c("AUT","BEL","CYP","DEU","DNK","ESP","EST","FIN","FRA","GBR",
             "GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL","PRT",
             "SVK","SVN","SWE","CHE")
KEEP <- c(EU_ISO3, "USA")

df <- df0 |> dplyr::filter(country %in% KEEP)

# --- 6) PD wide & GAP/Slope/Curvature ---------------------
wide_pd <- df |>
  dplyr::select(country, sector, date, q_1m, q_6m, q_1y, q_3y, q_5y) |>
  dplyr::mutate(
    q_1m = as.numeric(q_1m),
    q_6m = as.numeric(q_6m),
    q_1y = as.numeric(q_1y),
    q_3y = as.numeric(q_3y),
    q_5y = as.numeric(q_5y)
  )

gaps <- wide_pd |>
  dplyr::mutate(
    gap_6m_1m   = q_6m - q_1m,
    gap_1y_1m   = q_1y - q_1m,
    gap_3y_1m   = q_3y - q_1m,
    gap_5y_1m   = q_5y - q_1m,
    slope_5y_1y = q_5y - q_1y,
    curvature   = q_6m - (q_1m + q_1y)/2
  )

# --- 7) Mediane mensili EU vs USA (per contesto) ----------
country_to_region <- function(x) ifelse(x == "USA","USA","EUROPE")

gaps_summary <- gaps |>
  dplyr::mutate(region = country_to_region(country)) |>
  dplyr::group_by(region, date) |>
  dplyr::summarise(
    med_pd_1m = median(q_1m, na.rm=TRUE),
    med_pd_6m = median(q_6m, na.rm=TRUE),
    med_pd_1y = median(q_1y, na.rm=TRUE),
    med_pd_3y = median(q_3y, na.rm=TRUE),
    med_pd_5y = median(q_5y, na.rm=TRUE),
    med_gap_6m_1m = median(gap_6m_1m, na.rm=TRUE),
    med_gap_1y_1m = median(gap_1y_1m, na.rm=TRUE),
    med_gap_3y_1m = median(gap_3y_1m, na.rm=TRUE),
    med_gap_5y_1m = median(gap_5y_1m, na.rm=TRUE),
    .groups = "drop"
  )

# --- 8) Snapshot ultimo mese (per contesto) ----------------
last_date <- max(gaps$date, na.rm=TRUE)
snapshot <- gaps |>
  dplyr::filter(date == last_date) |>
  dplyr::mutate(region = country_to_region(country)) |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    last_pd_1m = median(q_1m, na.rm=TRUE),
    last_pd_1y = median(q_1y, na.rm=TRUE),
    last_gap_1y_1m = median(gap_1y_1m, na.rm=TRUE),
    n_countries = dplyr::n_distinct(country[!is.na(q_1m) | !is.na(q_1y)]),
    .groups="drop"
  )

# --- 9) Grafici di contesto EU vs USA ---------------------
pd_long_reg <- gaps_summary |>
  dplyr::select(region, date, dplyr::starts_with("med_pd_")) |>
  tidyr::pivot_longer(dplyr::starts_with("med_pd_"),
                      names_to="tenor", values_to="pd") |>
  dplyr::mutate(tenor = stringr::str_remove(tenor, "med_pd_"),
                tenor = factor(tenor, levels=c("1m","6m","1y","3y","5y")))
g1 <- ggplot(pd_long_reg, aes(date, pd, linetype=region)) +
  geom_line() +
  facet_wrap(~tenor, scales="free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Median PD by Region and Horizon", x="Month", y="PD (median)") +
  theme_minimal()

g2 <- ggplot(gaps_summary, aes(date, med_gap_1y_1m, linetype=region)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Median PD Gap (1Y - 1M): Europe vs USA", x="Month", y="Gap (pp)") +
  theme_minimal()

top6 <- gaps |>
  dplyr::group_by(country) |>
  dplyr::summarise(n = sum(!is.na(gap_1y_1m))) |>
  dplyr::arrange(desc(n)) |>
  dplyr::slice(1:6) |>
  dplyr::pull(country)
g3 <- gaps |>
  dplyr::filter(country %in% top6) |>
  ggplot(aes(date, gap_1y_1m)) +
  geom_line() +
  facet_wrap(~country, scales="free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="PD Gap (1Y - 1M) by Country (Top 6 coverage)", x="Month", y="Gap (pp)") +
  theme_minimal()

# --- 10) PARTE B — Output per SETTORE ---------------------

# 10a) Tabella aggregata per data × settore
output_agg <- gaps |>
  dplyr::group_by(date, sector) |>
  dplyr::summarise(
    pd_1m = mean(q_1m, na.rm=TRUE),
    pd_6m = mean(q_6m, na.rm=TRUE),
    pd_1y = mean(q_1y, na.rm=TRUE),
    pd_3y = mean(q_3y, na.rm=TRUE),
    pd_5y = mean(q_5y, na.rm=TRUE),
    slope_5y_1y = mean(slope_5y_1y, na.rm=TRUE),
    curvature   = mean(curvature,   na.rm=TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  )

# 10b) Trend PD(1Y) per settore (line chart)
top_sectors <- gaps |>
  dplyr::count(sector, sort=TRUE) |>
  dplyr::slice(1:10) |>
  dplyr::pull(sector)

g_pd_trends <- output_agg |>
  dplyr::filter(sector %in% top_sectors) |>
  ggplot(aes(date, pd_1y, color = sector)) +
  geom_line(alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="PD(1Y) trend per settore (top 10)", x="Month", y="PD 1Y (mean)") +
  theme_minimal() + theme(legend.position = "bottom")

# 10c) Distribuzione slope(5Y−1Y) per settore (density)
g_slope_distr <- gaps |>
  dplyr::filter(sector %in% top_sectors) |>
  ggplot(aes(slope_5y_1y, color = sector, fill = sector)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Distribuzione di slope (5Y − 1Y) per settore",
       x="Slope (5Y − 1Y)", y="Density") +
  theme_minimal() + theme(legend.position = "bottom")

# 10d) Boxplot PD(1Y) per settore
g_box_pd_sector <- gaps |>
  dplyr::filter(sector %in% top_sectors) |>
  ggplot(aes(x = sector, y = q_1y)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="PD(1Y) per settore — boxplot (top 10)", x="Sector", y="PD 1Y") +
  theme_minimal()

# 10e) (Opzionale) Descriptive_stats per scadenza
desc_stats_tenor <- gaps |>
  tidyr::pivot_longer(cols = c(q_1m,q_6m,q_1y,q_3y,q_5y),
                      names_to = "tenor", values_to = "pd") |>
  dplyr::group_by(tenor) |>
  dplyr::summarise(
    mean = mean(pd, na.rm=TRUE),
    sd   = sd(pd, na.rm=TRUE),
    min  = min(pd, na.rm=TRUE),
    max  = max(pd, na.rm=TRUE),
    .groups = "drop"
  )

# --- 11) Console preview ----------------------------------
message("\n==== HEAD(output_agg) ===="); print(utils::head(output_agg, 8))
message("\n==== Descriptive_stats per tenor ===="); print(desc_stats_tenor)

# --- 12) Salvataggi (nella cartella del modulo B) ---------
if (!TEST_MODE) {
  p <- function(fname) file.path(OUTPUT_DIR, fname)
  
  # EU/USA (di contesto)
  readr::write_csv(gaps,            p("pd_country_monthly.csv"))
  readr::write_csv(gaps_summary,    p("pd_region_medians.csv"))
  readr::write_csv(snapshot,        p("pd_snapshot_lastdate.csv"))
  ggsave(p("fig_pd_levels_by_region.png"), g1, width=10, height=6, dpi=150)
  ggsave(p("fig_gap_1y_1m_region.png"),   g2, width=10, height=6, dpi=150)
  ggsave(p("fig_gap_1y_1m_top6.png"),     g3, width=10, height=6, dpi=150)
  
  # Parte B (richiesta)
  readr::write_csv(output_agg,            p("output_agg.csv"))
  ggsave(p("PD_trends.png"),              g_pd_trends,   width=10, height=6, dpi=150)
  ggsave(p("Slope_distribution.png"),     g_slope_distr, width=10, height=6, dpi=150)
  ggsave(p("Box_PD_by_sector.png"),       g_box_pd_sector, width=9, height=6, dpi=150)
  capture.output(print(desc_stats_tenor), file = p("Descriptive_stats.txt"))
  
  message("Saved to: ", OUTPUT_DIR)
}