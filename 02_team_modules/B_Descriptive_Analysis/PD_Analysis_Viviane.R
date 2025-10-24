##############################################
# PD assignment - Cleaned inputs (TEST MODE)
# Legge: 01_data_clean/output_features.parquet
##############################################

# --- 0) Settings ------------------------------------------
TEST_MODE <- TRUE      # FALSE per salvare CSV/PNG in 03_outputs/
set.seed(1)

# --- 1) Packages ------------------------------------------
req <- c("arrow","dplyr","tidyr","stringr","lubridate",
         "readr","ggplot2","scales","broom")
new <- req[!(req %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# --- 2) Read cleaned parquet (percorso esplicito) ---------
parquet_path <- "01_data_clean/output_features.parquet"
stopifnot(file.exists(parquet_path))
message("Reading: ", parquet_path)
raw <- arrow::read_parquet(parquet_path)

# Normalizzo nomi
names(raw) <- names(raw) |> tolower()

# --- 3) Colonne chiave & coercizioni sicure ---------------
# data
date_col <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^data_date$|date|time")])
stopifnot(!is.na(date_col))
# paese (ISO3)
country_col <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^country$|country_code|ctry")])
stopifnot(!is.na(country_col))

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
  dplyr::mutate(date = to_Date_safe(date),
                country = toupper(country))

# --- 4) q_* già presenti? Se no li ricavo da kdp_* --------
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

# --- 6) Wide PDs & Gaps -----------------------------------
wide_pd <- df |>
  dplyr::select(country, date, q_1m, q_6m, q_1y, q_3y, q_5y) |>
  dplyr::mutate(
    q_1m = as.numeric(q_1m),
    q_6m = as.numeric(q_6m),
    q_1y = as.numeric(q_1y),
    q_3y = as.numeric(q_3y),
    q_5y = as.numeric(q_5y)
  )

gaps <- wide_pd |>
  dplyr::mutate(
    gap_6m_1m = q_6m - q_1m,
    gap_1y_1m = q_1y - q_1m,
    gap_3y_1m = q_3y - q_1m,
    gap_5y_1m = q_5y - q_1m
  )

# --- 7) Mediane mensili EU vs USA -------------------------
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
    .groups="drop"
  )

# --- 8) Snapshot ultimo mese ------------------------------
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

# --- 9) Plots ---------------------------------------------
# 9a) PD mediana per area e orizzonte
pd_long_reg <- gaps_summary |>
  dplyr::select(region, date, dplyr::starts_with("med_pd_")) |>
  tidyr::pivot_longer(dplyr::starts_with("med_pd_"),
                      names_to="tenor", values_to="pd") |>
  dplyr::mutate(tenor = stringr::str_remove(tenor,"med_pd_"),
                tenor = factor(tenor, levels=c("1m","6m","1y","3y","5y")))

g1 <- ggplot(pd_long_reg, aes(date, pd, linetype=region)) +
  geom_line() +
  facet_wrap(~tenor, scales="free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Median PD by Region and Horizon",
       x="Month", y="PD (median)") +
  theme_minimal()
print(g1)

# 9b) Gap (1Y-1M) mediano: EU vs USA
g2 <- ggplot(gaps_summary, aes(date, med_gap_1y_1m, linetype=region)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Median PD Gap (1Y - 1M): Europe vs USA",
       x="Month", y="Gap (pp)") +
  theme_minimal()
print(g2)

# 9c) Small-multiples per Paese (Top 6 copertura)
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
  labs(title="PD Gap (1Y - 1M) by Country (Top 6 coverage)",
       x="Month", y="Gap (pp)") +
  theme_minimal()
print(g3)

# --- 10) Descriptive statistics ---------------------------
# PD per Regione x Tenor
pd_long_all <- gaps |>
  dplyr::mutate(region = country_to_region(country)) |>
  tidyr::pivot_longer(cols = c(q_1m,q_6m,q_1y,q_3y,q_5y),
                      names_to="tenor", values_to="pd")

pd_desc_region <- pd_long_all |>
  dplyr::group_by(region, tenor) |>
  dplyr::summarise(
    n_total  = dplyr::n(),
    n_non_na = sum(!is.na(pd)),
    mean     = mean(pd, na.rm=TRUE),
    sd       = sd(pd, na.rm=TRUE),
    min      = min(pd, na.rm=TRUE),
    q25      = quantile(pd, 0.25, na.rm=TRUE),
    median   = median(pd, na.rm=TRUE),
    q75      = quantile(pd, 0.75, na.rm=TRUE),
    max      = max(pd, na.rm=TRUE),
    .groups  = "drop"
  )

# GAP per Regione x Tipo
gaps_long <- gaps |>
  dplyr::mutate(region = country_to_region(country)) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("gap_"),
                      names_to="gap", values_to="value")

gap_desc_region <- gaps_long |>
  dplyr::group_by(region, gap) |>
  dplyr::summarise(
    n_total  = dplyr::n(),
    n_non_na = sum(!is.na(value)),
    mean     = mean(value, na.rm=TRUE),
    sd       = sd(value, na.rm=TRUE),
    min      = min(value, na.rm=TRUE),
    q25      = quantile(value, 0.25, na.rm=TRUE),
    median   = median(value, na.rm=TRUE),
    q75      = quantile(value, 0.75, na.rm=TRUE),
    max      = max(value, na.rm=TRUE),
    .groups  = "drop"
  )

# --- 11) Trend test: med_gap_1y_1m ~ t --------------------
trend_region <- gaps_summary |>
  dplyr::mutate(t = as.numeric(date)) |>
  dplyr::group_by(region) |>
  dplyr::do(broom::tidy(lm(med_gap_1y_1m ~ t, data = .), conf.int=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::filter(term=="t") |>
  dplyr::mutate(
    slope_per_day  = estimate,
    slope_per_year = estimate * 365.25
  ) |>
  dplyr::select(region, slope_per_year, std.error, conf.low, conf.high, p.value)

# --- 12) Extra plots: distribuzioni -----------------------
g4 <- gaps_long |>
  dplyr::filter(gap=="gap_1y_1m") |>
  ggplot(aes(x=region, y=value)) +
  geom_boxplot(outlier.alpha=0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="Distribution of PD Gap (1Y - 1M) by Region",
       x="Region", y="Gap (pp)") +
  theme_minimal()
print(g4)

g5 <- pd_long_all |>
  ggplot(aes(x=region, y=pd)) +
  geom_boxplot(outlier.alpha=0.4) +
  facet_wrap(~tenor, scales="free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01)) +
  labs(title="PD Levels by Region and Tenor (Distribution over time)",
       x="Region", y="PD") +
  theme_minimal()
print(g5)

# --- 13) Console previews ---------------------------------
message("\n==== HEAD(gaps_summary) ===="); print(utils::head(gaps_summary, 8))
message("\n==== snapshot (", as.character(last_date), ") ===="); print(snapshot)
message("\n==== trend_region (slope per year) ===="); print(trend_region)
message("\n==== PD descriptives (region x tenor) ===="); print(utils::head(pd_desc_region, 10))
message("\n==== GAP descriptives (region x type) ===="); print(utils::head(gap_desc_region, 10))

# --- 14) Optional save ------------------------------------
if (!TEST_MODE) {
  dir.create("03_outputs", showWarnings = FALSE)
  readr::write_csv(gaps,            "03_outputs/pd_country_monthly.csv")
  readr::write_csv(gaps_summary,    "03_outputs/pd_region_medians.csv")
  readr::write_csv(snapshot,        "03_outputs/pd_snapshot_lastdate.csv")
  readr::write_csv(pd_desc_region,  "03_outputs/desc_pd_region_tenor.csv")
  readr::write_csv(gap_desc_region, "03_outputs/desc_gap_region_type.csv")
  readr::write_csv(trend_region,    "03_outputs/trend_gap_region.csv")
  ggsave("03_outputs/fig_pd_levels_by_region.png", g1, width=10, height=6, dpi=150)
  ggsave("03_outputs/fig_gap_1y_1m_region.png",   g2, width=10, height=6, dpi=150)
  ggsave("03_outputs/fig_gap_1y_1m_top6.png",     g3, width=10, height=6, dpi=150)
  ggsave("03_outputs/fig_box_gap_region.png",     g4, width=8,  height=5, dpi=150)
  ggsave("03_outputs/fig_box_pd_region_tenor.png",g5, width=10, height=6, dpi=150)
  message("Saved to 03_outputs/ .")
}