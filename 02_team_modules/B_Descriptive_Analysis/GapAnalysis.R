#prova 1 -> timeseries line chart
gap_ts <- gaps %>%
  mutate(month = floor_date(date, "month"),
         gap_5y_1y = q_5y - q_1y) %>%
  group_by(sector, month) %>%
  summarise(median_gap = median(gap_5y_1y, na.rm = TRUE), .groups="drop")

ggplot(gap_ts, aes(x = month, y = median_gap, color = sector)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "PD Term-Structure Gap (5Y – 1Y) Over Time by Sector",
       x = "Month", y = "Median Gap") +
  theme_minimal()
#prova 2 -> matrix
gap_matrix <- gaps %>%
  mutate(gap_5y_1y = q_5y - q_1y) %>%
  group_by(country, sector) %>%
  summarise(med_gap = median(gap_5y_1y, na.rm=TRUE), .groups = "drop")

ggplot(gap_matrix, aes(x = country, y = sector, fill = med_gap)) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(title = "Sector–Country PD Term-Structure Gap (5Y – 1Y)",
       x = "Country", y = "Sector", fill = "Gap") +
  theme_minimal()
#prova 3
gaps_scatter <- gaps %>%
  mutate(gap_5y_1y = q_5y - q_1y)

ggplot(gaps_scatter, aes(x = q_1y, y = q_5y, color = sector)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "PD 1Y vs PD 5Y (Diagonal = Flat Curve)",
       x = "PD 1Y", y = "PD 5Y") +
  theme_minimal()
#prova 4
gap_dens <- gaps %>%
  mutate(gap_5y_1y = q_5y - q_1y)

ggplot(gap_dens, aes(x = gap_5y_1y)) +
  geom_density(fill="blue", alpha=0.4) +
  scale_x_continuous(labels = scales::percent) +
  labs(title="Density of PD Term-Structure Gap (5Y – 1Y)",
       x="Gap", y="Density") +
  theme_minimal()
#prova 5
gap_rank <- gaps %>%
  mutate(gap_5y_1y = q_5y - q_1y) %>%
  group_by(sector) %>%
  summarise(med_gap = median(gap_5y_1y, na.rm = TRUE), .groups="drop") %>%
  arrange(med_gap)

ggplot(gap_rank, aes(x = med_gap, y = reorder(sector, med_gap))) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 0, yend = sector)) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Sector Ranking: PD Term-Structure Gap (5Y – 1Y)",
       x = "Median Gap", y = "Sector") +
  theme_minimal()
#prova 5 
#############################################
# Zero-centered heatmap of PD gap (5Y – 1Y)
# Built directly from the parquet file
#############################################

# --- 1) Packages ------------------------------------------------
req <- c("arrow", "dplyr", "ggplot2", "lubridate", "stringr", "readr", "scales")
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# --- 2) Settings ------------------------------------------------
TEST_MODE  <- FALSE
OUTPUT_DIR <- file.path("02_team_modules", "B_Descriptive_Analysis")
if (!TEST_MODE) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

parquet_path <- "01_data_clean/output_features.parquet"
stopifnot(file.exists(parquet_path))

# --- 3) Read parquet & basic cleaning ---------------------------
raw <- arrow::read_parquet(parquet_path)
names(raw) <- tolower(names(raw))

date_col    <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^data_date$|date|time")])
country_col <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^country$|country_code|ctry")])
sector_col  <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^gdesc$|sector|industry|sic_desc")])

to_Date_safe <- function(x){
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1970-01-01"))
  if (is.character(x)) {
    y <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(y))) y <- suppressWarnings(as.Date(x))
    return(y)
  }
  as.Date(x)
}

df <- raw %>%
  dplyr::rename(
    date    = !!date_col,
    country = !!country_col
  ) %>%
  dplyr::mutate(
    date    = to_Date_safe(date),
    country = toupper(country),
    sector  = if (!is.null(sector_col)) as.character(.data[[sector_col]]) else "UNKNOWN"
  )

# --- 4) Build q_1y and q_5y if needed ---------------------------
have_q <- all(c("q_1y", "q_5y") %in% names(df))

if (!have_q) {
  message("q_* not found. Deriving from kdp_*.")
  
  pd_monthly <- function(p, months) 1 - (1 - p)^(months/12)
  
  for (nm in c("kdp_1yr", "kdp_5yr")) {
    if (nm %in% names(df))
      df[[nm]] <- readr::parse_number(df[[nm]])
  }
  
  df <- df %>%
    dplyr::mutate(
      q_1y = if ("kdp_1yr" %in% names(.)) pd_monthly(kdp_1yr, 12) else NA_real_,
      q_5y = if ("kdp_5yr" %in% names(.)) pd_monthly(kdp_5yr, 60) else NA_real_
    )
}

# Keep reasonable time window (optional, same as before)
df <- df %>%
  dplyr::filter(date >= as.Date("2015-01-01"),
                date <= as.Date("2024-12-31"))

# --- 5) Compute PD gap (5Y – 1Y) --------------------------------
df_gap <- df %>%
  dplyr::mutate(
    q_1y      = as.numeric(q_1y),
    q_5y      = as.numeric(q_5y),
    gap_5y_1y = q_5y - q_1y
  )

# --- 6) Build country × sector median gap matrix ----------------
gap_matrix <- df_gap %>%
  dplyr::filter(!is.na(country), !is.na(sector)) %>%
  dplyr::group_by(country, sector) %>%
  dplyr::summarise(
    gap_5y_1y = median(gap_5y_1y, na.rm = TRUE),
    .groups = "drop"
  )

# --- 7) Zero-centred colour scale -------------------------------
# Make limits symmetric around 0 so blue/red are comparable
max_abs_gap <- max(abs(gap_matrix$gap_5y_1y), na.rm = TRUE)

g_gap_heatmap_zero <- ggplot(
  gap_matrix,
  aes(x = country, y = sector, fill = gap_5y_1y)
) +
  geom_tile(color = NA) +
  scale_fill_gradient2(
    low = "#2c7bb6",      # blue = negative gap
    mid = "white",        # white = zero
    high = "#d7191c",     # red = positive gap
    midpoint = 0,
    limits = c(-max_abs_gap, max_abs_gap),
    labels = scales::percent_format(accuracy = 0.1),
    name = "Gap (5Y – 1Y)"
  ) +
  labs(
    title = "Sector–Country PD Term-Structure Gap (5Y – 1Y)\nZero-Centered Color Scale",
    x = "Country",
    y = "Sector"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

# --- 8) Save & print --------------------------------------------
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "fig_gap_5y_1y_heatmap_zero_centered.png"),
    g_gap_heatmap_zero,
    width = 12, height = 7, dpi = 150
  )
}

g_gap_heatmap_zero