# ============================================================
# PACKAGES
# ============================================================
pkgs <- c("dplyr","tidyr","lubridate","stringr","purrr","ggplot2",
          "eurostat","quantmod","zoo","scales","readr","arrow","TTR","rlang")
new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))
options(eurostat_cache = TRUE)

try_silent <- function(expr) try(expr, silent = TRUE)

# ============================================================
# YOUR COUNTRIES + USA
# ============================================================
country_list <- c("AUT","BEL","CYP","DEU","DNK","ESP","EST","FIN","FRA","GBR",
                  "GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL","PRT",
                  "SVK","SVN","SWE","CHE","USA")

# ============================================================
# READ PD PARQUET + DATE NORMALIZATION
# ============================================================
data_path <- "C:/Users/Gaia/Documents/magistrale/AMUniversite/credit risk/DRIM2025_Project0/01_data_clean/output_features.parquet"
pd_raw <- arrow::read_parquet(data_path)

# detect date col
date_col <- dplyr::case_when(
  "data_date"  %in% names(pd_raw) ~ "data_date",
  "month_year" %in% names(pd_raw) ~ "month_year",
  TRUE ~ NA_character_
)
if (is.na(date_col)) stop("No date column found. Expect `data_date` or `month_year`.")

pd <- pd_raw %>%
  mutate(date_raw = .data[[date_col]]) %>%
  mutate(
    date = dplyr::case_when(
      inherits(date_raw, "Date")                ~ as.Date(date_raw),
      is.character(date_raw) & nchar(date_raw)==7 ~ lubridate::ym(date_raw),
      TRUE                                       ~ suppressWarnings(as.Date(date_raw))
    )
  ) %>%
  mutate(date = floor_date(date, "month")) %>%
  filter(country %in% country_list)

# choose PD horizon (your lowercase names)
pd_names_l <- tolower(names(pd))
pref <- c("kdp_1yr","kdp_1mo","kdp_3mo","kdp_6mo","kdp_2yr","kdp_3yr","kdp_5yr","kdp_10yr")
cand <- intersect(pref, pd_names_l)
if (length(cand)==0) stop("No KDP_* columns found in your dataset.")
PD_COL <- names(pd)[match(cand[1], pd_names_l)]
message("Using PD column: ", PD_COL)

# aggregate to country–month (median across firms)
pd_country_agg <- pd %>%
  filter(!is.na(country), !is.na(date)) %>%
  group_by(country, date) %>%
  summarize(PD = median(.data[[PD_COL]], na.rm = TRUE),
            n_companies = n(), .groups = "drop")

keep_countries <- intersect(country_list, unique(pd_country_agg$country))

# ============================================================
# ISO3 -> Eurostat code mapping
# ============================================================
iso3_to_eu2 <- tibble::tribble(
  ~iso3, ~eurostat_geo,
  "AUT","AT","BEL","BE","CYP","CY","DEU","DE","DNK","DK","ESP","ES","EST","EE","FIN","FI",
  "FRA","FR","GRC","EL","HRV","HR","IRL","IE","ITA","IT","LUX","LU","MLT","MT","NLD","NL",
  "POL","PL","PRT","PT","SVK","SK","SVN","SI","SWE","SE","GBR","UK","NOR","NO","CHE","CH"
)
iso_map <- iso3_to_eu2 %>% filter(iso3 %in% keep_countries)
eu_geo  <- iso_map$eurostat_geo

# ============================================================
# EUROSTAT HELPERS (normalize schema differences)
# ============================================================
normalize_eurostat <- function(df) {
  if (!is.data.frame(df) || ncol(df)==0) return(df)
  cn <- names(df)
  geo_col <- intersect(c("geo","GEO","geo_code"), cn)[1]
  time_col <- intersect(c("time","TIME_PERIOD"), cn)[1]
  val_col <- intersect(c("values","OBS_VALUE"), cn)[1]
  if (is.na(geo_col) || is.na(time_col) || is.na(val_col)) return(df)
  out <- df %>%
    dplyr::rename(
      geo   = !!sym(geo_col),
      time  = !!sym(time_col),
      values= !!sym(val_col)
    )
  if (!inherits(out$time,"Date")) {
    suppressWarnings({
      out$time <- dplyr::coalesce(
        tryCatch(lubridate::ym(out$time), error=function(e) as.Date(NA)),
        tryCatch(as.Date(out$time), error=function(e) as.Date(NA))
      )
    })
  }
  out
}

get_eu_unemp <- function(geo_vec){
  df <- eurostat::get_eurostat("une_rt_m", time_format="date", cache = TRUE)
  df <- normalize_eurostat(df)
  out <- df %>%
    dplyr::filter(geo %in% geo_vec, sex=="T", age=="Y15-74", s_adj=="SA") %>%
    dplyr::transmute(eurostat_geo = geo, date = time, Unemployment = values)
  message("EU unemployment rows: ", nrow(out)); out
}
get_eu_cpi_yoy <- function(geo_vec){
  df <- eurostat::get_eurostat("prc_hicp_manr", time_format="date", cache = TRUE)
  df <- normalize_eurostat(df)
  out <- df %>%
    dplyr::filter(geo %in% geo_vec, coicop=="CP00") %>%
    dplyr::transmute(eurostat_geo = geo, date = time, CPI_YoY = values)
  message("EU CPI YoY rows: ", nrow(out)); out
}
get_eu_10y <- function(geo_vec){
  df <- eurostat::get_eurostat("irt_lt_mcby_m", time_format="date", cache = TRUE)
  df <- normalize_eurostat(df)
  out <- df %>%
    dplyr::filter(geo %in% geo_vec, unit=="PC_PA") %>%
    dplyr::transmute(eurostat_geo = geo, date = time, Y10_Gov = values)
  message("EU 10Y yield rows: ", nrow(out)); out
}

eu_unemp <- try_silent(get_eu_unemp(eu_geo)); if(inherits(eu_unemp,"try-error")) eu_unemp <- tibble()
eu_cpi   <- try_silent(get_eu_cpi_yoy(eu_geo)); if(inherits(eu_cpi,"try-error")) eu_cpi <- tibble()
eu_y10   <- try_silent(get_eu_10y(eu_geo));   if(inherits(eu_y10,"try-error")) eu_y10 <- tibble()

# Safe join: only join tables that actually have keys
eu_list <- list(eu_unemp, eu_cpi, eu_y10)
eu_list <- purrr::keep(eu_list, ~ is.data.frame(.x) && ncol(.x)>0 && all(c("eurostat_geo","date") %in% names(.x)))
eu_macro <- if (length(eu_list)==0) {
  warning("No usable Eurostat tables. Proceeding without EU macro.")
  tibble(country=character(), date=as.Date(character()))
} else {
  purrr::reduce(eu_list, dplyr::full_join, by=c("eurostat_geo","date")) %>%
    dplyr::left_join(iso_map, by="eurostat_geo") %>%
    dplyr::rename(country=iso3)
}

# ============================================================
# USA MACROS (FRED)
# ============================================================
quantmod::getSymbols(c("UNRATE","CPIAUCSL","DGS10","GDPC1"), src="FRED", auto.assign=TRUE)

usa_unemp <- UNRATE %>% fortify.zoo() %>% as_tibble() %>% rename(date=Index, Unemployment=UNRATE)
usa_cpi_yoy <- TTR::ROC(CPIAUCSL, n = 12, type = "discrete") * 100
usa_cpi_yoy <- fortify.zoo(usa_cpi_yoy) %>%
  as_tibble() %>%
  rename(date = Index) %>%
  rename_with(~"CPI_YoY", -date) %>%
  mutate(CPI_YoY = as.numeric(CPI_YoY))
usa_y10 <- DGS10 %>% fortify.zoo() %>% as_tibble() %>% rename(date=Index, Y10_Gov=DGS10)
usa_gdp_q <- GDPC1 %>% fortify.zoo() %>% as_tibble() %>% rename(date=Index, GDP=GDPC1) %>%
  arrange(date) %>% mutate(GDP_YoY = 100*(GDP/lag(GDP,4)-1)) %>% select(date, GDP_YoY)
usa_gdp_m <- usa_gdp_q %>%
  mutate(date = floor_date(date, "quarter")) %>%
  complete(date = seq(min(date), max(date), by="month")) %>%
  arrange(date) %>% mutate(GDP_YoY = zoo::na.locf(GDP_YoY, na.rm = FALSE))
usa_macro <- usa_unemp %>%
  full_join(usa_cpi_yoy, by="date") %>%
  full_join(usa_y10, by="date") %>%
  full_join(usa_gdp_m, by="date") %>%
  mutate(country="USA")

# ============================================================
# CREDIT SPREADS
# ============================================================
quantmod::getSymbols(c("BAMLC0A4CBBB","BAMLC0A1CAAA"), src="FRED", auto.assign=TRUE)
usa_cs <- BAMLC0A4CBBB %>% fortify.zoo() %>% as_tibble() %>% rename(date=Index, BBB_OAS=BAMLC0A4CBBB) %>%
  left_join(BAMLC0A1CAAA %>% fortify.zoo() %>% as_tibble() %>% rename(date=Index, AAA_OAS=BAMLC0A1CAAA), by="date") %>%
  mutate(Credit_Spread = BBB_OAS - AAA_OAS, country="USA") %>%
  select(date, country, Credit_Spread)

eu_cs <- NULL
if(is.data.frame(eu_y10) && nrow(eu_y10)>0 && "Y10_Gov" %in% names(eu_y10)){
  de_curve <- eu_y10 %>% filter(eurostat_geo=="DE") %>% select(date, Bund10=Y10_Gov)
  if(nrow(de_curve)>0){
    eu_cs <- eu_y10 %>%
      left_join(de_curve, by="date") %>%
      left_join(iso_map, by="eurostat_geo") %>%
      transmute(country=iso3, date, Credit_Spread = Y10_Gov - Bund10)
  }
}

# ============================================================
# EU GDP: quarterly -> YoY -> monthly carry
# ============================================================
eu_gdp_q <- try_silent({
  eurostat::get_eurostat("namq_10_gdp", time_format="date") %>%
    normalize_eurostat() %>%
    filter(geo %in% eu_geo) %>%
    select(geo, time, values) %>%
    rename(eurostat_geo=geo, date=time, GDP=values) %>%
    group_by(eurostat_geo) %>% arrange(date, .by_group=TRUE) %>%
    mutate(GDP_YoY = 100*(GDP/lag(GDP,4)-1)) %>% ungroup()
})
if(inherits(eu_gdp_q,"try-error")) eu_gdp_q <- tibble()
eu_gdp_m <- NULL
if(nrow(eu_gdp_q)>0){
  eu_gdp_m <- eu_gdp_q %>%
    select(eurostat_geo, date, GDP_YoY) %>%
    group_by(eurostat_geo) %>%
    complete(date = seq(min(date), max(date), by="month")) %>%
    arrange(eurostat_geo, date) %>%
    mutate(GDP_YoY = zoo::na.locf(GDP_YoY, na.rm = FALSE)) %>%
    ungroup() %>% left_join(iso_map, by="eurostat_geo") %>%
    rename(country=iso3)
}

# ============================================================
# GPR (robust fetch) + merge
# ============================================================
get_gpr_safe <- function() {
  urls <- c(
    "https://www.policyuncertainty.com/gpr/GPRDataMonthly.csv",
    "http://www.policyuncertainty.com/gpr/GPRDataMonthly.csv"
  )
  for (u in urls) {
    out <- try_silent(readr::read_csv(u, show_col_types = FALSE))
    if (is.data.frame(out) && nrow(out)>0) return(out)
    tf <- tempfile(fileext=".csv")
    ok <- try_silent(utils::download.file(u, tf, mode="wb", quiet=TRUE))
    if (!inherits(ok,"try-error")) {
      df <- try_silent(readr::read_csv(tf, show_col_types = FALSE))
      if (is.data.frame(df) && nrow(df)>0) return(df)
    }
  }
  NULL
}
gpr_raw <- get_gpr_safe()
if (!is.null(gpr_raw)) {
  gpr_tidy <- gpr_raw %>%
    mutate(date = lubridate::ymd(paste(Year, Month, "01"))) %>%
    select(date, GPR_Global = GPR) %>%
    arrange(date)
  message("GPR rows fetched: ", nrow(gpr_tidy))
} else {
  gpr_tidy <- NULL
  warning("Could not fetch GPR; proceeding without it.")
}

# ============================================================
# ASSEMBLE MACRO PANEL (robust: tolerate missing EU series)
# ============================================================
eu_macro_all <- NULL
if (exists("eu_macro") && is.data.frame(eu_macro) && nrow(eu_macro) > 0) {
  
  # ensure expected columns exist; if a series was missing, create NA column
  needed_cols <- c("Unemployment","CPI_YoY","Y10_Gov")
  for (nm in needed_cols) {
    if (!nm %in% names(eu_macro)) eu_macro[[nm]] <- NA_real_
  }
  
  eu_macro_all <- eu_macro %>%
    dplyr::select(country, date, dplyr::any_of(needed_cols))
  
  # EU GDP YoY (monthly carried)
  if (exists("eu_gdp_m") && is.data.frame(eu_gdp_m) && nrow(eu_gdp_m) > 0) {
    eu_macro_all <- eu_macro_all %>%
      dplyr::full_join(eu_gdp_m %>% dplyr::select(country, date, GDP_YoY),
                       by = c("country","date"))
  }
  
  # EU sovereign credit spread vs Bund
  if (exists("eu_cs") && is.data.frame(eu_cs) && nrow(eu_cs) > 0) {
    eu_macro_all <- eu_macro_all %>%
      dplyr::full_join(eu_cs, by = c("country","date"))
  }
}

# USA block unchanged
usa_macro_all <- usa_macro %>%
  dplyr::full_join(usa_cs, by = c("country","date")) %>%
  dplyr::select(country, date,
                dplyr::any_of(c("Unemployment","CPI_YoY","Y10_Gov","GDP_YoY","Credit_Spread")))

# Bind and merge GPR (if available)
macro_all <- dplyr::bind_rows(eu_macro_all, usa_macro_all) %>%
  dplyr::filter(country %in% keep_countries) %>%
  dplyr::mutate(date = lubridate::floor_date(date, "month"))

if (exists("gpr_tidy") && is.data.frame(gpr_tidy) && nrow(gpr_tidy) > 0) {
  macro_all <- macro_all %>% dplyr::left_join(gpr_tidy, by = "date")
} else if (!"GPR_Global" %in% names(macro_all)) {
  macro_all <- macro_all %>% dplyr::mutate(GPR_Global = NA_real_)
}

# Merge with PD
df <- pd_country_agg %>%
  dplyr::inner_join(macro_all, by = c("country","date"))


# ============================================================
# MERGE WITH PD
# ============================================================
df <- pd_country_agg %>% inner_join(macro_all, by=c("country","date"))
message("Merged rows: ", nrow(df),
        " | Countries: ", length(unique(df$country)),
        " | Date range: ", as.character(min(df$date, na.rm=TRUE)), " -> ", as.character(max(df$date, na.rm=TRUE)))

# ============================================================
# PLOTTING: CLEAR OVERLAYS + CHANGES + SCATTER + ROLLING CORR
# ============================================================

.zscore <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

.require_var <- function(d, macro_var) {
  if (!macro_var %in% names(d)) {
    message("Skipping: column '", macro_var, "' not found.")
    return(FALSE)
  }
  TRUE
}

# 1) Overlay (levels, standardized)
plot_overlay <- function(d, ctry, macro_var, z_window = NULL) {
  if (!.require_var(d, macro_var)) return(NULL)
  dd <- d %>%
    dplyr::filter(country == ctry) %>%
    dplyr::select(date, PD, macro = dplyr::all_of(macro_var)) %>%
    dplyr::arrange(date)
  
  if (nrow(dd) == 0) return(NULL)
  if (all(is.na(dd$macro))) { message("All NA in ", ctry, " / ", macro_var); return(NULL) }
  
  if (is.null(z_window)) {
    dd <- dd %>% dplyr::mutate(PD_z = .zscore(PD), Macro_z = .zscore(macro))
  } else {
    dd <- dd %>%
      dplyr::mutate(
        PD_z    = zoo::rollapply(PD,    z_window, function(x) .zscore(x)[length(x)], align="right", fill=NA),
        Macro_z = zoo::rollapply(macro, z_window, function(x) .zscore(x)[length(x)], align="right", fill=NA)
      )
  }
  
  if (all(is.na(dd$Macro_z))) { message("Macro z-score NA in ", ctry, " / ", macro_var); return(NULL) }
  
  color_map <- c("PD (z)" = "#1f77b4")
  color_map[paste0(macro_var, " (z)")] <- "#d62728"
  
  ggplot(dd, aes(x = date)) +
    geom_line(aes(y = PD_z, color = "PD (z)"), linewidth = 1) +
    geom_line(aes(y = Macro_z, color = paste0(macro_var, " (z)")), linewidth = 1, linetype = 2) +
    scale_color_manual(values = color_map) +
    labs(title = paste0(ctry, " — PD vs ", macro_var, " (standardized)"),
         y = "z-score", x = NULL, color = NULL) +
    theme_minimal()
}

# 2) Change-on-change overlay (Δ month-over-month, standardized)
plot_overlay_changes <- function(d, ctry, macro_var) {
  if (!.require_var(d, macro_var)) return(NULL)
  dd <- d %>%
    dplyr::filter(country == ctry) %>%
    dplyr::arrange(date) %>%
    dplyr::transmute(
      date,
      dPD    = diff(c(NA_real_, as.numeric(PD))),
      dMacro = diff(c(NA_real_, as.numeric(.data[[macro_var]])))
    )
  
  if (nrow(dd) == 0) return(NULL)
  if (all(is.na(dd$dMacro))) { message("All Δmacro NA in ", ctry, " / ", macro_var); return(NULL) }
  
  dd <- dd %>% dplyr::mutate(dPD_z = .zscore(dPD), dMacro_z = .zscore(dMacro))
  if (all(is.na(dd$dMacro_z))) { message("Δmacro z-score NA in ", ctry, " / ", macro_var); return(NULL) }
  
  color_map <- c("ΔPD (z)" = "#1f77b4")
  color_map[paste0("Δ", macro_var, " (z)")] <- "#d62728"
  
  ggplot(dd, aes(x = date)) +
    geom_line(aes(y = dPD_z, color = "ΔPD (z)"), linewidth = 1) +
    geom_line(aes(y = dMacro_z, color = paste0("Δ", macro_var, " (z)")), linewidth = 1, linetype = 2) +
    scale_color_manual(values = color_map) +
    labs(title = paste0(ctry, " — ΔPD vs Δ", macro_var, " (standardized)"),
         y = "z-score", x = NULL, color = NULL) +
    theme_minimal()
}

# 3) Scatter with OLS (levels)
plot_scatter <- function(d, ctry, macro_var) {
  if (!.require_var(d, macro_var)) return(NULL)
  dd <- d %>%
    dplyr::filter(country == ctry) %>%
    dplyr::select(PD = PD, X = dplyr::all_of(macro_var)) %>%
    tidyr::drop_na()
  if (nrow(dd) < 6) { message("Few points: ", ctry, " / ", macro_var); return(NULL) }
  ggplot(dd, aes(x = X, y = PD)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = paste0(ctry, " — PD vs ", macro_var), x = macro_var, y = "PD") +
    theme_minimal()
}

# 4) Rolling correlation cor(ΔPD, Δmacro) with NA-safe window
plot_rolling_corr <- function(d, ctry, macro_var, k = 12) {
  if (!.require_var(d, macro_var)) return(NULL)
  dd <- d %>%
    dplyr::filter(country == ctry) %>%
    dplyr::arrange(date)
  
  if (nrow(dd) < k + 2) { message("Too few rows: ", ctry, " / ", macro_var); return(NULL) }
  
  dPD <- diff(as.numeric(dd$PD))
  dMX <- diff(as.numeric(dd[[macro_var]]))
  if (sum(complete.cases(dPD, dMX)) < 2) { message("No complete pairs: ", ctry, " / ", macro_var); return(NULL) }
  
  rc <- zoo::rollapply(
    cbind(dPD, dMX), k,
    function(x) {
      ok <- stats::complete.cases(x)
      if (sum(ok) < 2) return(NA_real_)
      stats::cor(x[ok, 1], x[ok, 2], use = "complete.obs")
    },
    by.column = FALSE, fill = NA
  )
  dt <- dd$date[-1]
  
  ggplot(tibble::tibble(date = dt, r = as.numeric(rc)), aes(date, r)) +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_line() +
    labs(title = paste0(ctry, " — Rolling corr(ΔPD, Δ", macro_var, ") ", k, "m"),
         y = "Correlation", x = NULL) +
    theme_minimal()
}

# ----------------------------
# QUICK EXAMPLES (adjust freely)
# ----------------------------
# Available macro vars in df typically include:
# "Unemployment","CPI_YoY","Y10_Gov","GDP_YoY","Credit_Spread","GPR_Global"

print(plot_overlay(df, "USA", "Unemployment"))
print(plot_overlay_changes(df, "USA", "Unemployment"))
print(plot_scatter(df, "USA", "Unemployment"))
print(plot_rolling_corr(df, "USA", "Unemployment", k = 12))

print(plot_overlay(df, "ITA", "CPI_YoY"))
print(plot_overlay(df, "AUT", "Y10_Gov"))
print(plot_overlay(df, "FRA", "Credit_Spread"))
print(plot_overlay(df, "GBR", "GPR_Global"))   # will work if GPR was fetched/merged
