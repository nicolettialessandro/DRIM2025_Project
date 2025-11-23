# ==========================================================
# 08_macro_by_groups.R  —  Panel FE per Cluster, Settore e Paese
# ==========================================================
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr)
  library(broom); library(ggplot2); library(forcats); library(stringr)
  library(plm); library(lmtest); library(sandwich); library(readxl); library(purrr); library(lubridate)
})

OUT <- "02_team_modules/D_Dependencies_Macro"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# === 1) Carica panel ===
panel <- read_parquet(file.path(OUT, "panel_df.parquet"))
names(panel) <- tolower(names(panel))

# colonne base
base_cols <- c("iso","gdesc","cluster","date","pd_mean",
               "unemployment_rate","hicp_yoy","yield_10y",
               "credit_spread_bp","gdp","gpr")

missing <- setdiff(base_cols, names(panel))
if (length(missing)) stop("❌ Mancano colonne base nel panel: ", paste(missing, collapse=", "))

panel <- panel %>%
  mutate(
    iso = toupper(as.character(iso)),
    gdesc = as.character(gdesc),
    cluster = as.factor(cluster),
    date = as.Date(date)
  ) %>%
  filter(is.finite(pd_mean))

# === 1.5) Integra macro USA da Excel ===
usa_path <- "01_data_clean/macro"

pick_date_col <- function(df) {
  nms <- tolower(names(df))
  cand <- c("date","observation_date","time","data","month","period")
  hit  <- match(cand, nms); hit <- hit[!is.na(hit)]
  if (length(hit) == 0) stop("Nessuna colonna data trovata.")
  as.Date(df[[hit[1]]])
}

load_excel_series <- function(file, value_patterns, out_col) {
  df <- tryCatch(read_excel(file), error = function(e) NULL)
  if (is.null(df)) return(NULL)
  names(df) <- tolower(names(df))
  date_vec <- pick_date_col(df)
  vcol <- names(df)[stringr::str_detect(names(df), paste0("(", paste(value_patterns, collapse="|"), ")"))]
  stopifnot("Colonna valori non trovata" = length(vcol) >= 1)
  tibble::tibble(iso = "USA", date = date_vec, !!out_col := as.numeric(df[[vcol[1]]]))
}

# Serie macro USA
usa_unemp <- load_excel_series(file.path(usa_path,"UNRATE_USA.xlsx"),        c("unemp","unrate","rate"),      "unemployment_rate")
usa_yield <- load_excel_series(file.path(usa_path,"yield_USA.xlsx"),         c("yield","bond","10"),          "yield_10y")
usa_cs    <- load_excel_series(file.path(usa_path,"credit_spread_USA.xlsx"), c("credit","spread"),            "credit_spread_bp")
usa_gdp   <- load_excel_series(file.path(usa_path,"GDP_USA.xlsx"),           c("^gdp$","domestic","product"), "gdp")
usa_gpr   <- load_excel_series(file.path(usa_path,"gpr_USA.xlsx"),           c("^gpr$","geopolitical","risk"),"gpr")

# CPI (YoY o calcolata)
usa_cpi_raw <- tryCatch(read_excel(file.path(usa_path,"inflation_USA.xlsx")), error = function(e) NULL)
if (!is.null(usa_cpi_raw)) {
  names(usa_cpi_raw) <- tolower(names(usa_cpi_raw))
  dvec <- pick_date_col(usa_cpi_raw)
  yoy_col <- names(usa_cpi_raw)[stringr::str_detect(names(usa_cpi_raw), "yoy")]
  if (length(yoy_col) >= 1) {
    usa_cpi <- tibble::tibble(iso="USA", date=dvec, hicp_yoy = as.numeric(usa_cpi_raw[[yoy_col[1]]]))
  } else {
    cpi_col <- names(usa_cpi_raw)[stringr::str_detect(names(usa_cpi_raw), "^cpi$|consumer|price|index")]
    stopifnot("Colonna CPI non trovata" = length(cpi_col) >= 1)
    cpi <- as.numeric(usa_cpi_raw[[cpi_col[1]]])
    usa_cpi <- tibble::tibble(iso="USA", date=dvec, hicp_yoy = (cpi/lag(cpi,12) - 1)*100)
  }
} else usa_cpi <- NULL

usa_macro_df <- list(usa_unemp, usa_cpi, usa_yield, usa_cs, usa_gdp, usa_gpr) %>%
  compact() %>%
  reduce(full_join, by = c("iso","date")) %>%
  mutate(
    iso  = toupper(iso),
    date = floor_date(as.Date(date), "month")
  ) %>%
  group_by(iso, date) %>%
  summarise(
    across(.fns = ~ suppressWarnings(if (is.numeric(.x)) mean(.x, na.rm = TRUE) else first(.x))),
    .groups = "drop"
  )

message("✅ USA macro rows loaded: ", nrow(usa_macro_df))

panel <- panel %>%
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

# === 2) Aggiungi z-score ===
scale_safe <- function(x) {
  if (all(is.na(x))) return(x)
  if (sd(x, na.rm=TRUE) == 0) return(rep(0, length(x)))
  return(as.numeric(scale(x)))
}

panel <- panel %>%
  mutate(
    z_unemp  = scale_safe(unemployment_rate),
    z_infl   = scale_safe(hicp_yoy),
    z_yield  = scale_safe(yield_10y),
    z_spread = scale_safe(credit_spread_bp),
    z_gdp    = scale_safe(gdp),
    z_gpr    = scale_safe(gpr)
  )

message("✅ Z-score aggiunti. Righe totali: ", nrow(panel))

# === 3) Funzione Panel FE robusta per gruppo ===
run_group_panel <- function(df, group_var, id_vars = c("iso", "date"), min_n = 300) {
  grp_sym <- rlang::sym(group_var)
  groups <- df %>%
    count(!!grp_sym, name = "n") %>%
    filter(n >= min_n) %>%
    pull(!!grp_sym)
  
  message("📊 Stimo Panel FE (within) per ", group_var, " — gruppi validi: ", length(groups))
  if (length(groups) == 0) return(tibble())
  
  res <- lapply(groups, function(g) {
    d <- df %>% filter(!!grp_sym == g)
    if (nrow(d) < min_n) return(NULL)
    pdata <- tryCatch(plm::pdata.frame(d, index = id_vars), error = function(e) NULL)
    if (is.null(pdata)) return(NULL)
    m <- tryCatch({
      mod <- plm::plm(
        pd_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
        data = pdata, model = "within", effect = "individual"
      )
      coefs <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, type = "HC1", cluster = "group"))
      broom::tidy(coefs) %>% mutate(!!group_var := g, n = nrow(d))
    }, error = function(e) NULL)
    return(m)
  })
  
  out <- bind_rows(res)
  if (!"term" %in% names(out)) return(tibble())
  out %>%
    filter(term != "(Intercept)") %>%
    relocate(!!grp_sym, n, .before = term)
}

# === 4) Panel FE per Cluster ===
coefs_cluster <- run_group_panel(panel, "cluster", id_vars = c("iso", "date"), min_n = 100)
if (nrow(coefs_cluster) > 0) {
  write_csv(coefs_cluster, file.path(OUT, "by_cluster_panelFE_coefs.csv"))
}

# === 5) Panel FE per Settore ===
coefs_sector <- run_group_panel(panel, "gdesc", id_vars = c("iso", "date"), min_n = 800)
if (nrow(coefs_sector) > 0) {
  write_csv(coefs_sector, file.path(OUT, "by_sector_panelFE_coefs.csv"))
}

# === 6) Panel FE per Paese ===
coefs_country <- run_group_panel(panel, "iso", id_vars = c("gdesc", "date"), min_n = 300)
if (nrow(coefs_country) > 0) {
  write_csv(coefs_country, file.path(OUT, "by_country_panelFE_coefs.csv"))
}

# === 7) Heatmap ===
plot_heatmap <- function(tbl, group_var, title) {
  if (nrow(tbl) == 0) return(NULL)
  ggplot(
    tbl %>%
      mutate(term = factor(term,
                           levels = c("z_spread","z_yield","z_unemp","z_infl","z_gdp","z_gpr"))) %>%
      mutate(sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      )),
    aes(x = term, y = fct_reorder(.data[[group_var]], estimate, .fun = median, .desc = TRUE),
        fill = estimate)
  ) +
    geom_tile() +
    geom_text(aes(label = sig), color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient(low = "lightgrey", high = "darkgreen") +
    labs(title = title, x = "Macro (z-score)", y = stringr::str_to_title(group_var), fill = "Coeff") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

if (nrow(coefs_cluster) > 0) {
  g1 <- plot_heatmap(coefs_cluster, "cluster", "Cluster Panel FE")
  ggsave(file.path(OUT, "heatmap_cluster_panelFE.png"), g1, width = 8, height = 5, dpi = 150)
}

if (nrow(coefs_sector) > 0) {
  g2 <- plot_heatmap(
    coefs_sector,
    "gdesc",
    "Sector Panel FE"
  )
  ggsave(file.path(OUT, "heatmap_sector_panelFE.png"), g2, width = 10, height = 8, dpi = 150)
}

if (nrow(coefs_country) > 0) {
  g3 <- plot_heatmap(coefs_country, "iso", "Country Panel FE")
  ggsave(file.path(OUT, "heatmap_country_panelFE.png"), g3, width = 8, height = 6, dpi = 150)
}

message("✅ Analisi completata — Panel FE stimati per cluster, settore e paese.")
