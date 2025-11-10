# ==========================================================
# 08_macro_by_groups.R  —  OLS per Cluster, Settore e Paese
# ==========================================================
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr)
  library(broom); library(ggplot2); library(forcats); library(stringr)
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

# === 2) Aggiungi z-score ===
scale_safe <- function(x) {
  if (all(is.na(x))) return(x)
  if (sd(x, na.rm=TRUE) == 0) return(rep(0, length(x)))
  return(as.numeric(scale(x)))
}

panel <- panel %>%
  mutate(
    z_unemp = scale_safe(unemployment_rate),
    z_infl = scale_safe(hicp_yoy),
    z_yield = scale_safe(yield_10y),
    z_spread = scale_safe(credit_spread_bp),
    z_gdp = scale_safe(gdp),
    z_gpr = scale_safe(gpr)
  )

message("✅ Z-score aggiunti. Righe totali: ", nrow(panel))

# === 3) Funzione OLS robusta ===
run_group_ols <- function(df, group_var, min_n = 300) {
  grp_sym <- rlang::sym(group_var)
  groups <- df %>%
    count(!!grp_sym, name = "n") %>%
    filter(n >= min_n) %>%
    pull(!!grp_sym)
  
  message("📊 Stimo OLS per ", group_var, " — gruppi validi: ", length(groups))
  if (length(groups) == 0) return(tibble())
  
  res <- lapply(groups, function(g) {
    d <- df %>% filter(!!grp_sym == g)
    if (nrow(d) < min_n) return(NULL)
    m <- tryCatch(
      lm(pd_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr, data = d),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)
    broom::tidy(m) %>%
      mutate(!!group_var := g, n = nrow(d))
  })
  
  out <- bind_rows(res)
  if (!"term" %in% names(out)) return(tibble())
  out %>%
    filter(term != "(Intercept)") %>%
    relocate(!!grp_sym, n, .before = term)
}

# === 4) OLS per cluster ===
coefs_cluster <- run_group_ols(panel, "cluster", min_n = 500)
if (nrow(coefs_cluster) > 0) {
  write_csv(coefs_cluster, file.path(OUT, "by_cluster_ols_coefs.csv"))
}

# === 5) OLS per settore ===
coefs_sector <- run_group_ols(panel, "gdesc", min_n = 1000)
if (nrow(coefs_sector) > 0) {
  write_csv(coefs_sector, file.path(OUT, "by_sector_ols_coefs.csv"))
}

# === 6) OLS per paese ===
coefs_country <- run_group_ols(panel, "iso", min_n = 500)   # abbasso soglia
if (nrow(coefs_country) > 0) {
  write_csv(coefs_country, file.path(OUT, "by_country_ols_coefs.csv"))
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
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
    labs(title = title, x = "Macro (z-score)", y = stringr::str_to_title(group_var), fill = "Coeff") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

if (nrow(coefs_cluster) > 0) {
  g1 <- plot_heatmap(coefs_cluster, "cluster", "OLS per Cluster — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_cluster_ols.png"), g1, width = 8, height = 5, dpi = 150)
}

if (nrow(coefs_sector) > 0) {
  top20 <- coefs_sector %>%
    distinct(gdesc, n) %>% arrange(desc(n)) %>% slice(1:20) %>% pull(gdesc)
  g2 <- plot_heatmap(coefs_sector %>% filter(gdesc %in% top20),
                     "gdesc", "OLS per Settore (Top 20) — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_sector_ols_top20.png"), g2, width = 10, height = 8, dpi = 150)
}

if (nrow(coefs_country) > 0) {
  g3 <- plot_heatmap(coefs_country, "iso", "OLS per Paese — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_country_ols.png"), g3, width = 8, height = 6, dpi = 150)
}

message("✅ Analisi completata — file salvati in: ", OUT)
