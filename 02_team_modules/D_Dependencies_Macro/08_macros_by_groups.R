# ==========================================================
# 08_macro_by_groups.R  —  Panel FE per Cluster, Settore e Paese
# ==========================================================
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr)
  library(broom); library(ggplot2); library(forcats); library(stringr)
  library(plm); library(lmtest); library(sandwich)
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
    
    # crea oggetto panel (unità = iso o gdesc)
    pdata <- tryCatch(plm::pdata.frame(d, index = id_vars), error = function(e) NULL)
    if (is.null(pdata)) return(NULL)
    
    # stima modello FE (within) + robust SE
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
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
    labs(title = title, x = "Macro (z-score)", y = stringr::str_to_title(group_var), fill = "Coeff") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

if (nrow(coefs_cluster) > 0) {
  g1 <- plot_heatmap(coefs_cluster, "cluster", "Panel FE per Cluster — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_cluster_panelFE.png"), g1, width = 8, height = 5, dpi = 150)
}

if (nrow(coefs_sector) > 0) {
  top20 <- coefs_sector %>%
    distinct(gdesc, n) %>% arrange(desc(n)) %>% slice(1:20) %>% pull(gdesc)
  g2 <- plot_heatmap(coefs_sector %>% filter(gdesc %in% top20),
                     "gdesc", "Panel FE per Settore (Top 20) — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_sector_panelFE_top20.png"), g2, width = 10, height = 8, dpi = 150)
}

if (nrow(coefs_country) > 0) {
  g3 <- plot_heatmap(coefs_country, "iso", "Panel FE per Paese — Coefficienti standardizzati")
  ggsave(file.path(OUT, "heatmap_country_panelFE.png"), g3, width = 8, height = 6, dpi = 150)
}

message("✅ Analisi completata — Panel FE stimati per cluster, settore e paese.")