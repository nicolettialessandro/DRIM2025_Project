# ==========================================================
# 07_macro_models.R — Two-Way Fixed Effects (Panel Model)
# ==========================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(broom)
  library(plm)
  library(readr)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
})

# === 1️⃣ Load panel ===
panel_df <- read_parquet("02_team_modules/D_Dependencies_Macro/panel_df.parquet")

# === 2️⃣ Clean ===
panel_df <- panel_df %>%
  mutate(
    iso = toupper(as.character(iso)),
    Cluster = as.factor(Cluster),
    date = as.Date(date),
    PD_mean = as.numeric(PD_mean),
    unemployment_rate = as.numeric(unemployment_rate),
    hicp_yoy = as.numeric(hicp_yoy),
    yield_10y = as.numeric(yield_10y),
    credit_spread_bp = as.numeric(credit_spread_bp),
    gdp = as.numeric(gdp),
    gpr = as.numeric(gpr)
  ) %>%
  filter(!is.na(PD_mean), !is.na(iso), !is.na(date))

# === 3️⃣ Z-score delle variabili macro ===
scale_safe <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(NA, length(x)))
  } else {
    return(as.numeric(scale(x)))
  }
}

panel_df <- panel_df %>%
  mutate(
    z_unemp  = scale_safe(unemployment_rate),
    z_infl   = scale_safe(hicp_yoy),
    z_yield  = scale_safe(yield_10y),
    z_spread = scale_safe(credit_spread_bp),
    z_gdp    = scale_safe(gdp),
    z_gpr    = scale_safe(gpr)
  )

# === 4️⃣ Crea panel data frame (plm) ===
panel_data <- pdata.frame(panel_df, index = c("iso", "date"))

# === 5️⃣ Two-Way Fixed Effects (country + time) ===
fe_tw <- plm(
  PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
  data = panel_data,
  model = "within",
  effect = "twoways"
)

panel_df %>%
  group_by(iso) %>%
  summarise(sd_spread = sd(credit_spread_bp, na.rm=TRUE)) %>%
  arrange(sd_spread)

# === 6️⃣ Coefficienti robusti (clustered by country) ===
coefs_tw <- coeftest(fe_tw, vcov = vcovHC(fe_tw, type = "HC1", cluster = "group"))

# === 7️⃣ Salvataggio risultati ===
tidy_fe <- tidy(coefs_tw)
write_csv(tidy_fe, "02_team_modules/D_Dependencies_Macro/panel_FE_twoways_coefs.csv")

fit_fe <- glance(fe_tw)
write_csv(fit_fe, "02_team_modules/D_Dependencies_Macro/panel_FE_twoways_fit.csv")

message("✅ Two-Way Fixed Effects model completed")

# === 8️⃣ Plot dei coefficienti ===
ggplot(tidy_fe %>% filter(term != "(Intercept)"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Macro Drivers of Default Probability (Two-Way FE)",
       x = NULL, y = "Coefficient (Robust SE)")

ggsave("02_team_modules/D_Dependencies_Macro/fig_FE_twoways_betas.png",
       width = 7, height = 5, dpi = 150)

message("🎉 Finished successfully — Two-Way FE results saved.")