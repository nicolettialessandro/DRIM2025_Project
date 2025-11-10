# ==========================================================
# 07_macro_models.R — Simple Macro–PD Regression Models
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
})

# === Load panel ===
panel_df <- read_parquet("02_team_modules/D_Dependencies_Macro/panel_df.parquet")

# === Clean ===
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

# === Create z-scores ===
scale_safe <- function(x) if (sd(x, na.rm=TRUE)==0) x*NA else as.numeric(scale(x))
panel_df <- panel_df %>%
  mutate(
    z_unemp = scale_safe(unemployment_rate),
    z_infl = scale_safe(hicp_yoy),
    z_yield = scale_safe(yield_10y),
    z_spread = scale_safe(credit_spread_bp),
    z_gdp = scale_safe(gdp),
    z_gpr = scale_safe(gpr)
  )

# === OLS ===
ols <- lm(PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr, data = panel_df)
write_csv(tidy(ols), "02_team_modules/D_Dependencies_Macro/ols_coefs.csv")
write_csv(glance(ols), "02_team_modules/D_Dependencies_Macro/ols_fit.csv")
message("✅ OLS done")

# === Panel FE per paese (aggregato) ===
agg_country <- panel_df %>%
  group_by(iso, date) %>%
  summarise(across(c(PD_mean, starts_with("z_")), ~ mean(.x, na.rm=TRUE)), .groups="drop")

panel_data <- pdata.frame(agg_country, index = c("iso", "date"))
fe_country <- plm(PD_mean ~ z_unemp + z_infl + z_yield + z_spread + z_gdp + z_gpr,
                  data = panel_data, model = "within", effect = "individual")
write_csv(tidy(fe_country), "02_team_modules/D_Dependencies_Macro/panel_FE_country.csv")
message("✅ Panel FE (country) done")

# === Plot ===
ols_tidy <- tidy(ols)
ggplot(ols_tidy %>% filter(term != "(Intercept)"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Macro Drivers of PD", x = NULL, y = "Coefficient") +
  theme_minimal()
ggsave("02_team_modules/D_Dependencies_Macro/fig_ols_betas.png", width = 7, height = 5, dpi = 150)

message("🎉 Finished successfully")
