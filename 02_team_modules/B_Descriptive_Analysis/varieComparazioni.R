# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify financial sector rows ----------------------------
df_fin <- df0 |>
  dplyr::mutate(
    is_financial = stringr::str_detect(
      sector,
      regex("FINANC|BANK|INSUR", ignore_case = TRUE)
    )
  ) |>
  dplyr::filter(is_financial)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_fin_reg <- df_fin |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::if_else(country == "USA",
                            "USA financial sector",
                            "European financial sector (+CHE)"),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_fin_1y <- df_fin_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) -------------------
g_pd_fin_1y <- ggplot(pd_fin_1y,
                      aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Financial Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_fin_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_financial_USA_vs_EuropeCHE_1Y.png"),
    g_pd_fin_1y,
    width = 10, height = 6, dpi = 150
  )
}

#information technology
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Information Technology sector rows --------------
df_it <- df0 |>
  dplyr::mutate(
    is_it = stringr::str_detect(
      sector,
      regex("INFORMATION|TECH|IT", ignore_case = TRUE)  # Matching IT sector
    )
  ) |>
  dplyr::filter(is_it)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_it_reg <- df_it |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::if_else(country == "USA",
                            "USA Information Technology",
                            "European Information Technology (+CHE)"),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_it_1y <- df_it_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for IT sector ------
g_pd_it_1y <- ggplot(pd_it_1y,
                     aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Information Technology Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_it_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_it_USA_vs_EuropeCHE_1Y.png"),
    g_pd_it_1y,
    width = 10, height = 6, dpi = 150
  )
}

#healthcare
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Healthcare sector rows ---------------------------
df_healthcare <- df0 |>
  dplyr::mutate(
    is_healthcare = stringr::str_detect(
      sector,
      regex("HEALTHCARE|MEDICAL|PHARMA|HEALTH CARE", ignore_case = TRUE)  # Matching Healthcare sector
    )
  ) |>
  dplyr::filter(is_healthcare)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_healthcare_reg <- df_healthcare |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Healthcare",
      country %in% eu_countries ~ "European Healthcare (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_healthcare_1y <- df_healthcare_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Healthcare ------
g_pd_healthcare_1y <- ggplot(pd_healthcare_1y,
                             aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Healthcare Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_healthcare_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_healthcare_USA_vs_EuropeCHE_1Y.png"),
    g_pd_healthcare_1y,
    width = 10, height = 6, dpi = 150
  )
}
#industrials
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Industrials sector rows ---------------------------
df_industrials <- df0 |>
  dplyr::mutate(
    is_industrials = stringr::str_detect(
      sector,
      regex("INDUSTRIAL|MANUFACTURING|ENGINEERING", ignore_case = TRUE)  # Matching Industrials sector
    )
  ) |>
  dplyr::filter(is_industrials)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_industrials_reg <- df_industrials |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Industrials",
      country %in% eu_countries ~ "European Industrials (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_industrials_1y <- df_industrials_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Industrials -----
g_pd_industrials_1y <- ggplot(pd_industrials_1y,
                              aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Industrials Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_industrials_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_industrials_USA_vs_EuropeCHE_1Y.png"),
    g_pd_industrials_1y,
    width = 10, height = 6, dpi = 150
  )
}
#energy 
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Energy sector rows ------------------------------
df_energy <- df0 |>
  dplyr::mutate(
    is_energy = stringr::str_detect(
      sector,
      regex("ENERGY|OIL|GAS|UTILITIES", ignore_case = TRUE)  # Matching Energy sector
    )
  ) |>
  dplyr::filter(is_energy)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_energy_reg <- df_energy |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Energy",
      country %in% eu_countries ~ "European Energy (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_energy_1y <- df_energy_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Energy ---------
g_pd_energy_1y <- ggplot(pd_energy_1y,
                         aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Energy Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_energy_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_energy_USA_vs_EuropeCHE_1Y.png"),
    g_pd_energy_1y,
    width = 10, height = 6, dpi = 150
  )
}
#materials
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Materials sector rows ----------------------------
df_materials <- df0 |>
  dplyr::mutate(
    is_materials = stringr::str_detect(
      sector,
      regex("MATERIALS|MINING|CHEMICALS|METALS", ignore_case = TRUE)  # Matching Materials sector
    )
  ) |>
  dplyr::filter(is_materials)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_materials_reg <- df_materials |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Materials",
      country %in% eu_countries ~ "European Materials (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_materials_1y <- df_materials_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Materials ------
g_pd_materials_1y <- ggplot(pd_materials_1y,
                            aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Materials Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_materials_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_materials_USA_vs_EuropeCHE_1Y.png"),
    g_pd_materials_1y,
    width = 10, height = 6, dpi = 150
  )
}
#utilities
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Utilities sector rows ----------------------------
df_utilities <- df0 |>
  dplyr::mutate(
    is_utilities = stringr::str_detect(
      sector,
      regex("UTILITIES|ELECTRICITY|WATER|GAS", ignore_case = TRUE)  # Matching Utilities sector
    )
  ) |>
  dplyr::filter(is_utilities)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_utilities_reg <- df_utilities |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Utilities",
      country %in% eu_countries ~ "European Utilities (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_utilities_1y <- df_utilities_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Utilities ------
g_pd_utilities_1y <- ggplot(pd_utilities_1y,
                            aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Utilities Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_utilities_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_utilities_USA_vs_EuropeCHE_1Y.png"),
    g_pd_utilities_1y,
    width = 10, height = 6, dpi = 150
  )
}
#real estate
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Real Estate sector rows -------------------------
df_real_estate <- df0 |>
  dplyr::mutate(
    is_real_estate = stringr::str_detect(
      sector,
      regex("REAL ESTATE|PROPERTY|REIT", ignore_case = TRUE)  # Matching Real Estate sector
    )
  ) |>
  dplyr::filter(is_real_estate)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_real_estate_reg <- df_real_estate |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Real Estate",
      country %in% eu_countries ~ "European Real Estate (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_real_estate_1y <- df_real_estate_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Real Estate -----
g_pd_real_estate_1y <- ggplot(pd_real_estate_1y,
                              aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Real Estate Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_real_estate_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_real_estate_USA_vs_EuropeCHE_1Y.png"),
    g_pd_real_estate_1y,
    width = 10, height = 6, dpi = 150
  )
}
#consumer staples
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Consumer Staples sector rows ----------------------
df_consumer_staples <- df0 |>
  dplyr::mutate(
    is_consumer_staples = stringr::str_detect(
      sector,
      regex("CONSUMER STAPLES|FOOD|BEVERAGES|HOUSEHOLD|CONS STAP", ignore_case = TRUE)  # Matching Consumer Staples sector
    )
  ) |>
  dplyr::filter(is_consumer_staples)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_consumer_staples_reg <- df_consumer_staples |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Consumer Staples",
      country %in% eu_countries ~ "European Consumer Staples (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_consumer_staples_1y <- df_consumer_staples_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Consumer Staples -----
g_pd_consumer_staples_1y <- ggplot(pd_consumer_staples_1y,
                                   aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Consumer Staples Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_consumer_staples_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_consumer_staples_USA_vs_EuropeCHE_1Y.png"),
    g_pd_consumer_staples_1y,
    width = 10, height = 6, dpi = 150
  )
}
#consumer discretionary
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Consumer Discretionary sector rows ----------------
df_consumer_discretionary <- df0 |>
  dplyr::mutate(
    is_consumer_discretionary = stringr::str_detect(
      sector,
      regex("CONSUMER DISCRETIONARY|AUTOMOTIVE|RETAIL|LEISURE|CONS DISC", ignore_case = TRUE)  # Matching Consumer Discretionary sector
    )
  ) |>
  dplyr::filter(is_consumer_discretionary)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_consumer_discretionary_reg <- df_consumer_discretionary |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Consumer Discretionary",
      country %in% eu_countries ~ "European Consumer Discretionary (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_consumer_discretionary_1y <- df_consumer_discretionary_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Consumer Discretionary -----
g_pd_consumer_discretionary_1y <- ggplot(pd_consumer_discretionary_1y,
                                         aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Consumer Discretionary Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_consumer_discretionary_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_consumer_discretionary_USA_vs_EuropeCHE_1Y.png"),
    g_pd_consumer_discretionary_1y,
    width = 10, height = 6, dpi = 150
  )
}
#communications
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Identify Communications sector rows ----------------------
df_communications <- df0 |>
  dplyr::mutate(
    is_communications = stringr::str_detect(
      sector,
      regex("COMMUNICATIONS|TELECOM|MEDIA|INTERNET", ignore_case = TRUE)  # Matching Communications sector
    )
  ) |>
  dplyr::filter(is_communications)

# --- C) Keep only USA + Europe + Switzerland ----------------------
df_communications_reg <- df_communications |>
  dplyr::filter(country == "USA" | country %in% eu_countries) |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA Communications",
      country %in% eu_countries ~ "European Communications (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)
  )

# --- D) Monthly averages: only 1-Year PD ---------------------------
pd_communications_1y <- df_communications_reg |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- E) Plot: USA vs Europe (incl. Switzerland) for Communications -----
g_pd_communications_1y <- ggplot(pd_communications_1y,
                                 aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD — Communications Sector\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_communications_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_communications_USA_vs_EuropeCHE_1Y.png"),
    g_pd_communications_1y,
    width = 10, height = 6, dpi = 150
  )
}
#pd USA vs Europa
# --- A) Define European countries + Switzerland -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE","GBR", "CHE"   # <-- Switzerland added
)

# --- B) Keep US and European (+ Switzerland) companies -----------
df_us_europe <- df0 |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA",
      country %in% eu_countries ~ "Europe (+CHE)",
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)   # Focusing on 1-year PD
  ) |>
  dplyr::filter(region %in% c("USA", "Europe (+CHE)"))

# --- C) Calculate monthly averages for both regions ----------------
pd_us_europe_1y <- df_us_europe |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- D) Plot: PD comparison between all US companies and European (+Switzerland) companies ---
g_pd_us_europe_1y <- ggplot(pd_us_europe_1y,
                            aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD Comparison\nUSA vs Europe (+Switzerland)",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_us_europe_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_US_vs_EuropeCHE_1Y.png"),
    g_pd_us_europe_1y,
    width = 10, height = 6, dpi = 150
  )
}
#plot PD separando USA EU, Uk e Svizzera
# --- A) Define European countries (excluding UK and Switzerland) -------------------
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
  "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
  "ROU","SVK","SVN","ESP","SWE"  # <-- Switzerland and UK excluded
)

# --- B) Keep USA, European (excluding UK and Switzerland), UK and Switzerland companies -----------
df_us_europe_uk_switzerland <- df0 |>
  dplyr::mutate(
    region = dplyr::case_when(
      country == "USA" ~ "USA",                       # USA region
      country == "GBR" ~ "UK",                        # UK region
      country == "CHE" ~ "Switzerland",               # Switzerland region
      country %in% eu_countries ~ "Europe",           # European countries (excluding UK and Switzerland)
      TRUE ~ "Other"
    ),
    month  = lubridate::floor_date(date, "month"),
    q_1y   = as.numeric(q_1y)   # Focusing on 1-year PD
  ) |>
  dplyr::filter(region %in% c("USA", "UK", "Europe", "Switzerland"))

# --- C) Calculate monthly averages for all regions -----------------
pd_us_europe_uk_switzerland_1y <- df_us_europe_uk_switzerland |>
  dplyr::group_by(region, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(q_1y, na.rm = TRUE),
    .groups   = "drop"
  )

# --- D) Plot: PD comparison between all US, UK, European (+ Switzerland) companies ---
g_pd_us_europe_uk_switzerland_1y <- ggplot(pd_us_europe_uk_switzerland_1y,
                                           aes(x = month, y = avg_pd_1y, color = region)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "1-Year PD Comparison\nUSA vs UK vs Europe vs Switzerland",
    x     = "Date",
    y     = "Average PD (1-Year)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

print(g_pd_us_europe_uk_switzerland_1y)

# Optional: save to file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "pd_US_vs_UK_vs_Europe_1Y.png"),
    g_pd_us_europe_uk_switzerland_1y,
    width = 10, height = 6, dpi = 150
  )
}
#heatmap
# --- A) Aggregate 1-Year PD by Country and Month -------------------
# Assuming df0 is your data frame with necessary columns
df_monthly_avg_pd <- df0 |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month")  # Extract month from the date
  ) |>
  dplyr::group_by(country, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(as.numeric(q_1y), na.rm = TRUE),  # Average 1Y PD for each country and month
    .groups = "drop"
  )

# --- B) Create the heatmap using ggplot2 ---------------------------
# Create the heatmap
g_monthly_pd_heatmap <- ggplot(df_monthly_avg_pd, aes(x = month, y = country, fill = avg_pd_1y)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgrey", high = "darkgreen") +  # Color scale from white (low) to red (high)
  labs(
    title = "Heatmap of Monthly 1-Year PD by Country",
    x = "Month",
    y = "Country",
    fill = "1-Year PD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(face = "bold", size = 14)
  )

# Print the heatmap
print(g_monthly_pd_heatmap)

# Optional: save the heatmap to a file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "heatmap_monthly_1Y_PD_by_country.png"),
    g_monthly_pd_heatmap,
    width = 12, height = 8, dpi = 150
  )
}
#heatmap sectors
# --- A) Aggregate 1-Year PD by Sector and Month -------------------
# Assuming df0 is your data frame with necessary columns
df_monthly_avg_pd_sector <- df0 |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month")  # Extract month from the date
  ) |>
  dplyr::group_by(sector, month) |>
  dplyr::summarise(
    avg_pd_1y = mean(as.numeric(q_1y), na.rm = TRUE),  # Average 1Y PD for each sector and month
    .groups = "drop"
  )

# --- B) Create the heatmap using ggplot2 ---------------------------
# Create the heatmap
g_monthly_pd_heatmap_sector <- ggplot(df_monthly_avg_pd_sector, aes(x = month, y = sector, fill = avg_pd_1y)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgrey", high = "darkgreen") +  # Color scale from white (low) to red (high)
  labs(
    title = "Heatmap of Monthly 1-Year PD by Sector",
    x = "Month",
    y = "Sector",
    fill = "1-Year PD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(face = "bold", size = 14)
  )

# Print the heatmap
print(g_monthly_pd_heatmap_sector)

# Optional: save the heatmap to a file
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "heatmap_monthly_1Y_PD_by_sector.png"),
    g_monthly_pd_heatmap_sector,
    width = 12, height = 8, dpi = 150
  )
}
#1 month PD
# --- A) Aggregate 1-Month PD by Sector and Month -------------------
df_monthly_avg_pd_sector_1m <- df0 |>
  dplyr::mutate(
    month = lubridate::floor_date(date, "month")  # Extract month
  ) |>
  dplyr::group_by(sector, month) |>
  dplyr::summarise(
    avg_pd_1m = mean(as.numeric(q_1m), na.rm = TRUE),  # Average 1M PD
    .groups = "drop"
  )

# --- B) Create heatmap using ggplot2 -------------------------------
g_monthly_pd_heatmap_sector_1m <- ggplot(df_monthly_avg_pd_sector_1m,
                                         aes(x = month, y = sector, fill = avg_pd_1m)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgrey", high = "darkgreen") +  # Adjust colors if needed
  labs(
    title = "Heatmap of Monthly 1-Month PD by Sector",
    x = "Month",
    y = "Sector",
    fill = "1M PD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

# --- C) Print heatmap ----------------------------------------------
print(g_monthly_pd_heatmap_sector_1m)

# --- D) Save heatmap (optional) ------------------------------------
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "heatmap_monthly_1M_PD_by_sector.png"),
    g_monthly_pd_heatmap_sector_1m,
    width = 12, height = 8, dpi = 150
  )
}
#average pd GAP
# --- 7bis) Average PD term-structure gap (5Y - 1Y) by sector ----

# 1. Costruisco il gap 5Y - 1Y (se non vuoi usare slope_5y_1y già esistente)
gaps <- gaps %>%
  dplyr::mutate(
    gap_5y_1y = q_5y - q_1y   # PD 5Y - PD 1Y
  )

# 2. Calcolo la media del gap per settore su tutto il campione
avg_gap_sector <- gaps %>%
  dplyr::group_by(sector) %>%
  dplyr::summarise(
    avg_gap_5y_1y = mean(gap_5y_1y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(avg_gap_5y_1y)

# 3. Grafico a barre orizzontali (stile slide)
g_gap_termstructure_sector <- ggplot(avg_gap_sector,
                                     aes(x = avg_gap_5y_1y,
                                         y = reorder(sector, avg_gap_5y_1y))) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Average PD Term-Structure Gap (5Y – 1Y) by Sector",
    x = "Gap (PD 5Y – PD 1Y)",
    y = "Sector"
  ) +
  theme_minimal()

# 4. Salvataggio del grafico
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "fig_gap_5y_1y_by_sector.png"),
    g_gap_termstructure_sector,
    width = 10, height = 6, dpi = 150
  )
}
#prova
#############################################
### 1) Packages
#############################################
req <- c("arrow", "dplyr", "ggplot2", "lubridate", "stringr", "readr", "scales")
new <- req[!(req %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

#############################################
### 2) Settings
#############################################
TEST_MODE  <- FALSE
OUTPUT_DIR <- file.path("02_team_modules", "B_Descriptive_Analysis")
if (!TEST_MODE) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

parquet_path <- "01_data_clean/output_features.parquet"

#############################################
### 3) Read + basic cleaning
#############################################
raw <- arrow::read_parquet(parquet_path)
names(raw) <- tolower(names(raw))

date_col    <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^data_date$|date|time")])
country_col <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^country$|country_code|ctry")])
sector_col  <- dplyr::first(names(raw)[stringr::str_detect(names(raw), "^gdesc$|sector|industry|sic_desc")])

to_Date_safe <- function(x){
  if (inherits(x,"Date"))   return(x)
  if (inherits(x,"POSIXt")) return(as.Date(x))
  if (is.numeric(x))        return(as.Date(x, origin = "1970-01-01"))
  if (is.character(x)) {
    y <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(y))) y <- suppressWarnings(as.Date(x))
    return(y)
  }
  as.Date(x)
}

df0 <- raw %>%
  dplyr::rename(
    date    = !!date_col,
    country = !!country_col
  ) %>%
  dplyr::mutate(
    date    = to_Date_safe(date),
    country = toupper(country),
    sector  = if (!is.null(sector_col)) as.character(.data[[sector_col]]) else "UNKNOWN"
  )

#############################################
### 4) Build q_* if needed
#############################################
have_q <- all(c("q_1y", "q_5y") %in% names(df0))

if (!have_q) {
  message("q_* not found. Deriving from kdp_*")
  pd_monthly <- function(p, months) 1 - (1 - p)^(months/12)
  
  for (nm in c("kdp_1yr", "kdp_5yr")) {
    if (nm %in% names(df0))
      df0[[nm]] <- readr::parse_number(df0[[nm]])
  }
  
  df0 <- df0 %>%
    mutate(
      q_1y = if ("kdp_1yr" %in% names(.)) pd_monthly(kdp_1yr, 12) else NA_real_,
      q_5y = if ("kdp_5yr" %in% names(.)) pd_monthly(kdp_5yr, 60) else NA_real_
    )
}

#############################################
### 5) Keep Europe (incl. UK, CHE, NOR) + USA, 2015–2024
#############################################
EU_ISO3 <- c("AUT","BEL","CYP","DEU","DNK","ESP","EST","FIN","FRA","GBR",
             "GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL","PRT",
             "SVK","SVN","SWE","CHE")

df <- df0 %>%
  filter(country %in% c(EU_ISO3, "USA")) %>%
  filter(date >= as.Date("2015-01-01"),
         date <= as.Date("2024-12-31"))

#############################################
### 6) Term-structure gap 5Y – 1Y
#############################################
gaps <- df %>%
  mutate(
    q_1y = as.numeric(q_1y),
    q_5y = as.numeric(q_5y),
    gap_5y_1y = q_5y - q_1y         # PD 5Y – PD 1Y
  )

#############################################
### 7) EUROPE graph (incl. UK & Switzerland)
#############################################
europe_gaps <- gaps %>%
  filter(country %in% EU_ISO3)

avg_gap_europe <- europe_gaps %>%
  group_by(sector) %>%
  summarise(
    avg_gap_5y_1y = mean(gap_5y_1y, na.rm = TRUE),   # average over time & firms
    .groups = "drop"
  ) %>%
  arrange(avg_gap_5y_1y)

g_gap_termstructure_europe <- ggplot(
  avg_gap_europe,
  aes(x = avg_gap_5y_1y,
      y = reorder(sector, avg_gap_5y_1y))
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Average PD Term-Structure Gap (5Y – 1Y) by Sector\nEurope (incl. UK & Switzerland)",
    x = "Gap (PD 5Y – PD 1Y)",
    y = "Sector"
  ) +
  theme_minimal()

if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "fig_gap_5y_1y_by_sector_EU.png"),
    g_gap_termstructure_europe,
    width = 10, height = 6, dpi = 150
  )
}

#############################################
### 8) USA graph
#############################################
usa_gaps <- gaps %>%
  filter(country == "USA")

avg_gap_usa <- usa_gaps %>%
  group_by(sector) %>%
  summarise(
    avg_gap_5y_1y = mean(gap_5y_1y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_gap_5y_1y)

g_gap_termstructure_usa <- ggplot(
  avg_gap_usa,
  aes(x = avg_gap_5y_1y,
      y = reorder(sector, avg_gap_5y_1y))
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Average PD Term-Structure Gap (5Y – 1Y) by Sector\nUSA",
    x = "Gap (PD 5Y – PD 1Y)",
    y = "Sector"
  ) +
  theme_minimal()

if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "fig_gap_5y_1y_by_sector_USA.png"),
    g_gap_termstructure_usa,
    width = 10, height = 6, dpi = 150
  )
}
#prova 2
#############################################
### PD Term-Structure Gap (5Y – 1Y)
### Europe only, sector medians, last month
#############################################

# 0) Define Europe (same as before, incl. UK, CHE, NOR)
EU_ISO3 <- c("AUT","BEL","CYP","DEU","DNK","ESP","EST","FIN","FRA","GBR",
             "GRC","HRV","IRL","ITA","LUX","MLT","NLD","NOR","POL","PRT",
             "SVK","SVN","SWE","CHE")

# 1) Keep only European countries and create month variable
gaps_eu <- gaps %>%
  dplyr::filter(country %in% EU_ISO3) %>%
  dplyr::mutate(
    month = lubridate::floor_date(date, "month"),
    gap_5y_1y = as.numeric(q_5y) - as.numeric(q_1y)   # PD 5Y – PD 1Y
  )

# 2) First step: monthly medians at country × sector level
monthly_cs <- gaps_eu %>%
  dplyr::group_by(country, sector, month) %>%
  dplyr::summarise(
    med_q_1y = median(q_1y, na.rm = TRUE),
    med_q_5y = median(q_5y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    gap_5y_1y = med_q_5y - med_q_1y
  )

# 3) Second step: for each month, median across countries by sector
monthly_sector <- monthly_cs %>%
  dplyr::group_by(sector, month) %>%
  dplyr::summarise(
    med_gap_5y_1y = median(gap_5y_1y, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Take the last month in the sample
last_month <- max(monthly_sector$month, na.rm = TRUE)

snapshot_sector <- monthly_sector %>%
  dplyr::filter(month == last_month) %>%
  dplyr::arrange(med_gap_5y_1y)

# 5) Plot: PD term-structure gap (5Y – 1Y) by sector – Europe
g_gap_eu_lastmonth <- ggplot(
  snapshot_sector,
  aes(x = med_gap_5y_1y,
      y = reorder(sector, med_gap_5y_1y))
) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = paste0("PD Term-Structure Gap (5Y – 1Y) by Sector\nEurope – ",
                   format(last_month, "%b %Y")),
    x = "Gap (PD 5Y – PD 1Y)",
    y = "Sector"
  ) +
  theme_minimal()

# 6) Save the figure (optional)
if (!TEST_MODE) {
  ggsave(
    file.path(OUTPUT_DIR, "fig_gap_5y_1y_by_sector_EU_lastmonth.png"),
    g_gap_eu_lastmonth,
    width = 10, height = 6, dpi = 150
  )
}