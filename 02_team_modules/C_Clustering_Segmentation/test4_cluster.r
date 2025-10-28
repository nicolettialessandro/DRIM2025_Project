# ==========================================================
# 03b_clustering_non_aggregated.R
# DRIM2025 Project – Credit Risk Clustering (firm-level)
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(cluster)
  library(factoextra)
})

set.seed(123)

# ---------- 1) Load data ----------
data <- read_parquet("01_data_clean/output_features.parquet")

# Controlla le colonne PD
kdp_cols <- grep("^kdp_", names(data), value = TRUE)
if (length(kdp_cols) == 0) stop("No PD columns (KDP_) found.")

# ---------- 2) Keep relevant columns ----------
df_firm <- data %>%
  select(country, gdesc, all_of(kdp_cols)) %>%
  drop_na()

cat("Numero di osservazioni (aziende):", nrow(df_firm), "\n")

# ---------- 3) Standardize PDs ----------
clust_data <- df_firm %>%
  select(all_of(kdp_cols)) %>%
  scale()

# ---------- 4) Run K-means with fixed 4 clusters ----------
k_opt <- 4
km_firm <- kmeans(clust_data, centers = k_opt, nstart = 50)
df_firm$Cluster <- factor(km_firm$cluster)

# ---------- 5) Summary by cluster ----------
cluster_summary_firm <- df_firm %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(PD_mean_overall = rowMeans(across(all_of(kdp_cols)), na.rm = TRUE)) %>%
  arrange(PD_mean_overall)

# Ordina i livelli dal rischio più basso al più alto
level_map <- setNames(seq_len(nrow(cluster_summary_firm)), cluster_summary_firm$Cluster)
df_firm <- df_firm %>%
  mutate(Cluster = factor(level_map[Cluster], levels = seq_len(nrow(cluster_summary_firm))))

# ---------- 6) Country/Sector composition ----------
all_countries_firm <- df_firm %>%
  count(Cluster, country, name = "n") %>%
  arrange(Cluster, desc(n))

all_sectors_firm <- df_firm %>%
  count(Cluster, gdesc, name = "n") %>%
  arrange(Cluster, desc(n))

# ---------- 7) Visualization ----------
p_pd_firm <- cluster_summary_firm %>%
  select(Cluster, all_of(kdp_cols)) %>%
  pivot_longer(cols = all_of(kdp_cols), names_to = "Horizon", values_to = "PD") %>%
  ggplot(aes(Horizon, PD, group = Cluster, color = Cluster)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Average PD by Cluster (Firm-level, k=4)",
       subtitle = "Clustering without aggregation",
       y = "Average PD (%)", x = "Horizon")

print(p_pd_firm)

fviz_cluster(km_firm, data = clust_data, geom = "point", ellipse.type = "convex") +
  ggtitle("Firm-level PD Clustering (k=4)")

# ---------- 8) Save results ----------
out_dir <- "02_team_modules/C_Clustering_Segmentation"
write_csv(df_firm, file.path(out_dir, "output_clusters_firm.csv"))
write_csv(cluster_summary_firm, file.path(out_dir, "summary_clusters_firm.csv"))
write_csv(all_countries_firm, file.path(out_dir, "all_countries_per_cluster_firm.csv"))
write_csv(all_sectors_firm, file.path(out_dir, "all_sectors_per_cluster_firm.csv"))

message("=== Firm-level clustering DONE ===")