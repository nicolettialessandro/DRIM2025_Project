# ==========================================================
# 03_clustering_segmentation.R  (versione finale – auto k con NbClust + silhouette plot)
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(cluster)
  library(factoextra)
  library(NbClust)
  library(stringr)
})

set.seed(123)

# ---------- 0) Utility ----------
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

out_dir <- "02_team_modules/C_Clustering_Segmentation"
ensure_dir(out_dir)

# ---------- 1) Load cleaned data ----------
data <- read_parquet("01_data_clean/output_features.parquet")

# Normalizza campi chiave
data <- data %>%
  mutate(
    country = trimws(as.character(country)),
    gdesc   = str_squish(as.character(gdesc))
  )

stopifnot(all(c("country", "gdesc") %in% names(data)))
kdp_cols <- grep("^kdp_", names(data), value = TRUE)
if (length(kdp_cols) == 0) stop("Nessuna colonna KDP_ trovata nel file parquet.")

# ---------- 2) Aggregate PDs by country-sector ----------
agg_df <- data %>%
  group_by(country, gdesc) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# ---------- 2B) Rimozione outlier robusta ----------
is_cyp_hc <- with(
  agg_df,
  country == "CYP" & str_detect(str_to_lower(gdesc), "^health[\\s\\-]?care$")
)
n_out <- sum(is_cyp_hc, na.rm = TRUE)
agg_df <- agg_df %>% filter(!is_cyp_hc)
message("🧹 Rimosse ", n_out, " occorrenze di CYP - Health Care")

agg_df <- agg_df %>% filter(!(country == "LUX" & gdesc == "Utilities"))
message("🧹 Rimosso outlier: LUX - Utilities (distanza eccessiva dal centroide)")

if (any(with(agg_df, country == "CYP" & str_detect(str_to_lower(gdesc), "health")))) {
  stop("⚠️ L'outlier CYP - Health Care è ancora presente dopo il filtro.")
}

# ---------- 3) Matrix for clustering ----------
kdp_cols <- grep("^kdp_", names(agg_df), value = TRUE)
clust_mat  <- agg_df %>% select(all_of(kdp_cols)) %>% as.matrix()
clust_data <- scale(clust_mat)

# ---------- 4) Determina k ottimale con NbClust ----------
message("🔍 Ricerca del numero ottimale di cluster con NbClust...")
nb <- NbClust(
  data = clust_data,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans",
  index = "silhouette"
)

k_opt <- nb$Best.nc[1]
message("✅ Numero di cluster ottimale trovato: k = ", k_opt)

# ---------- 5) Grafico silhouette per k ----------
sil_df <- data.frame(
  k = 2:10,
  silhouette = nb$All.index[!is.na(nb$All.index)]
)

p_sil <- ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Silhouette scores per k",
    x = "Number of clusters",
    y = "Average silhouette width"
  ) +
  geom_vline(xintercept = k_opt, linetype = "dashed", color = "red") +
  annotate("text", x = k_opt, y = max(sil_df$silhouette), 
           label = paste("Optimal k =", k_opt), vjust = -0.8, color = "red")
print(p_sil)

ggsave(filename = file.path(out_dir, "silhouette_plot.png"), plot = p_sil, width = 7, height = 5)

# ---------- 6) K-means clustering ----------
set.seed(123)
km <- kmeans(clust_data, centers = k_opt, nstart = 100, algorithm = "Lloyd")
print(table(km$cluster))

# ---------- 7) Aggiungi cluster ai dati ----------
agg_df <- agg_df %>% mutate(Cluster = factor(km$cluster))

# ---------- 8) PCA plot ----------
p1 <- fviz_cluster(km, data = clust_data, geom = "point", ellipse.type = "convex") +
  ggtitle(paste("Clustering of Country–Sector Risk Profiles (k =", k_opt, ")"))
print(p1)

ggsave(filename = file.path(out_dir, "pca_clusters.png"), plot = p1, width = 7, height = 5)

# ---------- 9) Ordinamento cluster per rischio medio ----------
cluster_summary <- agg_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(PD_mean_overall = rowMeans(across(all_of(kdp_cols)), na.rm = TRUE)) %>%
  arrange(PD_mean_overall)

# Mappa numerazione: 1 = rischio basso, max = rischio alto
level_map <- setNames(seq_len(nrow(cluster_summary)), cluster_summary$Cluster)
agg_df <- agg_df %>%
  mutate(Cluster = factor(level_map[Cluster], levels = seq_len(nrow(cluster_summary))))

# ---------- 10) Top paesi/settori ----------
top_n <- 25
top_countries <- agg_df %>%
  count(Cluster, country, name = "n") %>%
  group_by(Cluster) %>%
  slice_max(n, n = top_n, with_ties = FALSE) %>%
  arrange(Cluster, desc(n)) %>%
  ungroup()

top_sectors <- agg_df %>%
  count(Cluster, gdesc, name = "n") %>%
  group_by(Cluster) %>%
  slice_max(n, n = top_n, with_ties = FALSE) %>%
  arrange(Cluster, desc(n)) %>%
  ungroup()

# ---------- 11) Grafico PD medie ----------
pd_long <- cluster_summary %>%
  select(Cluster, all_of(kdp_cols)) %>%
  pivot_longer(cols = all_of(kdp_cols), names_to = "Horizon", values_to = "PD")

p_pd <- ggplot(pd_long, aes(Horizon, PD, group = Cluster, color = Cluster)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = paste("Average Probability of Default by Cluster (k =", k_opt, ")"),
    subtitle = "Cluster ordinati dal rischio medio più basso (1) al più alto",
    y = "PD media (%)", x = "Orizzonte"
  )
print(p_pd)

ggsave(filename = file.path(out_dir, "pd_means_plot.png"), plot = p_pd, width = 7, height = 5)

# ---------- 12) Export ----------
write_csv(agg_df, file.path(out_dir, "output_clusters.csv"))
write_csv(cluster_summary, file.path(out_dir, "summary_clusters.csv"))
write_csv(top_countries, file.path(out_dir, "top_countries_per_cluster.csv"))
write_csv(top_sectors, file.path(out_dir, "top_sectors_per_cluster.csv"))

message("=== DONE ===")
message("File scritti in: ", normalizePath(out_dir))
message("Numero combinazioni paese–settore: ", nrow(agg_df))
message("Cluster:"); print(table(agg_df$Cluster))