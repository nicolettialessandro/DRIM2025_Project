# ==========================================================
# 03_clustering_segmentation.R  (versione finale – forza 3 cluster)
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

# Normalizza campi chiave (spazi, maiuscole/minuscole)
data <- data %>%
  mutate(
    country = trimws(as.character(country)),
    gdesc   = str_squish(as.character(gdesc))
  )

# Controlli base
stopifnot(all(c("country","gdesc") %in% names(data)))
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

# Rimuovi outlier LUX–Utilities
agg_df <- agg_df %>% filter(!(country == "LUX" & gdesc == "Utilities"))
message("🧹 Rimosso outlier: LUX - Utilities (distanza eccessiva dal centroide)")

# Controllo finale
if (any(with(agg_df, country == "CYP" & str_detect(str_to_lower(gdesc), "health")))) {
  stop("⚠️ L'outlier CYP - Health Care è ancora presente dopo il filtro.")
}

# ---------- 3) Matrix for clustering (senza outlier) ----------
kdp_cols <- grep("^kdp_", names(agg_df), value = TRUE)
clust_mat  <- agg_df %>% select(all_of(kdp_cols)) %>% as.matrix()
clust_data <- scale(clust_mat)

# ---------- 4) Forza manualmente il numero di cluster ----------
k_opt <- 3
message("🔧 Forzo manualmente k = ", k_opt)

# ---------- 5) K-means robusto ----------
set.seed(123)
km <- kmeans(clust_data, centers = k_opt, nstart = 100, algorithm = "Lloyd")

# Controlla se i cluster sono davvero 3
print(table(km$cluster))
if (length(unique(km$cluster)) < k_opt) {
  warning("⚠️ Uno o più cluster risultano vuoti: i dati sono troppo concentrati per k = 3.")
}

# ---------- 6) Aggiungi cluster ai dati ----------
agg_df <- agg_df %>% mutate(Cluster = factor(km$cluster))

# ---------- 7) PCA plot finale ----------
p1 <- fviz_cluster(km, data = clust_data, geom = "point", ellipse.type = "convex") +
  ggtitle(paste("Clustering of Country–Sector Risk Profiles (k =", k_opt, ")"))
print(p1)

# ---------- 8) Ordinamento cluster per rischio medio ----------
cluster_summary <- agg_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(PD_mean_overall = rowMeans(across(all_of(kdp_cols)), na.rm = TRUE)) %>%
  arrange(PD_mean_overall)

level_map <- setNames(seq_len(nrow(cluster_summary)), cluster_summary$Cluster)
agg_df <- agg_df %>%
  mutate(Cluster = factor(level_map[Cluster], levels = seq_len(nrow(cluster_summary))))

# ---------- 9) Top paesi/settori ----------
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

# ---------- 10) Grafico PD medie ----------
pd_long <- cluster_summary %>%
  select(Cluster, all_of(kdp_cols)) %>%
  pivot_longer(cols = all_of(kdp_cols), names_to = "Horizon", values_to = "PD")

p_pd <- ggplot(pd_long, aes(Horizon, PD, group = Cluster, color = Cluster)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Average Probability of Default by Cluster",
       subtitle = "Cluster ordinati dal rischio medio più basso (1) al più alto",
       y = "PD media (%)", x = "Orizzonte")
print(p_pd)

# ---------- 11) Export ----------
write_csv(agg_df, file.path(out_dir, "output_clusters.csv"))
write_csv(cluster_summary, file.path(out_dir, "summary_clusters.csv"))
write_csv(top_countries, file.path(out_dir, "top_countries_per_cluster.csv"))
write_csv(top_sectors, file.path(out_dir, "top_sectors_per_cluster.csv"))

message("=== DONE ===")
message("File scritti in: ", normalizePath(out_dir))
message("Numero combinazioni paese–settore: ", nrow(agg_df))
message("Cluster:"); print(table(agg_df$Cluster))