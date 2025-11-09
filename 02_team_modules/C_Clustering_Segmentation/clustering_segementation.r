# ==========================================================
# 03_clustering_segmentation.R
# DRIM2025 Project – Credit Risk Clustering (clean & robust)
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(cluster)
  library(factoextra)
  library(NbClust)
})

set.seed(123)

# ---------- 0) Utility ----------
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

out_dir <- "02_team_modules/C_Clustering_Segmentation"
ensure_dir(out_dir)

# ---------- 1) Load cleaned data ----------
data <- read_parquet("01_data_clean/output_features.parquet")

# Controlli base
stopifnot(all(c("country","gdesc") %in% names(data)))
kdp_cols <- grep("^kdp_", names(data), value = TRUE)
if (length(kdp_cols) == 0) stop("Nessuna colonna KDP_ trovata nel file parquet.")

# ---------- 2) Aggregate PDs by country-sector ----------
agg_df <- data %>%
  group_by(country, gdesc) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Rimuove colonne KDP costanti o completamente NA (evita problemi in scale/kmeans)
is_all_na <- sapply(agg_df[kdp_cols], function(v) all(is.na(v)))
if (any(is_all_na)) {
  message("Rimossa/e colonna/e KDP tutta/e NA: ", paste(kdp_cols[is_all_na], collapse = ", "))
  kdp_cols <- kdp_cols[!is_all_na]
}

# Se qualche colonna ha varianza ~0, rimuovila (kmeans si basa su distanza euclidea)
near_zero_var <- function(x) isTRUE(all.equal(sd(x, na.rm = TRUE), 0))
nzv <- sapply(agg_df[kdp_cols], near_zero_var)
if (any(nzv)) {
  message("Rimossa/e colonna/e KDP con varianza ~0: ", paste(kdp_cols[nzv], collapse = ", "))
  kdp_cols <- kdp_cols[!nzv]
}
if (length(kdp_cols) < 2) stop("Servono almeno 2 orizzonti KDP non degeneri per il clustering.")

# ---------- 3) Matrix for clustering ----------
clust_mat <- agg_df %>%
  select(all_of(kdp_cols)) %>%
  as.matrix()

# Standardizza (z-score)
clust_data <- scale(clust_mat)

# ---------- 4) Determina k (NbClust + fallback silhouette) ----------
k_opt <- NA_integer_

# Prova NbClust (può fallire su alcuni dataset)
nb_res <- try({
  NbClust(clust_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
}, silent = TRUE)

if (!inherits(nb_res, "try-error")) {
  tab <- table(nb_res$Best.nc[1, ])
  k_opt <- as.numeric(names(which.max(tab)))
  message("NbClust suggerisce k = ", k_opt)
} else {
  message("NbClust non disponibile/ha fallito: passo alla silhouette.")
}

# Se NbClust ha fallito o ha dato qualcosa di strano, usa silhouette
if (is.na(k_opt) || k_opt < 2 || k_opt > 10) {
  sil_scores <- c()
  for (k in 2:10) {
    km_tmp <- kmeans(clust_data, centers = k, nstart = 25)
    sil <- silhouette(km_tmp$cluster, dist(clust_data))
    sil_scores <- c(sil_scores, mean(sil[, "sil_width"]))
  }
  k_opt <- which.max(sil_scores) + 1  # +1 perché parte da 2
  message("Silhouette suggerisce k = ", k_opt)
}

k_opt <- 4   # scelta analitica, compromesso tra separazione e granularità

# ---------- 5) K-means ----------
km <- kmeans(clust_data, centers = k_opt, nstart = 50)
agg_df <- agg_df %>%
  mutate(Cluster = factor(km$cluster))

# ---------- 5B) Rimozione outlier e rifacimento clustering ----------

# Conta quante osservazioni ha ciascun cluster
cluster_sizes <- agg_df %>%
  count(Cluster, name = "n")

# Trova i cluster con solo 1 osservazione (outlier)
outlier_clusters <- cluster_sizes %>%
  filter(n == 1) %>%
  pull(Cluster)

if (length(outlier_clusters) > 0) {
  message("Rimossi i cluster outlier: ", paste(outlier_clusters, collapse = ", "))
  
  # Filtra via l’outlier
  agg_df_clean <- agg_df %>%
    filter(!Cluster %in% outlier_clusters)
  
  # Rifai il clustering con k = 3 (dopo aver tolto l'outlier)
  kdp_cols <- grep("^kdp_", names(agg_df_clean), value = TRUE)
  clust_data_clean <- scale(as.matrix(agg_df_clean %>% select(all_of(kdp_cols))))
  
  set.seed(123)
  km3 <- kmeans(clust_data_clean, centers = 3, nstart = 50)
  
  # Aggiorna il dataframe con i nuovi cluster
  agg_df <- agg_df_clean %>%
    mutate(Cluster = factor(km3$cluster))
  
  message("Rieseguito K-means con k = 3 dopo rimozione outlier.")
} else {
  message("Nessun cluster outlier trovato — mantengo il clustering originale.")
}

# --- Silhouette plot per capire k ---
library(cluster)
sil_means <- c()
for (k in 2:10) {
  km <- kmeans(clust_data, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(clust_data))
  sil_means <- c(sil_means, mean(ss[, "sil_width"]))
}
sil_df <- data.frame(k = 2:10, silhouette = sil_means)

ggplot(sil_df, aes(k, silhouette)) +
  geom_line(color = "blue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Silhouette scores per k", x = "Number of clusters", y = "Average silhouette width")

ggsave("02_team_modules/C_Clustering_Segmentation/silhouette_plot.png", width = 8, height = 5)

# ---------- 6) Ordinamento cluster per rischio medio ----------
cluster_summary <- agg_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(PD_mean_overall = rowMeans(across(all_of(kdp_cols)), na.rm = TRUE)) %>%
  arrange(PD_mean_overall)

# Mappa i livelli (Cluster 1 = più basso rischio, ... = più alto rischio)
level_map <- setNames(seq_len(nrow(cluster_summary)), cluster_summary$Cluster)
agg_df <- agg_df %>%
  mutate(Cluster = factor(level_map[Cluster], levels = seq_len(nrow(cluster_summary))))

# Ricalcola il summary con i nuovi livelli ordinati
cluster_summary <- agg_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kdp_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(PD_mean_overall = rowMeans(across(all_of(kdp_cols)), na.rm = TRUE)) %>%
  arrange(as.integer(Cluster))

# ---------- 7) Paesi e Settori più rappresentati ----------
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

# ---------- 8) Grafici ----------
# (a) PD medie per orizzonte
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

# (b) PCA + cluster (solo per visualizzazione)
# NB: rifacciamo un kmeans sui livelli riordinati per coerenza visiva
km_plot <- kmeans(clust_data, centers = nlevels(agg_df$Cluster), nstart = 50)
fviz_cluster(km_plot, data = clust_data, geom = "point", ellipse.type = "convex") +
  ggtitle("Clustering of Country–Sector Risk Profiles (PCA view)")


library(ggplot2)

# Seleziona solo il cluster 2 (quello in alto a destra)
cluster_focus <- agg_df %>% filter(Cluster == 2)

# Scatterplot delle PD (puoi cambiare gli assi per esplorare)
ggplot(cluster_focus, aes(x = kdp_1mo, y = kdp_1yr)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_text(aes(label = paste(country, gdesc, sep = " - ")), hjust = 0, vjust = 0, size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Focus sul Cluster 2 (rischio più alto)",
       x = "PD a 1 mese", y = "PD a 1 anno")

# ---------- 9) Export ----------
write_csv(agg_df, file.path(out_dir, "output_clusters.csv"))
write_csv(cluster_summary, file.path(out_dir, "summary_clusters.csv"))
write_csv(top_countries, file.path(out_dir, "top_countries_per_cluster.csv"))
write_csv(top_sectors, file.path(out_dir, "top_sectors_per_cluster.csv"))

message("=== DONE ===")
message("k scelto: ", k_opt)
message("File scritti in: ", normalizePath(out_dir))

table(agg_df$Cluster)
agg_df %>% filter(Cluster == 3)

agg_df %>% filter(Cluster == 4)

# Controlla quanti elementi ci sono per cluster
table(agg_df$Cluster)

# Visualizza il contenuto del cluster "sospetto"
agg_df %>% filter(Cluster == 2)


