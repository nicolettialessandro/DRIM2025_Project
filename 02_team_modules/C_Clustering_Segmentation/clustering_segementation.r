#install.packages("data.table","cluster")

library(data.table)
library(arrow)
library(ggplot2)
library(cluster)  

#Carica features

set.seed(42)

features_path <- "01_data_clean/output_features.parquet"
DT <- as.data.table(read_parquet(features_path))

# colonne chiave “di forma” già create nella parte A
vars <- c("slope_1y_1m", "slope_5y_1y", "curvature", "curvature_5y",
          "q_1m", "q_6m", "q_1y", "q_3y", "q_5y")

# tieni solo righe complete su queste variabili
DT_use <- DT[, c("tic","data_date","gdesc", vars), with = FALSE]
DT_use <- DT_use[complete.cases(DT_use)]
nrow(DT_use)

# Standardizza (molto importante per K-means)
X <- as.matrix(DT_use[, ..vars])
X_scaled <- scale(X)            # salva centri e scale per profiling dopo

center_ <- attr(X_scaled, "center")
scale_  <- attr(X_scaled, "scale")

#Scegli k (elbow + silhouette su un sample per velocità)
# === Selezione k: elbow su 50k, silhouette su 5k (no O(n^2) gigantesca) ===
ns_elbow <- min(50000, nrow(X_scaled))
ns_sil   <- min(5000,  nrow(X_scaled))   # <<< qui il fix principale

set.seed(42)
idx_elbow <- sample.int(nrow(X_scaled), ns_elbow)
idx_sil   <- sample.int(nrow(X_scaled), ns_sil)

Xe <- X_scaled[idx_elbow, , drop = FALSE]
Xs <- X_scaled[idx_sil,   , drop = FALSE]

ks <- 2:8
elbow <- numeric(length(ks))
silav <- numeric(length(ks))

# 1) Elbow (veloce, nessuna distanza O(n^2))
for (i in seq_along(ks)) {
  k <- ks[i]
  km <- kmeans(Xe, centers = k, nstart = 5, iter.max = 50)
  elbow[i] <- km$tot.withinss
}

# 2) Silhouette: calcolo distanze UNA sola volta su 5k
d_sil <- dist(Xs)  # ora è ~ (5000*4999/2)*8B ≈ 100 MB
for (i in seq_along(ks)) {
  k <- ks[i]
  km <- kmeans(Xs, centers = k, nstart = 5, iter.max = 50)
  sil <- silhouette(km$cluster, d_sil)
  silav[i] <- mean(sil[, 3])
}

k_opt <- ks[which.max(silav)]
k_opt

#Salva i grafici diagnostici
dir.create("03_outputs/figs", recursive = TRUE, showWarnings = FALSE)
df_k <- data.frame(k = ks, elbow = elbow, silhouette = silav)
ggplot(df_k, aes(k, elbow)) + geom_line() + geom_point() +
  labs(title="Elbow (tot within SS)") + theme_minimal()
ggsave("03_outputs/figs/k_elbow.png", width=6, height=4, dpi=150)

ggplot(df_k, aes(k, silhouette)) + geom_line() + geom_point() +
  labs(title="Average silhouette") + theme_minimal()
ggsave("03_outputs/figs/k_silhouette.png", width=6, height=4, dpi=150)

# Allenamento scalabile del K-means

#fai k-means sul sample per trovare centri buoni;
#poi lancia k-means su TUTTO usando quei centri come inizializzazione (veloce e stabile).

# 4a) fit su sample
km_s <- kmeans(Xs, centers = k_opt, nstart = 10, iter.max = 100)

# 4b) fit su tutto, iniziando dai centri del sample
km_full <- kmeans(X_scaled, centers = km_s$centers, iter.max = 100)
table(km_full$cluster)


#Assegna cluster e salva
DT_use[, cluster := factor(km_full$cluster)]

# Tabella di profilo cluster (medie nelle unità ORIGINALI)
centers_scaled <- km_full$centers
centers_orig <- sweep(centers_scaled, 2, scale_, `*`)
centers_orig <- sweep(centers_orig, 2, center_, `+`)
centers_dt <- as.data.table(centers_orig)
centers_dt[, cluster := 1:nrow(centers_dt)]
setcolorder(centers_dt, c("cluster", vars))

# Salvataggi
dir.create("02_team_modules/C_Clustering_Segmentation", recursive = TRUE, showWarnings = FALSE)
cols_out <- c("tic","data_date","gdesc","cluster", vars)
fwrite(DT_use[, ..cols_out], "02_team_modules/C_Clustering_Segmentation/output_clusters.csv")
saveRDS(list(k = k_opt, centers_scaled = centers_scaled,
             center = center_, scale = scale_),
        "03_outputs/models/kmeans_centers.rds")

fwrite(centers_dt, "02_team_modules/C_Clustering_Segmentation/cluster_centroids.csv")

#Plot
# scatter slope vs curvature (sample per leggibilità)
ns_plot <- min(20000, nrow(DT_use))
idx_p <- sample.int(nrow(DT_use), ns_plot)

p1 <- ggplot(DT_use[idx_p], aes(slope_1y_1m, curvature, color = cluster)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(title = sprintf("K-means clustering (k = %d): slope vs curvature", k_opt),
       x = "Slope (1Y - 1M)", y = "Curvature (6M vs avg 1M & 1Y)") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("03_outputs/figs/clusters_slope_curvature.png", p1, width = 7, height = 5, dpi = 150)

# profilo centroidi (radar-like 'poverello' con linee, normalizzato)
centers_norm <- as.data.table(scale(centers_orig))
centers_norm[, cluster := factor(1:.N)]
centers_long <- melt(centers_norm, id.vars = "cluster",
                     variable.name = "feature", value.name = "z_score")

p2 <- ggplot(centers_long, aes(feature, z_score, group = cluster, color = cluster)) +
  geom_line(linewidth = 1) + geom_point() +
  coord_flip() +
  labs(title = "Cluster profiles (standardized features)") +
  theme_minimal()
ggsave("03_outputs/figs/cluster_profiles.png", p2, width = 7, height = 5, dpi = 150)

#Summary table
summary_tbl <- DT_use[, .(
  n = .N,
  slope_1y_1m = mean(slope_1y_1m),
  slope_5y_1y  = mean(slope_5y_1y),
  curvature    = mean(curvature),
  curvature_5y = mean(curvature_5y),
  q_1y         = mean(q_1y),
  q_5y         = mean(q_5y)
), by = cluster][order(cluster)]

fwrite(summary_tbl, "02_team_modules/C_Clustering_Segmentation/cluster_summary.csv")
summary_tbl

sector_mix <- DT_use[, .N, by = .(cluster, gdesc)][order(cluster, -N)]
fwrite(sector_mix, "02_team_modules/C_Clustering_Segmentation/cluster_sector_mix.csv")