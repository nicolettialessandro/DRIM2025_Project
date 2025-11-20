# ==========================================================
# 04_join_clusters.R
# DRIM2025 Project – Merge cleaned data with cluster labels
# ==========================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

# ---------- 1) Carica la mappa country–sector → cluster ----------
# Usa il file aggiornato della parte C (dopo la pulizia e k=3)
cluster_path <- "02_team_modules/C_Clustering_Segmentation/output_clusters.csv"

if (!file.exists(cluster_path)) stop("❌ File dei cluster non trovato: ", cluster_path)

clusters_map <- read_csv(cluster_path, show_col_types = FALSE) %>%
  select(country, gdesc, Cluster) %>%
  mutate(
    country = toupper(country),
    gdesc = as.character(gdesc),
    Cluster = as.factor(Cluster)
  )

message("✅ Cluster map caricata con ", nrow(clusters_map), " combinazioni country–sector.")

# ---------- 2) Carica i dati mensili originali ----------
data_path <- "01_data_clean/output_features.parquet"

if (!file.exists(data_path)) stop("❌ File dati base non trovato: ", data_path)

data_full <- read_parquet(data_path) %>%
  mutate(
    country = toupper(country),
    gdesc = as.character(gdesc)
  )

message("✅ Dati originali caricati con ", nrow(data_full), " osservazioni mensili.")

# ---------- 3) Join: assegna a ogni riga il suo cluster ----------
data_with_cluster <- data_full %>%
  left_join(clusters_map, by = c("country", "gdesc"))

# Controllo: quanti record senza cluster?
missing_clusters <- sum(is.na(data_with_cluster$Cluster))
if (missing_clusters > 0) {
  warning("⚠️ ", missing_clusters, " osservazioni non hanno trovato corrispondenza di cluster.")
} else {
  message("✅ Tutte le osservazioni hanno un cluster assegnato.")
}

# ---------- 4) Salvataggio ----------
out_path <- "01_data_clean/output_features_with_cluster.parquet"
write_parquet(data_with_cluster, out_path)
message("💾 File salvato in: ", normalizePath(out_path))

# ---------- 5) Verifica finale ----------
message("Riepilogo per cluster:")
print(table(data_with_cluster$Cluster, useNA = "ifany"))

# ==========================================================
# 04_join_clusters.R
# DRIM2025 Project – Merge cleaned data with cluster labels
# ==========================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

# ---------- 1) Carica la mappa country–sector → cluster ----------
# Usa il file aggiornato della parte C (dopo la pulizia e k=3)
cluster_path <- "02_team_modules/C_Clustering_Segmentation/output_clusters.csv"

if (!file.exists(cluster_path)) stop("❌ File dei cluster non trovato: ", cluster_path)

clusters_map <- read_csv(cluster_path, show_col_types = FALSE) %>%
  select(country, gdesc, Cluster) %>%
  mutate(
    country = toupper(country),
    gdesc = as.character(gdesc),
    Cluster = as.factor(Cluster)
  )

message("✅ Cluster map caricata con ", nrow(clusters_map), " combinazioni country–sector.")

# ---------- 2) Carica i dati mensili originali ----------
data_path <- "01_data_clean/output_features.parquet"

if (!file.exists(data_path)) stop("❌ File dati base non trovato: ", data_path)

data_full <- read_parquet(data_path) %>%
  mutate(
    country = toupper(country),
    gdesc = as.character(gdesc)
  )

message("✅ Dati originali caricati con ", nrow(data_full), " osservazioni mensili.")

# ---------- 3) Join: assegna a ogni riga il suo cluster ----------
data_with_cluster <- data_full %>%
  left_join(clusters_map, by = c("country", "gdesc"))

# Controllo: quanti record senza cluster?
missing_clusters <- sum(is.na(data_with_cluster$Cluster))
if (missing_clusters > 0) {
  warning("⚠️ ", missing_clusters, " osservazioni non hanno trovato corrispondenza di cluster.")
} else {
  message("✅ Tutte le osservazioni hanno un cluster assegnato.")
}


# ---------- 3B) Diagnosi delle osservazioni mancanti ----------
missing_df <- data_with_cluster %>%
  filter(is.na(Cluster)) %>%
  distinct(country, gdesc)

if (nrow(missing_df) > 0) {
  message("⚠️ Elenco delle combinazioni senza cluster:")
  print(missing_df)
  write_csv(missing_df, "01_data_clean/missing_country_sector.csv")
}

# ---------- 3C) Fix automatico dei NA ----------
if (missing_clusters > 0) {
  country_mode <- data_with_cluster %>%
    filter(!is.na(Cluster)) %>%
    group_by(country) %>%
    summarise(mode_cluster = names(sort(table(Cluster), decreasing = TRUE))[1])
  
  data_with_cluster <- data_with_cluster %>%
    left_join(country_mode, by = "country") %>%
    mutate(Cluster = ifelse(is.na(Cluster), mode_cluster, Cluster)) %>%
    select(-mode_cluster)
  
  message("🔧 Cluster NA rimpiazzati con il cluster più frequente nel paese.")
}


# ---------- 4) Salvataggio ----------
out_path <- "01_data_clean/output_features_with_cluster.parquet"
write_parquet(data_with_cluster, out_path)
message("💾 File salvato in: ", normalizePath(out_path))

# ---------- 5) Verifica finale ----------
message("Riepilogo per cluster:")
print(table(data_with_cluster$Cluster, useNA = "ifany"))

library(ggplot2)

# Converte la tabella in data frame per il plot
cluster_counts <- as.data.frame(table(data_with_cluster$Cluster, useNA = "ifany"))
names(cluster_counts) <- c("Cluster", "Count")

# Rinomina i cluster con etichette più intuitive
cluster_counts <- cluster_counts %>%
  mutate(Cluster_Label = recode(Cluster,
                                "1" = "Low risk",
                                "2" = "High risk"))

p_cluster <- ggplot(cluster_counts, aes(x = Cluster_Label, y = Count, fill = Cluster_Label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  theme_minimal() +
  labs(title = "Cluster distribution per level of risk",
       x = "Level of risk", y = "Number of observations")


ggsave(
  filename = file.path(out_dir, "cluster_distribution.png"),
  plot =p_cluster,
  width = 7,
  height = 5
)