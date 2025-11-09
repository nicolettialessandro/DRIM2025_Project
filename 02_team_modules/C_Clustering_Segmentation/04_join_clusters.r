library(arrow)
library(dplyr)
library(readr)

# Carica la mappa country–sector → cluster
clusters_map <- read_csv("02_team_modules/C_Clustering_Segmentation/output_clusters.csv") %>%
  select(country, gdesc, Cluster)

# Carica i dati mensili originali (con PD, slopes, curvature, ecc.)
data_full <- read_parquet("01_data_clean/output_features.parquet")

# Uniforma i nomi
data_full <- data_full %>%
  mutate(country = toupper(country),
         gdesc = as.character(gdesc))

# Aggiungi il cluster corrispondente
data_with_cluster <- data_full %>%
  left_join(clusters_map, by = c("country", "gdesc"))

# Salva il nuovo file per le analisi successive
write_parquet(data_with_cluster, "01_data_clean/output_features_with_cluster.parquet")

message("✅ File con cluster assegnati salvato.")