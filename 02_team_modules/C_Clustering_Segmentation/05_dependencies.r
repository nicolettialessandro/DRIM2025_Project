library(igraph)
library(dplyr)

df <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

# Calcola media PD per country–sector
agg <- df %>%
  group_by(country, gdesc) %>%
  summarise(mean_pd = mean(q_1y, na.rm=TRUE), .groups="drop")

# Matrice di correlazione
mat <- cor(t(as.matrix(agg$mean_pd)), use="pairwise.complete.obs")

# Crea grafo e trova comunità
g <- graph_from_adjacency_matrix(as.matrix(mat), mode="undirected", weighted=TRUE, diag=FALSE)
comm <- cluster_louvain(g)
plot(comm, g)