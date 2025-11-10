# ==========================================================
# 05_dependencies_clean.R
# DRIM2025 Project – Risk Dependencies (Correlation Network)
# ==========================================================
out_dir <- "02_team_modules/D_Dependencies_Analysis"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(ggraph)
  library(ggplot2)
})

# ---------- 1) Carica dati ----------
df <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

# Verifica colonne chiave
stopifnot(all(c("country", "gdesc", "q_1y") %in% names(df)))

# ---------- 2) Costruisci matrice PD (paese-settore × tempo) ----------
pd_matrix <- df %>%
  group_by(country, gdesc, data_date) %>%
  summarise(PD = mean(q_1y, na.rm = TRUE), .groups = "drop") %>%
  unite("id", country, gdesc, remove = FALSE) %>%
  pivot_wider(names_from = data_date, values_from = PD)

# Rimuove colonne non numeriche
row_names <- pd_matrix$id
pd_matrix <- pd_matrix %>% select(-id, -country, -gdesc)
pd_matrix <- as.data.frame(pd_matrix)
rownames(pd_matrix) <- row_names

# ---------- 3) Calcola matrice di correlazione ----------
# ---------- 3) Calcola matrice di correlazione ----------
cor_mat <- cor(t(pd_matrix), use = "pairwise.complete.obs")

# Sostituisce eventuali NaN o Inf con 0
cor_mat[!is.finite(cor_mat)] <- 0

# Applica soglia (solo correlazioni forti)
# ---------- 4) Applica soglia e rimuovi correlazioni negative ----------
threshold <- 0.7

# Mantieni solo correlazioni positive e forti
cor_mat[cor_mat < threshold] <- 0
cor_mat[!is.finite(cor_mat)] <- 0

# ---------- 4) Applica soglia (solo correlazioni forti) ----------
threshold <- 0.7
cor_mat[abs(cor_mat) < threshold] <- 0

# ---------- 5) Crea grafo delle correlazioni ----------
g <- graph_from_adjacency_matrix(cor_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

# Rimuove nodi isolati (senza legami)
g <- delete.vertices(g, degree(g) == 0)

# ---------- 6) Community detection (Louvain) ----------
comm <- cluster_louvain(g)
V(g)$community <- membership(comm)

# Controllo
cat("Numero di nodi nel grafo:", vcount(g), "\n")
cat("Numero di community trovate:", length(unique(V(g)$community)), "\n")

library(tidyr)
library(dplyr)
library(igraph)
library(scales)

# ---------- 7) Tabella con nodi, community e colori ----------
community_table <- data.frame(
  node = names(V(g)),
  community = as.integer(V(g)$community)
)

# Separa in country e sector
community_table <- community_table %>%
  separate(node, into = c("country", "sector"), sep = "_", remove = FALSE) %>%
  arrange(community)

# Assegna una palette di colori alle community (stessa logica del plot)
n_communities <- length(unique(V(g)$community))
palette <- hue_pal()(n_communities)
V(g)$color <- palette[V(g)$community]

# Aggiungi i colori alla tabella
community_colors <- data.frame(
  community = sort(unique(as.integer(V(g)$community))),   # ✅ anche qui
  color = palette[sort(unique(as.integer(V(g)$community)))]
)

community_table <- community_table %>%
  left_join(community_colors, by = "community")

# Mostra riepilogo
print(table(community_table$community))
print(community_colors)

# Sovrascrivi il file esistente
write.csv(community_table,
          "02_team_modules/D_Dependencies_Analysis/community_members.csv",
          row.names = FALSE)

message("✅ File aggiornato con colonna 'color' sovrascritto in: 02_team_modules/D_Dependencies_Analysis/community_members.csv")