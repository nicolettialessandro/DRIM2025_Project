# ==========================================================
# 05_dependencies_clean.R — DRIM2025 Correlation Network
# ==========================================================

out_dir <- "02_team_modules/D_Dependencies_Analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(ggraph)
  library(ggrepel)
  library(ggplot2)
})

# ---------- 1) Load data ----------
df <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

stopifnot(all(c("country", "gdesc", "q_1y", "Cluster") %in% names(df)))

# ---------- 2) Build PD matrix (country–sector × time) ----------
pd_matrix <- df %>%
  group_by(country, gdesc, data_date) %>%
  summarise(PD = mean(q_1y, na.rm = TRUE), .groups = "drop") %>%
  unite("node", country, gdesc, sep = "_", remove = FALSE) %>%
  pivot_wider(names_from = data_date, values_from = PD)

rownames_vec <- pd_matrix$node

pd_matrix <- pd_matrix %>% 
  select(-node, -country, -gdesc)

pd_matrix <- as.data.frame(pd_matrix)
rownames(pd_matrix) <- rownames_vec

# ---------- 3) Correlation matrix ----------
cor_mat <- cor(t(pd_matrix), use = "pairwise.complete.obs")
cor_mat[!is.finite(cor_mat)] <- 0

# ---------- 4) Threshold ----------
thr <- 0.70

cor_mat <- cor(t(pd_matrix), use = "pairwise.complete.obs")

# Apply threshold
cor_mat[abs(cor_mat) < thr] <- 0

# Remove negative weights – Louvain requires non-negative
cor_mat[cor_mat < 0] <- 0

# Remove NaN/Inf
cor_mat[!is.finite(cor_mat)] <- 0

# ---------- 5) Graph ----------
g <- graph_from_adjacency_matrix(cor_mat, mode = "undirected",
                                 weighted = TRUE, diag = FALSE)

g <- delete.vertices(g, degree(g) == 0)

# ---------- 6) Community detection ----------
comm <- cluster_louvain(g)
V(g)$community <- membership(comm)

# ---------- 7) Add attributes (country, sector, cluster) ----------
community_table <- data.frame(
  node = names(V(g)),
  community = as.integer(V(g)$community)
) %>%
  separate(node, into = c("country", "sector"), sep = "_", remove = FALSE)

cluster_map <- df %>%
  mutate(node = paste(country, gdesc, sep = "_")) %>%
  distinct(node, Cluster)

community_table <- community_table %>%
  left_join(cluster_map, by = "node")

# Write attributes into graph
V(g)$country <- community_table$country
V(g)$sector  <- community_table$sector
V(g)$Cluster <- dplyr::recode(as.character(community_table$Cluster),
                                  "1" = "Low risk",
                                  "2" = "High risk")

palette <- scales::hue_pal()(length(unique(V(g)$community)))
V(g)$color <- palette[V(g)$community]

write.csv(community_table,
          file.path(out_dir, "community_members.csv"),
          row.names = FALSE)

# ==========================================================
# 8) PLOTS (4 versioni pulite)
# ==========================================================

set.seed(123)

# 1) Basic Community Graph
p_comm <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.10, color = "grey70") +
  geom_node_point(aes(color = factor(community)), size = 4) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_color_manual(values = palette, name = "Community") +
  theme_void() +
  ggtitle("Correlation Network — Communities")

ggsave(file.path(out_dir, "network_1_communities.png"),
       p_comm, width = 12, height = 10, dpi = 150)


# 2) Community Graph + Country Labels
p_country <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.10, color = "grey70") +
  geom_node_point(aes(color = factor(community)), size = 4) +
  geom_node_text(aes(label = country), size = 3, alpha = 0.7, repel = TRUE) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_color_manual(values = palette, name = "Community") +
  theme_void() +
  ggtitle("Correlation Network — Country labels")

ggsave(file.path(out_dir, "network_2_countries.png"),
       p_country, width = 12, height = 10, dpi = 150)


# 3) Community Graph + Sector Labels
p_sector <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.10, color = "grey70") +
  geom_node_point(aes(color = factor(community)), size = 4) +
  geom_node_text(aes(label = sector), size = 3, alpha = 0.7, repel = TRUE) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_color_manual(values = palette, name = "Community") +
  theme_void() +
  ggtitle("Correlation Network — Sector labels")

ggsave(file.path(out_dir, "network_3_sectors.png"),
       p_sector, width = 12, height = 10, dpi = 150)

# 4) Risk Cluster Graph (High vs Low risk) – MODIFICATO
p_cluster <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.10, color = "grey75") +
  geom_node_point(aes(color = Cluster), size = 4) +
  # === MODIFICA QUI: Combina le etichette per il nodo ===
  geom_node_text(aes(label = paste(country, sector, sep = "-")), size = 2.5, alpha = 0.7, repel = TRUE) +
  # ======================================================
scale_color_manual(values = c("Low risk" = "#1f78b4",
                              "High risk" = "#e31a1c"),
                   drop = FALSE,
                   name = "Risk cluster") +
  scale_edge_width(range = c(0.1, 1)) +
  theme_void() +
  ggtitle("Correlation Network — Nodes labeled by Country-Sector")

ggsave(file.path(out_dir, "network_4_combined_labels.png"),
       p_cluster, width = 12, height = 10, dpi = 150)


print(p_comm)
print(p_sector)
print(p_cluster)
print(p_country)
# ==========================================================
# 5) NUOVO GRAFICO: Communities (Louvain) + Etichetta Paese-Settore
# ==========================================================

# Utilizza la struttura di p_comm, ma aggiunge l'etichetta combinata
# e aumenta le dimensioni/risoluzione per evitare sovrapposizioni.

p_comm_country_sector <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.10, color = "grey7") +
  geom_node_point(aes(color = factor(community)), size = 4) +
  
  # Etichetta combinata Paese-Settore
  geom_node_text(aes(label = paste(country, sector, sep = "_")), 
                 size = 2.5, # Dimensione del testo leggermente ridotta
                 alpha = 0.8, 
                 repel = TRUE,
                 max.overlaps = 50) + # Aumenta la tolleranza alle sovrapposizioni per visualizzare più etichette
  
  scale_edge_width(range = c(0.1, 1)) +
  scale_color_manual(values = palette, name = "Community") +
  theme_void() +
  ggtitle("Correlation Network — Communities (Labeled by Country-Sector)")

# Salva con dimensioni maggiori e alta risoluzione (DPI)
ggsave(file.path(out_dir, "network_5_comm_country_sector.png"),
       p_comm_country_sector, width = 16, height = 14, dpi = 300) # Aumentato da 12x10 a 16x14

print(p_comm_country_sector)