# ==========================================================
# 09_dependencies_pca.R — PCA delle PD con cleaning robusto
# ==========================================================

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------- 1) Carica dati ----------
df <- read_parquet("01_data_clean/output_features_with_cluster.parquet")

stopifnot(all(c("country","gdesc","data_date","q_1y") %in% names(df)))

# ---------- 2) Matrice PD ----------
pd_mat <- df %>%
  group_by(country, gdesc, data_date) %>%
  summarise(PD = mean(q_1y, na.rm = TRUE), .groups = "drop") %>%
  unite("id", country, gdesc, remove = FALSE) %>%
  select(id, data_date, PD) %>%
  pivot_wider(names_from = data_date, values_from = PD)

# Estrai ID e matrice numerica
row_ids <- pd_mat$id
pd_mat <- pd_mat %>% select(-id)
pd_mat <- as.data.frame(pd_mat)
rownames(pd_mat) <- row_ids

# ---------- 3) Converti tutto in numeric ----------
pd_mat <- pd_mat %>% mutate(across(everything(), as.numeric))

# ---------- 4) Rimuovi colonne tutte NA ----------
all_na_cols <- sapply(pd_mat, function(x) all(is.na(x)))
pd_mat <- pd_mat[, !all_na_cols]

cat("Colonne rimosse (tutte NA):", sum(all_na_cols), "\n")

# ---------- 5) Rimuovi colonne con varianza zero ----------
zero_sd_cols <- sapply(pd_mat, function(col) {
  sd_val <- suppressWarnings(sd(col, na.rm = TRUE))
  if (is.na(sd_val)) return(TRUE)  # trattiamo NA sd come colonna da rimuovere
  return(sd_val == 0)
})

cat("Colonne rimosse (varianza zero):", sum(zero_sd_cols), "\n")

pd_mat <- pd_mat[, !zero_sd_cols]

# ---------- 6) Impute NA con media colonna (richiesto per PCA) ----------
for (j in seq_len(ncol(pd_mat))) {
  na_idx <- is.na(pd_mat[, j])
  if (any(na_idx)) {
    pd_mat[na_idx, j] <- mean(pd_mat[, j], na.rm = TRUE)
  }
}

# ---------- 7) PCA ----------
pca <- prcomp(pd_mat, scale. = TRUE)

print(summary(pca))

# ---------- 8) Plot Scree ----------
scree_plot <- ggplot(data.frame(PC = 1:length(pca$sdev),
                                Var = pca$sdev^2 / sum(pca$sdev^2)),
                     aes(x = PC, y = Var)) +
  geom_col(fill = "#1f78b4") +
  geom_line(aes(y = Var), color = "red") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "Scree Plot — PCA delle PD",
       x = "Principal Component",
       y = "Variance Explained")

ggsave("02_team_modules/D_Dependencies_Analysis/pca_scree.png",
       scree_plot, width = 7, height = 5, dpi = 150)

print(scree_plot)

# ---------- 9) PCA Biplot ----------
pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$id <- rownames(pca_df)
pca_df <- pca_df %>% separate(id, into = c("country","sector"), sep = "_")

biplot_pca <- ggplot(pca_df, aes(PC1, PC2, label = sector)) +
  geom_point(alpha = 0.6, color="#1f78b4") +
  ggrepel::geom_text_repel(size = 3) +
  theme_minimal() +
  labs(title = "PCA — Country–Sector PD Mapping",
       x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100,1),"%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100,1),"%)"))

ggsave("02_team_modules/D_Dependencies_Analysis/pca_biplot.png",
       biplot_pca, width = 9, height = 7, dpi = 150)

print(biplot_pca)