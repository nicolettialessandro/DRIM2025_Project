# ============================
# 1. Load data & select variables
# ============================
library(dplyr)
library(arrow)
library(corrplot)
library(car)
library(ggplot2)
library(tidyr)

df <- read_parquet("02_team_modules/D_Dependencies_Macro/panel_df.parquet")

macro_vars <- df %>%
  select(unemployment_rate, hicp_yoy, yield_10y, credit_spread_bp, gdp, gpr) %>%
  na.omit()

# ============================
# 2. Correlation matrix + save
# ============================
cor_mat <- cor(macro_vars)
corrplot(cor_mat, method="color", addCoef.col="black",
         tl.cex=0.8, number.cex=0.7)

write.csv(cor_mat, "correlation_matrix.csv", row.names = TRUE)

# ============================
# 3. VIF (multicollinearity test)
# ============================
lm_test <- lm(
  gpr ~ unemployment_rate + hicp_yoy + yield_10y + credit_spread_bp + gdp,
  data = macro_vars
)

vif_values <- vif(lm_test)
print(vif_values)
write.csv(vif_values, "vif_values.csv")

# ============================
# 4. PCA
# ============================
pca <- prcomp(macro_vars, scale. = TRUE)

summary(pca)    # variance explained
plot(pca, type="l")  # scree plot

# Save loadings
write.csv(round(pca$rotation,3), "pca_loadings.csv")

# ============================
# 5. Loadings Plot (PC1 & PC2)
# ============================

loadings <- as.data.frame(pca$rotation[,1:2]) %>%
  mutate(variable = rownames(.))

colnames(loadings) <- c("PC1","PC2","variable")

loadings_long <- loadings %>%
  pivot_longer(cols=c("PC1","PC2"),
               names_to="PC",
               values_to="loading")

loadings_long$variable <- factor(
  loadings_long$variable,
  levels = loadings$variable[order(loadings$PC1, decreasing = TRUE)]
)

p_pca_loadings <- ggplot(loadings_long, aes(x=variable, y=loading, fill=PC)) +
  geom_col(position=position_dodge(width=0.7), width=0.6) +
  coord_flip() +
  scale_fill_manual(values=c("PC1"="#1f78b4","PC2"="#33a02c")) +
  theme_minimal(base_size=13) +
  labs(
    title = "PCA Loadings: Contribution to PC1 and PC2",
    subtitle = paste0("PC1 + PC2 explain ~",
                      round(sum(summary(pca)$importance[2,1:2])*100,1),
                      "% of macro variation"),
    x = "Variable",
    y = "Loading value",
    fill = "Principal Component"
  )

print(p_pca_loadings)

ggsave("pca_loadings_plot.png",
       p_pca_loadings, width=10, height=6, dpi=300)