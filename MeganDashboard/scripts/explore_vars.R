library(ggplot2)
library(sf)
library(readr)
library(corrplot)
library(dplyr)

df <- readRDS("MeganDashboard/data/df.cont.inequity.compo.coastal.scores_sr.rds")
df <- st_drop_geometry(df)

df_clean <- df |>
  st_drop_geometry() |>
  select(where(is.numeric))

df_clean[!is.finite(as.matrix(df_clean))] <- NA
df_clean <- na.omit(df_clean)

cor_matrix <- cor(df_numeric, use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

pca <- prcomp(df_clean, scale. = TRUE)
summary(pca)

scores <- as.data.frame(pca$x)

ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PCA: Country Scores in PC Space")

