library(devtools)
install_github("vqv/ggbiplot", force = T)
library(ggbiplot)

koi_pca = koi %>% dplyr::select(-koi_disposition)

pr.out = prcomp(koi[6:20], scale=T)
summary(pr.out)

dim(pr.out$x)
dim(koi)


pca_trans <- recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric())

pca_estimates <- prep(pca_trans)
names(pca_estimates)
pca_estimates$var_info
pca_estimates$steps[[1]]

sdev <- pca_estimates$steps[[7]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

var_df <- data.frame(PC=paste0("PC", 1:length(sdev)),
                     var_explained=percent_variation,
                     stringsAsFactors = FALSE)
var_df %>% 
  mutate(PC = fct_inorder(PC)) %>%
  ggplot(aes(x=PC, y=var_explained)) + 
  geom_col()

var_df

juice(pca_estimates)

# Cumulative sum of explained variance

plot(cumsum(percent_variation), xlab = "Principal Component",
     ylab = "Proportion of Variation Explained", 
     ylim = c(0,1), type = 'b') %>%
  abline(h=.9, col = 2) %>% 
  grid(col = "lightgray", lty = "dotted")


# Biplot of PC1 vs PC2

plot(x = pr.out$x[, c(1, 2)][,1],
     y = pr.out$x[, c(1, 2)][,2],
     col = 1:3,
     cex = 0.3)

# 
koi.dist <- dist(koi)
koi.hclust <- hclust(koi.dist)
dend3 = as.dendrogram(koi.hclust)
dend3 = color_branches(dend3, k = 3)
dend3 = color_labels(dend3, k = 3)

dend3 = set(dend3, "labels_cex", 0.3)

dend3 = set_labels(dend3, labels = koi$koi_disposition[order.dendrogram(dend3)])

# Plot dendogram
plot(dend3, horiz = T)
     