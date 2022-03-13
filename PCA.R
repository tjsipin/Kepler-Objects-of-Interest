koi_pca = koi %>% dplyr::select(-koi_disposition)

pr.out = prcomp(koi[6:20], scale=T)
pr.out$center
pr.out$rotation
dim(pr.out$x)
dim(koi)
biplot(pr.out, scale = 0)




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

juice(pca_estimates)

plot(cumsum(percent_variation), xlab = "Principal Component",
     ylab = "Proportion of Variation Explained", 
     ylim = c(0,1), type = 'b') %>%
  abline(h=.9, col = 2) %>% 
  grid(col = "lightgray", lty = "dotted")


