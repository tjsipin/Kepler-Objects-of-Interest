# lab04

XTrain = train.koi %>% dplyr::select(-koi_disposition)
YTrain = train.koi$koi_disposition
XTest = test.koi %>% dplyr::select(-koi_disposition)
YTest = test.koi$koi_disposition

do.chunk <- function(chunkid, folddef, Xdat, Ydat, ...) {
  # Get training index
  train = (folddef!=chunkid)
  
  # Get training set by the above index
  Xtr = Xdat[train,]
  Ytr = Ydat[train]
  
  # Get validation set
  Xvl = Xdat[!train,]
  
  # Get responses in validation set
  Yvl = Ydat[!train]
  
  # Predict training labels``
  predYtr = knn(train=Xtr, test=Xtr, cl=Ytr, ...)
  
  # Predict validation labels
  predYvl = knn(train=Xtr, test=Xvl, cl=Ytr, ...)
  
  data.frame(fold = chunkid,
             train.error = mean(predYtr != Ytr), # Training error for each fold
             val.error = mean(predYvl != Yvl))
}

# Set 3-fold CV

nfold = 3

set.seed(123)
folds = cut(1:nrow(train.koi), breaks=nfold, labels=F) %>% sample()
folds


# Set error.folds as a vector to save validation errors in future
error.folds = NULL

# Give possible number of nearest neighbors to be considered
allK = 1:50

set.seed(234)

# Loop through different number of neighbors
for(k in allK) {
  # Loop through different chunk id
  for (j in seq(3)) {
    tmp = do.chunk(chunkid = j, folddef = folds, Xdat = XTrain, Ydat = YTrain, k = k)
    
    tmp$neighbors = k # Records last number of neighbor
    
    error.folds = rbind(error.folds, tmp) # combines results
  }
}

head(error.folds, 10)

# Transform the format of error.folds for further convenience
errors = melt(error.folds, id.vars = c('fold', 'neighbors'), value.name = 'error')

# Choose the number of neighbors which minimizes validation error
val.error.means = errors %>%
  # Select all rows of validation errors
  filter(variable=='val.error') %>%
  # Group the selected data frame by neighbors
  group_by(neighbors, variable) %>%
  # Calculate CV error rate for each k
  summarise_each(funs(mean), error) %>%
  # Remove existing group
  ungroup() %>%
  filter(error==min(error))

# Best num of neighbors (if tie, pick larger number of neighbors for simpler model)

bestneighbor = max(val.error.means$neighbors)
bestneighbor

set.seed(345)
pred.YTest = knn(train = XTrain, test = XTest, cl = YTrain, k = bestneighbor)

# Confusion matrix
conf.matrix = table(predicted = pred.YTest, true = YTest)

# Test error rate
1 - sum(diag(conf.matrix)/sum(conf.matrix))

# Plot errors
ggplot(errors, aes(x = neighbors, y = error, color = variable)) + 
  geom_line(aes(group = interaction(variable,fold))) +
  stat_summary(aes(group = variable), fun = "mean", geom = "line", size = 3) +
  geom_vline(aes(xintercept = bestneighbor), linetype = "dashed")
