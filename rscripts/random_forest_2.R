tree_prep <- prep(recipe)
juiced <- juice(tree_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(tune_spec)

set.seed(234)
rf_folds <- vfold_cv(train.koid.3)

doParallel::registerDoParallel()

set.seed(345)

tune_res <- tune_grid(
  tune_wf,
  resamples = rf_folds,
  grid = 20
)

tune_res
