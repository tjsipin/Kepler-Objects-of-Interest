rf_model <-
  rand_forest(min_n = tune(),
              mtry = tune(),
              mode = "classification") %>%
  set_engine("ranger")


rf_workflow <- workflow() %>%
  add_model(rf_model)  %>%
  add_recipe(recipe)

rf_params <- parameters(rf_model) %>%
  update(mtry = mtry(range = c(2, 12)))


rf_grid <- grid_regular(rf_params,
                        levels = 5)

rf_grid%>%
  count(tree_depth)

rf_res <- rf_workflow %>%
  tune_grid(resamples = koi_folds,
            grid = rf_grid)



rf_tune <- rf_workflow %>%
  tune_grid(resamples = koi_folds,
            grid = rf_grid)

save(rf_tune, rf_workflow, file = "rf_tune.rda")

tune_grid(rf_model,
          recipe,
          koi_folds,
          rf_grid)

autoplot(rf_tune)

show_best(rf_tune, metric = "accuracy") %>% dplyr::select(-.estimator, -.config)

# best tree mtry = 7, min_n = 2. negligible difference in roc_auc and mean accuracy.


