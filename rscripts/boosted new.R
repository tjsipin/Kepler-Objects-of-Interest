# Creating model

boost_model <- boost_tree(min_n= tune(), 
                          mtry =tune(), 
                          learn_rate= tune(),
                          mode = "classification") %>%
  set_engine("xgboost")

boost_wflow <-
  workflow() %>%
  add_recipe(recipe) %>% 
  add_model(boost_model)

# Trying different parameters (mtry from 2 to 12, same as rf)

boost_params <- parameters(boost_model) %>%
  update(mtry = mtry(range = c(2, 12)))

# Define grid

boost_grid <- grid_regular(boost_params,
                           levels=3
)

# Tuning model

boost_tune <- boost_wflow %>%
  tune_grid(resamples = koi_folds,
            grid = boost_grid)

# tune_grid(boost_model,
#           recipe,
#           koi_folds,
#           boost_grid)

autoplot(boost_tune)

# Boost results for metrics

boost_res <- 
  boost_wflow %>% 
  # fit_resamples(
  #   resamples = koi_folds,
  #   metrics = metric_set(
  #     recall, precision, f_meas,
  #     accuracy, kap,
  #     roc_auc, sens, spec),
  #   control = control_resamples(save_pred = TRUE)
  # )
  tune_grid(resamples = koi_folds,
            grid = boost_grid,
            metrics = metric_set(
              recall, precision, f_meas,
              accuracy, kap,
              roc_auc, sens, spec
            ))

boost_res %>% collect_metrics(summarize = TRUE)
