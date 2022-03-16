set.seed(123)

tune_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tune_spec


tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid

set.seed(234)

# set koi_disposition as factor
as.factor(koi$koi_disposition)

set.seed(345)
tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(recipe)

tree_res <- tree_wf %>%
  tune_grid(resamples = koi_folds,
            grid = tree_grid)

tree_res 

tree_res %>%
  collect_metrics()

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) + 
  geom_line(size = 1.5, alpha = 0.6) + 
  geom_point(size = 2) + 
  facet_wrap(~ .metric, scales = "free", nrow = 2) + 
  scale_x_log10(labels = scales::label_number()) + 
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tree_res %>%
  show_best("accuracy")

best_tree <- tree_res %>%
  select_best("accuracy")

best_tree # best tree has tree depth of 8


final_tree_wf <- tree_wf %>%
  finalize_workflow(best_tree)

final_tree_wf  

final_tree_fit <-
  final_tree_wf %>%
  last_fit(koi_split)

final_tree_fit %>%
  collect_metrics()

