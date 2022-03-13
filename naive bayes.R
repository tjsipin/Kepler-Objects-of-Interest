nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR") %>%
  set_args(usekernel = TRUE)

# Error: zero variance when usekernel = FALSE
# Warning: Numerical 0 probability for all observations
nb_fit <- nb_spec %>%
  fit(level ~ . - koi_score, data = train.koid.2)
augment(nb_fit, new_data = test.koid.2) %>%
  conf_mat(truth = level, estimate = .pred_class)

augment(nb_fit, new_data = test.koid.2) %>%
  accuracy(truth = level, estimate = .pred_class)


#### koid.3
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR") %>%
  set_args(usekernel = TRUE)

# Error: zero variance when usekernel = FALSE
# Warning: Numerical 0 probability for all observations
nb_fit <- nb_spec %>%
  fit(level ~ . - koi_score, data = train.koid.3)
augment(nb_fit, new_data = test.koid.3) %>%
  conf_mat(truth = level, estimate = .pred_class)

augment(nb_fit, new_data = test.koid.3) %>%
  accuracy(truth = level, estimate = .pred_class)