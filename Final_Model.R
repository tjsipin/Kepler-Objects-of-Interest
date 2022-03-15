# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


rf_workflow_tuned <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune))

rf_results <- fit(rf_workflow_tuned, koi_train)

rf_results_test <- fit(rf_workflow_tuned, koi_test)

koi_train$koi_disposition <- as.factor(koi_train$koi_disposition)
koi_test$koi_disposition <- as.factor(koi_test$koi_disposition)

final_model <- 
  rand_forest(min_n = 2,
              mtry = 7,
              mode = "classification") %>%
  set_engine("ranger") %>%
  fit(koi_disposition ~ ., data = koi_train)

final_model %>%
  predict(koi_test) %>%
  bind_cols(koi_test) %>%
  metrics(truth = koi_disposition, estimate = .pred_class)

# Per classifier metrics

koi_probs <- final_model %>%
  predict(koi_test, type = "prob") %>%
  bind_cols(koi_test)

glimpse(koi_probs)

# gain curve plot
koi_probs %>%
  gain_curve(koi_disposition, .pred_CANDIDATE:`.pred_FALSE POSITIVE`) %>%
  autoplot()

# if we target 25% of observations with highest probability of being CANDIDATE,
# we will get ~80% of all possible CANDIDATE observations

# roc curve plot
koi_probs %>%
  roc_curve(koi_disposition, .pred_CANDIDATE:`.pred_FALSE POSITIVE`) %>%
  autoplot()

