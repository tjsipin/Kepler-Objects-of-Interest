lda_spec <- discrim_linear() %>% 
  set_mode("classification") %>%
  set_engine("MASS")

lda_fit <- lda_spec %>% 
  fit(level ~ . - koi_score, data = train.koid.2)

lda_fit

predict(lda_fit, new_data = test.koid.2)
predict(lda_fit, new_data = test.koid.2, type="prob")

# set up cv
set.seed(123)
folds <- vfold_cv(train.koid.2, v =10)
lda_wf <- workflow() %>%
  add_model(lda_spec) %>%
  add_formula(level ~ . - koi_score)

set.seed(456)
lda_fit_rs <- lda_wf %>%
  fit_resamples(folds)
collect_metrics(lda_fit_rs)


# performance cv
augment(lda_fit, new_data = test.koid.2) %>%
  conf_mat(truth = level, estimate = .pred_class)

augment(lda_fit, new_data = test.koid.2) %>%
  accuracy(truth = level, estimate = .pred_class)


#### koid.3

lda_spec <- discrim_linear() %>% 
  set_mode("classification") %>%
  set_engine("MASS")

lda_fit <- lda_spec %>% 
  fit(level ~ . - koi_score, data = train.koid.3)

lda_fit

predict(lda_fit, new_data = test.koid.3)
predict(lda_fit, new_data = test.koid.3, type="prob")

# performance
augment(lda_fit, new_data = test.koid.3) %>%
  conf_mat(truth = level, estimate = .pred_class)

augment(lda_fit, new_data = test.koid.3) %>%
  accuracy(truth = level, estimate = .pred_class)






