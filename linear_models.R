lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit <- lm_spec %>%
  fit(koi_score ~ . - level, data = train.koid.2)

lm_fit

lm_fit %>% summary()

# normal prediction
predict(lm_fit, new_data = test.koid.2)

# confidence interval
predict(lm_fit, new_data = test.koid.2, type = "conf_int")

# evaluate performance of model
bind_cols(predict(lm_fit, new_data = test.koid.2),
          test.koid.2) %>%
  dplyr::select(koi_score, .pred)

summ_lm_fit <- summary(lm_fit)


