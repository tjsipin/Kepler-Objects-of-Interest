lr_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lr_fit <- lr_spec %>%
  fit(level ~ . - koi_score, data = train.koid.2)

# 