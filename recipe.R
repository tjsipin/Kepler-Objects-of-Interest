# Recipe
recipe <- recipe(
  koi_disposition ~ koi_fpflag_nt + koi_fpflag_ss + koi_fpflag_co +
    koi_fpflag_ec + koi_period + koi_time0bk + koi_impact + koi_duration + koi_depth + koi_prad + 
    koi_teq + koi_insol + koi_model_snr + koi_steff + koi_slogg + koi_srad + 
    ra + dec + koi_kepmag, data = train.koi) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_predictors())


# CV train and test split
koi_split <- initial_split(koi,
                           strata = koi_disposition)
koi_train <- training(koi_split)
koi_test <- testing(koi_split)

# CV Folds 

koi_folds <- vfold_cv(koi_train)
train_folds <- vfold_cv(koi_train, v = 10)
save(train_folds, recipe, file = "model_setup.rda")
