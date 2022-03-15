#Boosting using class based method
library(gbm)
set.seed(1)
boost.koi = gbm(koi_disposition ~., data=koi_train, 
                distribution="multinomial", n.trees=500, interaction.depth=4, shrinkage = 0.01)

#plot and relative influence statistics
summary(boost.koi) 

#Another way
recipe <- recipe(
  koi_disposition ~ koi_fpflag_nt + koi_fpflag_ss + koi_fpflag_co +
    koi_fpflag_ec + koi_period + koi_time0bk + koi_impact + koi_duration + koi_depth + koi_prad + 
    koi_teq + koi_insol + koi_model_snr + koi_steff + koi_slogg + koi_srad + 
    ra + dec + koi_kepmag, data = koi_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_predictors())

#creating the model
boost_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

#creating the workflow
boost_wflow <-
  workflow() %>%
  add_recipe(recipe) %>% 
  add_model(boost_spec)

#getting the res
boost_res <- 
  boost_wflow %>% 
  fit_resamples(
    resamples = koi_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

#collecting metric
boost_res %>% collect_metrics(summarize = TRUE)
