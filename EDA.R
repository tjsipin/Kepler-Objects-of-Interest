# correlation
koi.corr <- koi %>%
  dplyr::select(-c(koi_disposition, koi_fpflag_co, koi_fpflag_ec, koi_fpflag_nt, koi_fpflag_ss)) %>%
  correlate()

koi.corr

rplot(koi.corr)

# data exploration
summary(koi)

# graph of univariate, multivariate relationships between outcome and predictor(s) or between predictors

# histograms

ggplot(koi, aes(koi$koi_disposition)) + 
  geom_bar(stat = "count")

# qq plot?