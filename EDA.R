# correlation
koid.2.corr <- koid.2 %>% dplyr::select(-c(koi_disposition, koi_pdisposition,level)) %>% correlate()

koid.2.corr

rplot(koid.2.corr)

# data exploration
summary(koid.2)

# graph of univariate, multivariate relationships between outcome and predictor(s) or between predictors

# histograms

ggplot(koi, aes(koi$koi_disposition)) + 
  geom_bar(stat = "count")

# make splits at 0.1 and 0.9 for koid.3?