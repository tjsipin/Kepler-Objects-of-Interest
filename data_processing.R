koi = read.csv("cumulative.csv")
koi <- dplyr::select(koi,-c(kepoi_name, koi_teq_err1, koi_teq_err2, rowid, kepid, kepler_name, koi_tce_delivname))



# data wrangle koi

koi = na.omit(koi)
koi = koi %>% dplyr::select(c(koi_disposition, koi_fpflag_nt, koi_fpflag_ss, 
                              koi_fpflag_co, koi_fpflag_ec, koi_period, koi_time0bk, koi_impact, 
                              koi_duration, koi_depth, koi_prad, koi_teq, koi_insol, koi_model_snr, 
                              koi_steff, koi_slogg, koi_srad, ra, dec, koi_kepmag)) %>%
  mutate(koi_disposition = as.character(koi_disposition)) %>%
  mutate(koi_fpflag_nt = factor(koi_fpflag_nt)) %>%
  mutate(koi_fpflag_ss = factor(koi_fpflag_ss)) %>%
  mutate(koi_fpflag_co = factor(koi_fpflag_co)) %>%
  mutate(koi_fpflag_ec = factor(koi_fpflag_ec))

# train and test split

train = sample(1:nrow(koi), nrow(koi) * .8)
train.koi = koi[train,]
test.koi = koi[-train,]


# 2 and 3 level factors for prediction level


koid.2 = koi %>%
  mutate(level = as.factor(ifelse(koi_score <= median(koi_score), "Low", "High")))

koid.2b = koi %>%
  mutate(level = as.factor(ifelse(koi_score <= median(koi_score), "Low", "High")))

koid.3 = koi %>%
  mutate(level = as.factor(ifelse(koi_score <= .1, "Low",
                                  ifelse(koi_score <= .9, "Medium",
                                         ifelse(koi_score > .9, "High", 0)))))
koid.3 = koid.3 %>% dplyr::select(c(level, koi_disposition, koi_pdisposition, koi_score, koi_fpflag_nt, koi_fpflag_ss, koi_fpflag_co, koi_fpflag_ec, koi_period, koi_time0bk, koi_impact, koi_duration, koi_depth, koi_prad, koi_teq, koi_insol, koi_model_snr, koi_tce_plnt_num, koi_steff, koi_slogg, koi_srad, ra, dec, koi_kepmag))


# train and test split koid.3

train.3 = sample(1:nrow(koid.3), nrow(koid.3) * .8)
train.koid.3 = koid.3[train.3,]
test.koid.3 = koid.3[-train.3,]

# modifying koid.2
koid.2 = koid.2 %>% dplyr::select(c(level, koi_disposition, koi_pdisposition, koi_score, koi_fpflag_nt, koi_fpflag_ss, koi_fpflag_co, koi_fpflag_ec, koi_period, koi_time0bk, koi_impact, koi_duration, koi_depth, koi_prad, koi_teq, koi_insol, koi_model_snr, koi_tce_plnt_num, koi_steff, koi_slogg, koi_srad, ra, dec, koi_kepmag))

# train and test split

train = sample(1:nrow(koid.2), nrow(koid.2) * .8)
train.koid.2 = koid.2[train,]
test.koid.2 = koid.2[-train,]
