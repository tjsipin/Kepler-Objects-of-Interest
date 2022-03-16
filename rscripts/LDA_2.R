library(MASS)

lda.fit <- lda(koi_disposition ~ koi_fpflag_nt + koi_fpflag_ss + koi_fpflag_co +
                 koi_fpflag_ec + koi_period + koi_time0bk + koi_impact + koi_duration + koi_depth + koi_prad + 
                 koi_teq + koi_insol + koi_model_snr + koi_steff + koi_slogg + koi_srad + 
                 ra + dec + koi_kepmag, data = koi_train)
lda.fit

plot(lda.fit)

lda.pred <- predict(lda.fit, koi_train)

lda.class <- lda.pred$class

table(lda.class, koi_train$koi_disposition)

mean(lda.class == koi_train$koi_disposition)

sum(lda.pred$posterior >= .5)
