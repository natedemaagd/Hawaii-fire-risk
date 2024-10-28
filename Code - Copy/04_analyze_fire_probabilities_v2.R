library(caret); library(ggplot2)

load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
island_name <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')

# create sequence of thresholds
thresholds <- seq(0,1,0.001)




##### ROC curve for all islands #####

# create list of eval models
eval_models <- list(evalbi, evaloa, evalka, evalmn)
names(eval_models) <- island

for(i in 1:length(island)){
  
  # for each threshold, calculate sensitivity and specificity
  roc_dat <- data.frame(sensitivity = rep(NA, times = length(thresholds)),
                        specificity = rep(NA, times = length(thresholds)))
  for(j in 1:length(thresholds)){
    predicted_values <- ifelse(eval_models[[i]][[2]]$probs$Group1$B > thresholds[[j]], 1, 0)
    eval_models[[i]][[2]]$probs$Group1$obs_dummy <- ifelse(eval_models[[i]][[2]]$probs$Group1$obs == 'B', 1, 0)  # create dummy variable for burned
    actual_values <- eval_models[[i]][[2]]$probs$Group1$obs_dummy
    conf_matrix <- table(predicted_values, actual_values)
    if(nrow(conf_matrix) == 2){
      roc_dat[j,] <- c(sensitivity(conf_matrix), specificity(conf_matrix))  # for some thresholds at ends, may not get matrix with 2 rows/columns, so skip those
    }
  }
  
  # 1-specificity (false positives)
  roc_dat$one_minus_specificity <- 1-roc_dat$specificity
  
  # find optimal/unbiased threshold: maximize sum of sensitivity and specificity
  roc_dat$sum_SensSpec <- roc_dat$sensitivity + roc_dat$specificity
  roc_dat$optimal_threshold <- 0
  roc_dat[which.max(roc_dat$sum_SensSpec), 'optimal_threshold'] <- 1
  
  # add threshold values, rearrange columns, and save
  roc_dat$threshold <- thresholds
  roc_dat <- roc_dat[c('threshold', 'sensitivity', 'specificity', 'one_minus_specificity', 'sum_SensSpec', 'optimal_threshold')]
  write.csv(roc_dat, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_prob_thresholds/ROC curves/ROC_dat_', island[[i]], '.csv'))
  
  
  # plot
  ggplot(data = roc_dat) +
    geom_line(aes(x = one_minus_specificity, y= sensitivity)) +
    geom_abline(slope = 1, intercept = 0, linetype ='longdash') +
    labs(x = '1 - specificity', y = 'Sensitivity')
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_prob_thresholds/ROC curves/ROC_curve_', island[[i]], '.png'))
  
  gc()
}
