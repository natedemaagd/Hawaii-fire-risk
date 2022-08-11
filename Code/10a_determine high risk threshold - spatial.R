
library(raster); library(ggplot2)

# load fire data
dat_fire <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_pixel_data.rds')
dat_fire2 <- dat_fire

# create data.frame from dat_fire
dat_fire <- lapply(dat_fire, function(x){
  x$year <- rep(substr(x$yearmonth, 1, 4), times = length(x$fire_values))
  x$island <- rep(x$island, times = length(x$fire_values))
  x
})

for(i in 1:length(dat_fire)){
  dat_fire[[i]]$fireID <- rep(i, times = length(dat_fire[[i]]$fire_values))
  dat_fire[[i]] <- dat_fire[[i]][c('fire_values', 'island', 'year', 'fireID')]
}

dat_fire <- lapply(dat_fire, function(x){
  df <- data.frame(fire_values = x$fire_values,
                   island = x$island,
                   year = x$year,
                   fireID = x$fireID)
  
  df
})

dat_fire <- do.call(rbind, dat_fire)
rm(i)




##### summarize fire vs. non-fire probabilities #####

# combine values from each fire
dat_fire2 <- lapply(dat_fire2, function(x){
  df <- data.frame(prob_burned = c(x$fire_values, x$sample_nonfire_vals),
                   status = c(rep('Burned', times = length(x$fire_values)),
                              rep('Unburned', times = length(x$sample_nonfire_vals))),
                   island = x$island,
                   year   = substr(x$yearmonth, 1, 4))
  df
})
dat_fire2 <- do.call(rbind, dat_fire2)

dat_fire_summary <- aggregate(dat_fire2$prob_burned, list(dat_fire2$island, dat_fire2$status), summary)
summary(dat_fire2[dat_fire2$status == 'Unburned', 'values'])
summary(dat_fire2[dat_fire2$status == 'Burned',   'values'])

# ggplot(dat_fire2) +
#   geom_density(aes(values, color = status, fill = status))




##### determine high fire risk threshold - 25th, 50th, and 75th percentile of burned pixels #####

# define high-risk as function of fire probability within burned pixels, by island

# get 25th percentile probability for burned pixels by island
highRiskThresholds <- aggregate(dat_fire2[dat_fire2$status == 'Burned', 'prob_burned'], list(dat_fire2[dat_fire2$status == 'Burned', 'island']), function(x) quantile(x, probs = 0.25, na.rm = TRUE))
colnames(highRiskThresholds) <- c('island', 'hiRiskThresh_25pctileBurned')

# get 50th percentile fire probability for burned pixels by island
highRiskThresholds$hiRiskThresh_50pctileBurned <- aggregate(dat_fire2[dat_fire2$status == 'Burned', 'prob_burned'], list(dat_fire2[dat_fire2$status == 'Burned', 'island']), median, na.rm = TRUE)[,2]

# get 75th percentile fire probability for burned pixels by island
highRiskThresholds$hiRiskThresh_75pctileBurned <- aggregate(dat_fire2[dat_fire2$status == 'Burned', 'prob_burned'], list(dat_fire2[dat_fire2$status == 'Burned', 'island']), function(x) quantile(x, probs = 0.75, na.rm = TRUE))[,2]




##### determine high fire risk threshold - from eval models #####

# load eval models
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

# add cutoff determined by max MCC
highRiskThresholds$hiRiskThresh_MCC <- c(evalbi[[2]]$probs$Group1[which.max(evalbi[[2]]$probs$Group1$MCC), 'B'],
                                         evalka[[2]]$probs$Group1[which.max(evalka[[2]]$probs$Group1$MCC), 'B'],
                                         evalmn[[2]]$probs$Group1[which.max(evalmn[[2]]$probs$Group1$MCC), 'B'],
                                         evaloa[[2]]$probs$Group1[which.max(evaloa[[2]]$probs$Group1$MCC), 'B'])

# add cutoff determined by max F1
highRiskThresholds$hiRiskThresh_F1  <- c(evalbi[[2]]$probs$Group1[which.max(evalbi[[2]]$probs$Group1$F1), 'B'],
                                         evalka[[2]]$probs$Group1[which.max(evalka[[2]]$probs$Group1$F1), 'B'],
                                         evalmn[[2]]$probs$Group1[which.max(evalmn[[2]]$probs$Group1$F1), 'B'],
                                         evaloa[[2]]$probs$Group1[which.max(evaloa[[2]]$probs$Group1$F1), 'B'])

# add cutoff determined by max Informedness
highRiskThresholds$hiRiskThresh_Informedness <- c(evalbi[[2]]$probs$Group1[which.max(evalbi[[2]]$probs$Group1$Informedness), 'B'],
                                                  evalka[[2]]$probs$Group1[which.max(evalka[[2]]$probs$Group1$Informedness), 'B'],
                                                  evalmn[[2]]$probs$Group1[which.max(evalmn[[2]]$probs$Group1$Informedness), 'B'],
                                                  evaloa[[2]]$probs$Group1[which.max(evaloa[[2]]$probs$Group1$Informedness), 'B'])

# merge all high risk thresholds to fire data and create high-risk dummies
dat_fire2 <- dplyr::left_join(dat_fire2, highRiskThresholds, 'island')
dat_fire2$fireHighRisk_50pctileBurned <- ifelse(dat_fire2$prob_burned > dat_fire2$hiRiskThresh_50pctileBurned, 1, 0)
dat_fire2$fireHighRisk_MCC            <- ifelse(dat_fire2$prob_burned > dat_fire2$hiRiskThresh_MCC,            1, 0)
dat_fire2$fireHighRisk_F1             <- ifelse(dat_fire2$prob_burned > dat_fire2$hiRiskThresh_F1,             1, 0)
dat_fire2$fireHighRisk_Informedness   <- ifelse(dat_fire2$prob_burned > dat_fire2$hiRiskThresh_Informedness,   1, 0)




##### for each threshold type, find percentile of that high-risk probability w/in burned pixels #####

# initiate dataframe
threshold_percentiles_within_burned_pixels <- data.frame(expand.grid(list(island = c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu'),
                                                                          stat   = c('hiRiskThresh_25pctileBurned', 'hiRiskThresh_50pctileBurned', 'hiRiskThresh_75pctileBurned', 'hiRiskThresh_MCC', 'hiRiskThresh_F1', 'hiRiskThresh_Informedness'))))

# for each row (island-statistic combo), determine the percentile of the threshold value within that island's burned pixels
threshold_percentiles_within_burned_pixels$pctile <- NA
for(i in 1:nrow(threshold_percentiles_within_burned_pixels)){
  
  # get all burned pixels' probabilities in that island
  prob_vec = dat_fire2[dat_fire2$island == as.character(threshold_percentiles_within_burned_pixels$island[[i]]) & dat_fire2$status == 'Burned', 'prob_burned']
  
  # get appropriate threshold value
  threshold = highRiskThresholds[highRiskThresholds$island == as.character(threshold_percentiles_within_burned_pixels$island[[i]]),
                                 as.character(threshold_percentiles_within_burned_pixels$stat[[i]])]
  
  # get percentile of threshold value within the probability vector
  pctile = ecdf(prob_vec)(threshold)
  
  # get percentile of threshold value i within `prob_vec`
  threshold_percentiles_within_burned_pixels$pctile[[i]] <- ecdf(prob_vec)(threshold)
}
















##### regress burn area onto number of high-risk pixels #####

# by island and year, count number of burned pixels
pixels_burned <- aggregate(dat_fire2[dat_fire2$status == 'Burned', 'status'], list(dat_fire2[dat_fire2$status == 'Burned', 'island'], dat_fire2[dat_fire2$status == 'Burned', 'year']), length)
colnames(pixels_burned) <- c('island', 'year', 'num_burned_pixels')

# by island and year, count number of high-risk pixels
pixels_highRisk <- aggregate(dat_fire2[dat_fire2$fire_highRisk == 1, 'fire_highRisk'], list(dat_fire2[dat_fire2$fire_highRisk == 1, 'island'], dat_fire2[dat_fire2$fire_highRisk == 1, 'year']), length)
colnames(pixels_highRisk) <- c('island', 'year', 'num_highRisk_pixels')

# merge data
dat_highRisk_burned <- dplyr::left_join(pixels_burned, pixels_highRisk, by = c('island', 'year'))
rm(pixels_burned, pixels_highRisk)

# HAWAII ISLAND 2005 LARGE NUMBER OF BURNED PIXELS AND HIGH-RISK PIXELS
dat_highRisk_burned <- dat_highRisk_burned[!(dat_highRisk_burned$island == 'Hawaii' & dat_highRisk_burned$year == '2005'),]
dat_highRisk_burned <- dat_highRisk_burned[complete.cases(dat_highRisk_burned),]

# regressions
reg_highRisk_burned1 <- lm(num_burned_pixels ~ num_highRisk_pixels, data = dat_highRisk_burned)
summary(reg_highRisk_burned1)

reg_highRisk_burned2 <- lm(num_burned_pixels ~ num_highRisk_pixels + island, data = dat_highRisk_burned)
summary(reg_highRisk_burned2)

reg_highRisk_burned3 <- lm(num_burned_pixels ~ num_highRisk_pixels + island + num_highRisk_pixels*island, data = dat_highRisk_burned)
summary(reg_highRisk_burned3)

# plot
ggplot(data = dat_highRisk_burned, aes(x = num_highRisk_pixels, y = num_burned_pixels)) +
  #geom_point(aes(color = year, shape = island)) +
  geom_point(aes(shape = island)) +
  labs(x = 'Number of high-risk pixels', y = 'Number of burned pixels', shape = 'Island') +
  geom_smooth(method = "lm", se = TRUE, color = 'black') +
  annotate(geom = 'text', x = 30000, y = 10000, label = paste0('Correlation = ', round(cor(dat_highRisk_burned$num_highRisk_pixels,
                                                                                           dat_highRisk_burned$num_burned_pixels), 2))) +
  theme(text=element_text(size=20))
ggsave('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/10_regress_burnArea_onto_numHighRiskPixels.png')




##### save data #####

save(dat_fire, dat_fire2, highRiskThresholds, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata')
