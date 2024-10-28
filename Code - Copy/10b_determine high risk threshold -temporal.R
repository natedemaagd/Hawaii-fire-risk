
# in script 10a, fire risk threshold was determined by looking at probabilities within fire perimeters on each island.
# Here, we look at temporal risk: what is the risk in months without fire vs months with fire?

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
ggsave('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/10b_regress_burnArea_onto_numHighRiskPixels.png')




##### save data #####

save(dat_fire, dat_fire2, highRiskThresholds, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10a_determine high risk threshold.Rdata')
