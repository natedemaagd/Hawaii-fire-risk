
# This script examines current fire risk anomalies (north shore Molokai and
# Puu Kukui Maui)

library(raster)
library(ggplot2)




##### load data #####

# load landcover rasters
ras_bare <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_bare_2016.tif")
ras_herb <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_herb_2016.tif")
ras_wood <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_wood_2016.tif")

# load fire risk rasters
ras_riskDry <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_dry_MauiCounty.tif")
ras_riskWet <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_wet_MauiCOunty.tif")




##### format data #####

# crop landcover rasters to Maui only
ras_bare <- crop(ras_bare, ras_riskDry)
ras_herb <- crop(ras_herb, ras_riskDry)
ras_wood <- crop(ras_wood, ras_riskDry)
gc()

# convert to data.frame
dat <- as.data.frame(ras_bare, xy = TRUE)
colnames(dat) <- c('x', 'y', 'bare')
dat$herb <- values(ras_herb)
dat$wood <- values(ras_wood)
dat$riskDry <- values(ras_riskDry)
dat$riskWet <- values(ras_riskWet)
dat <- dat[complete.cases(dat),]
rm(ras_bare, ras_herb, ras_wood, ras_riskDry, ras_riskWet); gc()

# cut landcover data into deciles
dat$bare_deciles <- cut(dat$bare, breaks = 10)
dat$herb_deciles <- cut(dat$herb, breaks = 10)
dat$wood_deciles <- cut(dat$wood, breaks = 10)
gc()




##### plots #####

ggplot(data = dat[dat$riskDry < 0.02 & dat$riskDry > 0.005,], aes(x = bare_deciles, y = riskDry)) +
  geom_violin() +
  labs(x = 'Proportion bare ground', y = 'Fire risk') +
  theme(text = element_text(size = 14))

ggplot(data = dat[dat$riskDry < 0.02 & dat$riskDry > 0.005,], aes(x = herb_deciles, y = riskDry)) +
  geom_violin() +
  labs(x = 'Proportion herbaceous cover', y = 'Fire risk') +
  theme(text = element_text(size = 14))

ggplot(data = dat[dat$riskDry < 0.02 & dat$riskDry > 0.005,], aes(x = wood_deciles, y = riskDry)) +
  geom_violin() +
  labs(x = 'Proportion woody cover', y = 'Fire risk') +
  theme(text = element_text(size = 14))


