
# this script creates individual monthly dummy rasters for low, moderate, and high probability threshold for Hawaii Island
# Didn't fit in 13a because of memory issues

library(raster)

# load thresholds
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata')
rm(dat_fire, dat_fire2)

# define islands
vec_islands <- 'Hawaii'

# list files for island i
list_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
                             pattern = vec_islands[[1]])

# load island i raster m, and determine if each threshold was passed in every pixel
for(m in 1:length(list_filenames)){
  
  # load the probability raster
  raster_probability <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                      list_filenames[[m]]))
  
  # dummy raster generation: did the fire probability threshold get passed? LOW PROBABILITY
  raster_lowProbDummy <- raster_probability
  values(raster_lowProbDummy) <- ifelse(values(raster_probability) >
                                          highRiskThresholds[highRiskThresholds$island == vec_islands[[1]],
                                                             'hiRiskThresh_25pctileBurned'],
                                        1, 0)
  writeRaster(raster_lowProbDummy, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/13b Hawaii Island individual rasters/dummyRaster_lowFireRisk_Hawaii_',
                                                     substr(list_filenames[[m]], 31, 36), '.tif'),
              overwrite = TRUE)

  # dummy raster generation: did the fire probability threshold get passed? MODERATE PROBABILITY
  raster_moderateProbDummy <- raster_probability
  values(raster_moderateProbDummy) <- ifelse(values(raster_probability) >
                                          highRiskThresholds[highRiskThresholds$island == vec_islands[[1]],
                                                             'hiRiskThresh_50pctileBurned'],
                                        1, 0)
  writeRaster(raster_moderateProbDummy, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/13b Hawaii Island individual rasters/dummyRaster_moderateFireRisk_Hawaii_',
                                                     substr(list_filenames[[m]], 31, 36), '.tif'),
              overwrite = TRUE)
  
  # dummy raster generation: did the fire probability threshold get passed? HIGH PROBABILITY
  raster_highProbDummy <- raster_probability
  values(raster_highProbDummy) <- ifelse(values(raster_probability) >
                                          highRiskThresholds[highRiskThresholds$island == vec_islands[[1]],
                                                             'hiRiskThresh_75pctileBurned'],
                                        1, 0)
  writeRaster(raster_highProbDummy, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/13b Hawaii Island individual rasters/dummyRaster_highFireRisk_Hawaii_',
                                                     substr(list_filenames[[m]], 31, 36), '.tif'),
              overwrite = TRUE)
  
  rm(raster_lowProbDummy,raster_moderateProbDummy, raster_highProbDummy, raster_probability)
  gc()
  
}