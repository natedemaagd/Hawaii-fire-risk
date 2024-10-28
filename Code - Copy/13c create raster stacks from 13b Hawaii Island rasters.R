
# this script takes the individual monthly dummy rasters from 13b and creates a raster stack for each risk threshold

library(raster)

# define risk levels
vec_riskLevels <- c('lowFireRisk', 'moderateFireRisk', 'highFireRisk')

# list all raster filenames
vec_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/13b Hawaii Island individual rasters/', pattern = '.tif')

# for each risk level, create stack of rasters
for(i in 1:length(vec_riskLevels)){
  
  # list files for risk level i
  vec_filenames_riskLeveli <- vec_filenames[grep(vec_riskLevels[[i]], vec_filenames)]
  
  # create raster stack from those files
  rasterStack <- stack(lapply(vec_filenames_riskLeveli, function(r){
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/13b Hawaii Island individual rasters/', r))
  }))
  
  saveRDS(rasterStack, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/dummyRasterStack_',
                                     vec_riskLevels[[i]], '_Hawaii.rds'))
  
  rm(rasterStack); gc()
  
}
