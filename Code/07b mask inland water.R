
# This script masks inland water so it has a fire risk of 0.

# TO DO: MASK HISTORICAL RASTERS, POST-CLIMATE RATERS, AND OTHERS BEYOND 07

library(terra)




##### load data #####

# list final rasters to be masked
list_rastersNames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
             pattern = '.tif', full.names = TRUE)
list_rastersBinnedNames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned',
             pattern = '.tif', full.names = TRUE)

# # remove the water-masked rasters if they already exist
# list_rastersNames <-
#   list_rastersNames[-grep('waterMasked', list_rastersNames)]
# list_rastersBinnedNames <-
#   list_rastersBinnedNames[-grep('waterMasked', list_rastersBinnedNames)]

# load landcover rasters
ras_lc <- rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                      "mhi_s0_baseline_names.tif"))
ras_bare <- rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                        "HI_FracLC_bare_2016.tif"))




##### mask water pixels #####

# mask probability rasters
for(i in 1:length(list_rastersNames)) {
  
  # get file name
  filename <- list_rastersNames[[i]]
                  
  # load raster
  ras <- rast(filename)
  
  # crop land cover raster
  ras_bare_cropped <-
    crop(ras_bare, ras)
  
  # make dummy raster for masking
  ras_bare_cropped_dummy <- ras_bare_cropped
  ras_bare_cropped_dummy[ras_bare_cropped_dummy >= 0.99] <- 1
  ras_bare_cropped_dummy[ras_bare_cropped_dummy < 1] <- 0
  
  # mask
  ras_masked <- mask(ras, ras_bare_cropped_dummy,
                     maskvalue = 1, updatevalue = 0)
  
  # write raster
  writeRaster(ras_masked,
              filename = paste0(substr(filename, 1, nchar(filename)-4),
                                '_waterMasked.tif'),
              overwrite = TRUE)
  
  rm(filename, ras, ras_masked, ras_bare_cropped)
  gc()
  print(i)
}

# mask binned rasters
for(i in 1:length(list_rastersBinnedNames)) {
  
  # get file name
  filename <- list_rastersBinnedNames[[i]]
  
  # load raster
  ras <- rast(filename)
  
  # crop land cover raster
  ras_bare_cropped <-
    mask(ras_bare, ras)
  
  # make dummy raster for masking
  ras_bare_cropped_dummy <- ras_bare_cropped
  ras_bare_cropped_dummy[ras_bare_cropped_dummy >= 0.99] <- 1
  ras_bare_cropped_dummy[ras_bare_cropped_dummy < 1] <- 0
  
  # mask
  r_masked <- mask(ras, ras_bare_cropped, maskvalue = 1, updatevalue = 0)
  
  # write raster
  writeRaster(r_masked,
              filename = paste0(substr(filename, 1, nchar(filename)-4),
                                '_waterMasked.tif'),
              overwrite = TRUE)
  
  rm(filename, ras, r_masked, ras_lc_cropped)
  gc()
  print(i)
}

rm(i)

