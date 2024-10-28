
# this file loads 1999 and 2016 landcover rasters from Matt Lucas and formats them for use in our project

library(raster); library(rgdal)




##### load and format 1999 and 2016 rasters #####

# load data (stacks of rasters)
raster_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/Raw 1999 2016', pattern = '.tif')
raster_stacks <- lapply(raster_names, function(r) stack(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/Raw 1999 2016/', r)))

# unstack all rasters
rasters <- lapply(raster_stacks, unstack)

# `rasters` is now a nested list of individual rasters. Unlist to create a single list
rasters <- unlist(rasters)

# get raster names, and append landcover type
raster_names <- sapply(rasters, names)
raster_names <- sapply(raster_names, function(s) substr(s,1,nchar(s)-2) )  # remove last two characters (the layer id number: .1, .2, .3)
landcover_type <- c('_herb', '_wood', '_bare')
raster_names <- paste0(raster_names, landcover_type)
names(rasters) <- raster_names
rm(raster_stacks, landcover_type)

# group rasters by landcover and year
rasters_herb_1999 <- grep('1999', raster_names, value = TRUE)
rasters_herb_1999 <- grep('herb', rasters_herb_1999, value = TRUE)
rasters_herb_1999 <- rasters[names(rasters) %in% rasters_herb_1999]
rasters_wood_1999 <- grep('1999', raster_names, value = TRUE)
rasters_wood_1999 <- grep('wood', rasters_wood_1999, value = TRUE)
rasters_wood_1999 <- rasters[names(rasters) %in% rasters_wood_1999]
rasters_bare_1999 <- grep('1999', raster_names, value = TRUE)
rasters_bare_1999 <- grep('bare', rasters_bare_1999, value = TRUE)
rasters_bare_1999 <- rasters[names(rasters) %in% rasters_bare_1999]
rasters_herb_2016 <- grep('2016', raster_names, value = TRUE)
rasters_herb_2016 <- grep('herb', rasters_herb_2016, value = TRUE)
rasters_herb_2016 <- rasters[names(rasters) %in% rasters_herb_2016]
rasters_wood_2016 <- grep('2016', raster_names, value = TRUE)
rasters_wood_2016 <- grep('wood', rasters_wood_2016, value = TRUE)
rasters_wood_2016 <- rasters[names(rasters) %in% rasters_wood_2016]
rasters_bare_2016 <- grep('2016', raster_names, value = TRUE)
rasters_bare_2016 <- grep('bare', rasters_bare_2016, value = TRUE)
rasters_bare_2016 <- rasters[names(rasters) %in% rasters_bare_2016]
rm(rasters, raster_names)

# combine landcover-year raster groups (currently separated by county) into single statewide rasters
raster_bare_1999 <- Reduce(function(...)merge(...,tolerance=0),rasters_bare_1999); rm(rasters_bare_1999)
raster_herb_1999 <- Reduce(function(...)merge(...,tolerance=0),rasters_herb_1999); rm(rasters_herb_1999)
raster_wood_1999 <- Reduce(function(...)merge(...,tolerance=0),rasters_wood_1999); rm(rasters_wood_1999)
raster_bare_2016 <- Reduce(function(...)merge(...,tolerance=0),rasters_bare_2016); rm(rasters_bare_2016)
raster_herb_2016 <- Reduce(function(...)merge(...,tolerance=0),rasters_herb_2016); rm(rasters_herb_2016)
raster_wood_2016 <- Reduce(function(...)merge(...,tolerance=0),rasters_wood_2016); rm(rasters_wood_2016)
gc()

# resample to base landcover raster
raster_base <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
raster_bare_1999_resampled <- resample(raster_bare_1999, raster_base)
raster_herb_1999_resampled <- resample(raster_herb_1999, raster_base)
raster_wood_1999_resampled <- resample(raster_wood_1999, raster_base)
raster_bare_2016_resampled <- resample(raster_bare_2016, raster_base)
raster_herb_2016_resampled <- resample(raster_herb_2016, raster_base)
raster_wood_2016_resampled <- resample(raster_wood_2016, raster_base)

# save rasters, converting from percent (0-100) to fraction (0-1)
writeRaster(raster_herb_1999_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_herb_1999.tif', overwrite = TRUE)
writeRaster(raster_wood_1999_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_1999.tif', overwrite = TRUE)
writeRaster(raster_bare_1999_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_bare_1999.tif', overwrite = TRUE)
writeRaster(raster_herb_2016_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_herb_2016.tif', overwrite = TRUE)
writeRaster(raster_wood_2016_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_2016.tif', overwrite = TRUE)
writeRaster(raster_bare_2016_resampled/100, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_bare_2016.tif', overwrite = TRUE)

