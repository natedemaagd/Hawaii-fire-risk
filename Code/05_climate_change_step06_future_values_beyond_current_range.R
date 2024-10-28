library(raster); library(dplyr); library(doParallel)
registerDoParallel(cores = 10)




##### get current range of temp/rainfall #####

# list all current mean rainfall and temperature rasters
raster_current_rainfall <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
raster_current_temp     <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_ann/HI_EVAP_mean_annual_temp_statewide_250m.tif")

# define island extents
island_extents <- list(
  extent(-156.2, -154.8, 18.8, 20.28),
  extent(-156.75, -155.9, 20.4, 21.1),
  extent(-156.8, -156.45,20.45, 20.61),
  extent( -157.5, -156.5,  21.05, 21.3),
  extent(-158.4, -157.5,  21.2, 21.8),
  extent(-159.9, -159.2 , 21.8, 22.3),
  extent( -157.1, -156.75,  20.66, 20.95),
  extent(-157.5, -155.9, 20.45, 21.3)
)
# BI     # maui, kahoolawe   # kahoolawe   # molokai   # oahu     # kauai   # lanai   # maui county (maui, lanai, molokai, kahoolawe)
names(island_extents) <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# split climate rasters by island
raster_current_rainfall_byIsland <- list()
for(i in 1:length(island_extents)){
  raster_current_rainfall_byIsland[[i]] <- crop(raster_current_rainfall, island_extents[[i]])
}
raster_current_temp_byIsland <- list()
for(i in 1:length(island_extents)){
  raster_current_temp_byIsland[[i]] <- crop(raster_current_temp, island_extents[[i]])
}
names(raster_current_rainfall_byIsland) <- names(raster_current_temp_byIsland) <- names(island_extents)
rm(i, raster_current_rainfall,raster_current_temp, island_extents)

# get range of temp and rainfall for each island
range_rain_byIsland <- lapply(raster_current_rainfall_byIsland, function(r) range(values(r), na.rm = TRUE))
range_temp_byIsland <- lapply(raster_current_temp_byIsland,     function(r) range(values(r), na.rm = TRUE))
rm(raster_current_rainfall_byIsland, raster_current_temp_byIsland); gc()




##### find how much current range is exceeded in future #####

# rainfall
rasters_future_rain_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/', pattern = 'future_rain')
foreach(i = 1:length(rasters_future_rain_filenames), .packages = c('raster', 'dplyr')) %dopar% {
  
  # load raster i
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/', rasters_future_rain_filenames[[i]]))
  
  # select correct range based on island
  island <- strsplit(rasters_future_rain_filenames, '_')[[i]][[6]]
  
  # find difference between future predicted value and current range (take smallest absolute difference of the two - accounts for some values being lower than min and others being higher than max)
  vals <- values(r)[!is.na(values(r))]
  vals_below_range <- vals - min(range_rain_byIsland[[island]])
  vals_above_range <- vals - max(range_rain_byIsland[[island]])
  vals_within_range <- between(vals, min(range_rain_byIsland[[island]]), max(range_rain_byIsland[[island]]))
    vals_within_range <- ifelse(vals_within_range, 0, 99999)  # if in range, want value to be 0 otherwise discard it
  vals_df <- data.frame('below' = vals_below_range,
                        'above' = vals_above_range,
                        'within' = vals_within_range)
  vals_df$val_final <- apply(vals_df, MARGIN = 1, function(r){r[[which.min(abs(r))]]})
  values(r)[!is.na(values(r))] <- vals_df$val_final
  
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/future_climate_beyond_current_range/rain/valOutsideCurrentRange_', rasters_future_rain_filenames[[i]]), overwrite = TRUE)
  
  # create additional dummy raster, 1 = value outside current range and 0 = value within current range
  values(r)[values(r) < 0] <- -1
  values(r)[values(r) > 0] <- 1
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/future_climate_beyond_current_range/rain/dummy/valOutsideCurrentRangeDummy_', rasters_future_rain_filenames[[i]]), overwrite = TRUE)
  gc()
}

gc()

# temperature
rasters_future_temp_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp/', pattern = 'future_temp')
foreach(i = 1:length(rasters_future_temp_filenames), .packages = c('raster', 'dplyr')) %dopar% {
  
  # load raster i
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp/', rasters_future_temp_filenames[[i]]))
  
  # select correct range based on island
  island <- strsplit(rasters_future_rain_filenames, '_')[[i]][[6]]
  
  # find difference between future predicted value and current range (take smallest absolute difference of the two - accounts for some values being lower than min and others being higher than max)
  vals <- values(r)[!is.na(values(r))]
  vals_below_range <- vals - min(range_temp_byIsland[[island]])
  vals_above_range <- vals - max(range_temp_byIsland[[island]])
  vals_within_range <- between(vals, min(range_temp_byIsland[[island]]), max(range_temp_byIsland[[island]]))
  vals_within_range <- ifelse(vals_within_range, 0, 99999)  # if in range, want value to be 0 otherwise discard it
  vals_df <- data.frame('below' = vals_below_range,
                        'above' = vals_above_range,
                        'within' = vals_within_range)
  vals_df$val_final <- apply(vals_df, MARGIN = 1, function(r){r[[which.min(abs(r))]]})
  values(r)[!is.na(values(r))] <- vals_df$val_final
  
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/future_climate_beyond_current_range/temp/valOutsideCurrentRange_', rasters_future_temp_filenames[[i]]), overwrite = TRUE)
  
  # create additional dummy raster, 1 = value outside current range and 0 = value within current range
  values(r)[values(r) < 0] <- -1
  values(r)[values(r) > 0] <- 1
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/future_climate_beyond_current_range/temp/dummy/valOutsideCurrentRangeDummy_', rasters_future_temp_filenames[[i]]), overwrite = TRUE)
  gc()
}

gc()
