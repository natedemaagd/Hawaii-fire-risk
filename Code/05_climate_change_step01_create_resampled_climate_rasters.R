library(raster)

# define islands and months
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
island_name <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')
month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
rain_season <- c('wet', 'wet', 'wet', 'wet', 'dry', 'dry', 'dry', 'dry', 'dry', 'dry', 'wet', 'wet')

# define island extents
island_extents <- list(
  extent(-156.2, -154.8, 18.8, 20.28),  # BI
  #extent(-156.75, -155.9, 20.4, 21.1),
  #extent(-156.8, -156.45,20.45, 20.61),
  #extent( -157.5, -156.5,  21.05, 21.3),
  extent(-158.4, -157.5,  21.2, 21.8),  # Oahu
  extent(-159.9, -159.2 , 21.8, 22.3),  # Kauai
  #extent( -157.1, -156.75,  20.66, 20.95),
  extent(-157.5, -155.9, 20.45, 21.3)  # Maui Nui
)

# create mean monthly rainfall rasters
for(i in 1:length(island)){
  for(m in 4:length(month)){
    monthlyRainfall_names <- list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly',
                                           pattern = paste0('_', month[[(m-1)]], '_'))
    monthlyRainfall_names <- monthlyRainfall_names[grep(island[[i]], monthlyRainfall_names)] # keep only the necessary island
    monthlyRainfall_rasters <- lapply(monthlyRainfall_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', r)))
    rf_monthlyAvg <- Reduce(mean, monthlyRainfall_rasters)
    names(rf_monthlyAvg) <- 'rf_monthlyAvg'
    rm(monthlyRainfall_names, monthlyRainfall_rasters)
    writeRaster(rf_monthlyAvg, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_mean_monthly/mean_rainfall_', island[[i]], '_month', month[[m]], '.tif'), overwrite = TRUE)
    removeTmpFiles(h=2)
    gc()
  }
}

rm(rf_monthlyAvg, i, m); gc()




##### future temperature - annual #####

# load future climate data (deltas)
temp_deltas <- list(
  dyn_rcp45_2100_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureTemperature/DynDS_1FutureSeasChange_Temp_250m/DynDS_FutureSeasChange_Temp_degC_250m/DynDS_HI_Temp_degC_chng_rcp45_Ann_2100.tif"),
  dyn_rcp85_2100_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureTemperature/DynDS_1FutureSeasChange_Temp_250m/DynDS_FutureSeasChange_Temp_degC_250m/DynDS_HI_Temp_degC_chng_rcp85_Ann_2100.tif"),
  sta_rcp45_2070_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureTemperature/StatDS_1FutureSeasChange_Temp_250m/StatDS_FutureSeasChange_Temp_degC_250m/StatDS_HI_Temp_degC_chng_rcp45_ann_2040_2070.tif"),
  sta_rcp85_2070_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureTemperature/StatDS_1FutureSeasChange_Temp_250m/StatDS_FutureSeasChange_Temp_degC_250m/StatDS_HI_Temp_degC_chng_rcp85_ann_2040_2070.tif"),
  sta_rcp45_2100_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureTemperature/StatDS_1FutureSeasChange_Temp_250m/StatDS_FutureSeasChange_Temp_degC_250m/StatDS_HI_Temp_degC_chng_rcp45_ann_2100.tif"),
  sta_rcp85_2100_temp = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureTemperature/StatDS_1FutureSeasChange_Temp_250m/StatDS_FutureSeasChange_Temp_degC_250m/StatDS_HI_Temp_degC_chng_rcp85_ann_2100.tif")
)

# apply anomaly to current temp for all islands
for(i in 1:length(island)){

  # load temp for island i
  temp_current <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp_', island[[i]], '_.tif'))
  
  # crop deltas rasters to match island i and resample
  temp_delta_island_i <- lapply(temp_deltas, function(r) crop(r, temp_current))
  temp_delta_island_i <- lapply(temp_delta_island_i, function(r) resample(r, temp_current))
  
  # add delta to current temp
  temp_futures <- lapply(temp_delta_island_i, function(r) r + temp_current)
  
  # save rasters
  for(r in 1:length(temp_futures)){
    writeRaster(temp_futures[[r]],
                filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp/future_temp_', substr(names(temp_futures)[[r]], 1, 14), '_', island[[i]], '.tif'),
                overwrite = TRUE)
  }
  
  gc()
  removeTmpFiles(2)
}




##### future temperature - max monthly #####

# apply anomaly to current max temp for all islands
for(i in 1:length(island)){
  for(m in 1:length(month)){
    # load max temp for island i
    temp_current <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/AvgMonthlyHistorical/AvgMonthlyHistorical_', island[[i]], '_month', month[[m]], '.tif'))
    
    # crop deltas rasters to match island i and resample
    temp_delta_island_i <- lapply(temp_deltas, function(r) crop(r, temp_current))
    temp_delta_island_i <- lapply(temp_delta_island_i, function(r) resample(r, temp_current))
    
    # add delta to current temp
    temp_futures <- lapply(temp_delta_island_i, function(r) r + temp_current)
    
    # save rasters
    for(r in 1:length(temp_futures)){
      writeRaster(temp_futures[[r]],
                  filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp_max/future_tempmax_', substr(names(temp_futures)[[r]], 1, 14), '_', island[[i]], '_month', month[[m]], '.tif'),
                  overwrite = TRUE)
    }
  }
  gc()
  removeTmpFiles(2)
}




##### future rainfall #####

# load future climate data (deltas as percent change)
rain_deltas <- list(
  dyn_rcp45_2100_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp45_Dry_2100.tif"),
  dyn_rcp45_2100_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp45_Wet_2100.tif"),
  dyn_rcp85_2100_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp85_Dry_2100.tif"),
  dyn_rcp85_2100_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp85_Wet_2100.tif"),
  sta_rcp45_2070_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_dry_2040_2070.tif"),
  sta_rcp45_2070_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_wet_2040_2070.tif"),
  sta_rcp85_2070_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_dry_2040_2070.tif"),
  sta_rcp85_2070_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_wet_2040_2070.tif"),
  sta_rcp45_2100_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_dry_2100.tif"),
  sta_rcp45_2100_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_wet_2100.tif"),
  sta_rcp85_2100_dry_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_dry_2100.tif"),
  sta_rcp85_2100_wet_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_wet_2100.tif")
)

rain_deltas_annual <- list(
  dyn_rcp45_2100_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp45_Ann_2100.tif"),
  dyn_rcp85_2100_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp85_Ann_2100.tif"),
  sta_rcp45_2070_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_ann_2040_2070.tif"),
  sta_rcp85_2070_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_ann_2040_2070.tif"),
  sta_rcp45_2100_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp45_ann_2100.tif"),
  sta_rcp85_2100_rain = raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_ann_2100.tif")
)

# divide percent by 100 and add 1: 1 = current rainfall,0.8 is 80% current rainfall, 1.2 is 120%, etc.
rain_deltas <- lapply(rain_deltas, function(r) (r/100)+1 )
rain_deltas_annual <- lapply(rain_deltas_annual, function(r) (r/100)+1 )

# apply anomaly statewide for mean annual rainfall
monthly_cumrf_12mo_statewide <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
monthly_cumrf_12mo_statewide <- resample(monthly_cumrf_12mo_statewide, raster_lc)
for(i in 1:length(island)){
  
  # crop current avg annual rainfall to island i
  rain_current <- crop(monthly_cumrf_12mo_statewide, island_extents[[i]])
  
  # crop deltas rasters for island i and resample
  rain_delta_island_i <- lapply(rain_deltas_annual, function(r) crop(r, rain_current))
  rain_delta_island_i <- lapply(rain_delta_island_i, function(r) resample(r, rain_current))
  
  # apply rainfall delta
  rain_futures <- lapply(rain_delta_island_i, function(r) r * rain_current)
  
  # replace negative numbers with 0s, if they exist
  rain_futures <- lapply(rain_futures, function(r){r[r<0] <- 0; r})
  
  # save rasters
  for(r in 1:length(rain_futures)){
    writeRaster(rain_futures[[r]],
                filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', substr(names(rain_futures)[[r]], 1, 14), '_', island[[i]], '_ann.tif'),
                overwrite = TRUE)
  }
  
  gc()
  removeTmpFiles(2)
}

# apply anomaly to each island-month
for(i in 1:length(island)){
  for(m in 1:length(month)){
    
    # load current rainfall for island i, month m
    rain_current <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_mean_monthly/mean_rainfall_', island[[i]], '_month', month[[m]], '.tif'))
    
    # crop deltas rasters to match island i and resample
    rain_delta_island_i <- lapply(rain_deltas, function(r) crop(r, rain_current))
    rain_delta_island_i <- lapply(rain_delta_island_i, function(r) resample(r, rain_current))
    
    # add delta to current rain, accounting for wet/dry season
    if(month[[m]] %in% c('11', '12', '01', '02', '03', '04')){
      rain_futures <- lapply(rain_delta_island_i[seq(2,12,2)], function(r) r * rain_current)
    } else {
      rain_futures <- lapply(rain_delta_island_i[seq(1,11,2)], function(r) r * rain_current)
    }
    
    # replace negative numbers with 0s, if they exist
    rain_futures <- lapply(rain_futures, function(r){r[r<0] <- 0; r})
    
    # save rasters
    for(r in 1:length(rain_futures)){
      writeRaster(rain_futures[[r]],
                  filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', substr(names(rain_futures)[[r]], 1, 14), '_', island[[i]], '_month', month[[m]], '.tif'),
                  overwrite = TRUE)
    }
    
    gc()
    removeTmpFiles(2)
    
  }
}
