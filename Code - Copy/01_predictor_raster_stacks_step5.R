
# this file creates "mean max" temperature rasters for the prediction raster stacks, and crops all max temperature rasters to the islands for stacking

library(raster)




##### mean max rasters #####

# load monthly max temp rasters
rasterListNames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/monthly', pattern = 'Tmax')
rasterList <- lapply(rasterListNames, function (r){raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/monthly/', r))})

# create means for each month
monthCodes <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')  # month codes used to extract correct month's raster for each year


s <- stack(rasterList[which(substr(rasterListNames, 20, 21) == '01')])
r <- mean(s)/100

for(i in 1:length(monthCodes)){
  
  # create stack of max temps
  s <- stack(rasterList[which(substr(rasterListNames, 20, 21) == monthCodes[[i]])])
  
  # write mean of all max temps as new raster
  writeRaster(mean(s)/100,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/mean_max_temp_by_month/meanMax_month', monthCodes[[i]], '.tif'),
              overwrite = TRUE)
}

rm(r, s, monthCodes)




##### raster cropping for stacks - mean monthly max temperature #####

# load landcover raster
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')

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
names(island_extents) <- c('extha', 'extma',            'extkah',     'extmol',   'extoa',   'extka',  'extlan', 'extmaco')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# load
all_rasters_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/mean_max_temp_by_month', pattern = 'meanMax_month')
all_rasters <- lapply(all_rasters_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/mean_max_temp_by_month/', r)))

# divide values by 100 so it's in degrees C (currently no decimals)
all_rasters <- lapply(all_rasters, function(r){r/100;r})

# format to match landcover raster
all_rasters <- lapply(all_rasters, function(r) resample(r, raster_lc))
saveRDS(all_rasters, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_historical_avgMaxTemperature_resampled.rds')

# split each raster by island and write raster
year <- unique(substr(all_rasters_names, 1, 4))
month <- unique(substr(all_rasters_names, 14, 15))

for(i in 1:length(island_extents)){
    for(m in 1:length(month)){
      writeRaster(crop(all_rasters[[which(all_rasters_names == paste0('meanMax_month', month[[m]], '.tif'))]],
                       island_extents[[i]]),
                  filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/AvgMonthlyHistorical/AvgMonthlyHistorical_',
                                    island[[i]], '_month', month[[m]], '.tif'))
      gc()
    }
}




##### raster cropping for stacks - monthly max temperature #####

# load landcover raster
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')

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
names(island_extents) <- c('extha', 'extma',            'extkah',     'extmol',   'extoa',   'extka',  'extlan', 'extmaco')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# load
all_rasters_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/monthly', pattern = 'Tmax')
all_rasters <- lapply(all_rasters_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/monthly/', r)))

# # format to match landcover raster- 10 hours to run
# all_rasters <- lapply(all_rasters, function(r) resample(r, raster_lc))
# saveRDS(all_rasters, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_historical_monthlyMaxTemperature_resampled.rds')
# 
# # divide values by 100 so it's in degrees C (currently no decimals)
# a <- Sys.time()
# all_rasters <- lapply(all_rasters, function(r){r/100;r})
# Sys.time()-a

# split each raster by island and write raster
year <- unique(substr(all_rasters_names, 16, 19))
month <- unique(substr(all_rasters_names, 20, 21))

# for(i in 1:length(island_extents)){
#   for(y in 1:length(year)){
#     for(m in 1:length(month)){
#       writeRaster(crop(all_rasters[[which(all_rasters_names == paste0(year[[y]], '_', month[[m]], '_statewide_rf_mm.tif'))]],
#                        island_extents[[i]]),
#                   filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/AvgMonthlyHistorical/',
#                                    island[[i]], year[[y]], month[[m]], '.tif', sep = '_'))
#       gc()
#     }
#   }
# }

for(y in 1:length(year)){
  for(m in 1:length(month)){
    
    # resample raster for year-month
    r_state <- resample(all_rasters[[which(all_rasters_names == paste0('Tmax_map_state_', year[[y]], month[[m]], '_monthly_comp.tif'))]],
                        raster_lc)
    
    # divide values by 100 (to add decimal place)
    r_state <- r_state/100
    
    # crop to each island and save
    for(i in 1:length(island)){
      writeRaster(crop(r_state, island_extents[[i]]),
                  filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/MonthlyHistorical/',
                                    'MaxTempMonthly_', island[[i]], '_', year[[y]], month[[m]], '.tif'),
                  overwrite = TRUE)
      gc()
    }
  }
}
