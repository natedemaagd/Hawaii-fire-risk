
# this file combines the rasters created in steps 1 and 2 into stacks for predictions

library(raster)
setwd('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/completed_stacks')

# create vectors for naming raster stacks
year <- 1999:2016
month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# write raster stacks
for(i in 1:length(island)){
  for(m in 1:length(month)){
    for(y in 1:length(year)){
      
      # load rasters
      herbcov_yearof              <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/herbcov_yearof/herbcov_yearof', island[[i]], year[[y]], '.tif', sep= '_'))
      woodcov_yearof              <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/woodcov_yearof/woodcov_yearof', island[[i]], year[[y]], '.tif', sep= '_'))
      HIEVAP_min_monthly_soil_mst <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst', island[[i]], '.tif', sep = '_'))
      max_temp                    <- raster(paste())
      mean_annual_temp            <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp', island[[i]], '.tif', sep = '_'))
      rf_1mo_prior                <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_1mo_prior/rf_1mo_prior', island[[i]], month[[m]], '.tif', sep = '_'))
      monthly_cumrf_3mo           <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_cumrf_3mo/monthly_cumrf_3mo', island[[i]], month[[m]], '.tif', sep = '_'))
      monthly_cumrf_12mo          <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_cumrf_12mo/monthly_cumrf_12mo', island[[i]], '.tif', sep = '_'))
      island_dummy                <- raster(paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/island_dummy/dummy', island[[i]], '.tif', sep = '_'))
      
      # create stack
      r_stack <- raster::stack(herbcov_yearof,
                               woodcov_yearof,
                               HIEVAP_min_monthly_soil_mst,
                               mean_annual_temp,
                               rf_1mo_prior,
                               monthly_cumrf_3mo,
                               monthly_cumrf_12mo,
                               island_dummy)
      
          names(r_stack) <- c('herbcov_yearof',
                              'woodcov_yearof',
                              'HIEVAP_min_monthly_soil_mst',
                              'mean_annual_temp',
                              'rf_1mo_prior',
                              'monthly_cumrf_3mo',
                              'monthly_cumrf_12mo',
                              'island_dummy')
      
      # write stack
      saveRDS(r_stack, file = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/completed_stacks/raster_stack', island[[i]], year[[y]], month[[m]], '.rds', sep = '_'))
      
      gc()
      
    }
  }
}

rm(i.m.y)
