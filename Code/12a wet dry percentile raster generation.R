
# this script uses historical rainfall data to determine 25th and 75th percentiles for each pixel, by month and island

library(raster); library(stringr); library(doParallel)
registerDoParallel(cores = 4)

# list islands
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')

# list months
vec_months <- sprintf("%02d", 1:12)

# list historical rainfall files
vec_historicalRainfallFileNames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly', pattern = '.tif')




##### historical rainfall 25th and 75th percentiles by island and month #####

# for each island, load historical rainfall
foreach(i = 1:length(vec_islands), .packages = c('raster', 'stringr')) %dopar% {
  for(m in 1:12){
    
    # list all files for island i and month m
    files <- vec_historicalRainfallFileNames[str_detect(vec_historicalRainfallFileNames, vec_islands[[i]]) &   # subset filenames by island
                                               str_detect(vec_historicalRainfallFileNames, paste0('_', vec_months[[m]], '_'))]   # subset filenames by month of year
    
    # load those files
    rasterStack <- stack(lapply(files, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', r))))
    
    # calculate quartiles
    raster_quantiles <- calc(rasterStack, fun = function(r) raster::quantile(r, na.rm = TRUE))
    
    # write 25th and 75th percentile rasters
    writeRaster(raster_quantiles[[2]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm25pctile_', vec_islands[[i]], '_month', vec_months[[m]], '.tif'),
                overwrite = TRUE)
    writeRaster(raster_quantiles[[4]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm75pctile_', vec_islands[[i]], '_month', vec_months[[m]], '.tif'),
                overwrite = TRUE)
    
    gc()
  }
}
