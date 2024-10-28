
# this script takes the raster stacks created in 13b and 13c and combines them into total frequency rasters, which are then plotted

library(raster)

# list raster stack filenames
list_rasterStackFilenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters', pattern = '.rds')

# sum each stack of dummy rasters
for(i in 1:length(list_rasterStackFilenames)){
  
  # load raster stack
  rasterStack <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/',
                                list_rasterStackFilenames[[i]]))
  
  # dummy rasters -> frequency raster (add all rasters together)
  raster_freq <- calc(rasterStack, sum)
  
  # get risk level and island name for file i
  file_label <- substr(list_rasterStackFilenames[[i]], 18, nchar(list_rasterStackFilenames[[i]])-4)
  
  # save frequency raster
  writeRaster(raster_freq, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13d fire risk threshold frequency rasters/',
                                             file_label, '.tif'),
              overwrite = TRUE)
  
  rm(rasterStack, raster_freq, file_label); gc()
  
}
