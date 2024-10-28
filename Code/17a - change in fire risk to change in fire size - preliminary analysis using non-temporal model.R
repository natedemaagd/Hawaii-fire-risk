
# This script analyzes how the predicted size of fires will change with changing
# fire risk over time. This preliminary analysis uses the non-temporal forest fire
# model from script 16a V2.

library(raster)
library(ggplot2)
library(sf)
library(tidyverse)





##### data setup #####

# define islands
vec_islands <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')

# load watershed shapefile
sf_watershed <-
  read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii watersheds/Watersheds.shp")




##### historical and future fire risk data formatting #####

# load max historical fire risk (from monthly data)
list_rasters_maxHistoricalRisk <- list()
for(i in 1:length(vec_islands)){
  list_rasters_maxHistoricalRisk[[i]] <- 
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14c median and max historical fire risk prob by island/',
                  'max fire risk ', vec_islands[[i]], '.tif'))
}
rm(i)

# load max future fire risk (from monthly data)
list_rasters_maxHistoricalRisk <- list()
for(i in 1:length(vec_islands)){
  list_rasters_maxHistoricalRisk[[i]] <- 
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14c median and max historical fire risk prob by island/',
                  'futureMax Risk ', vec_islands[[i]], '.tif'))
}
rm(i)









# merge all islands into one raster
raster_fireRisk <- do.call(merge, list_rasters_maxHistoricalRisk)
raster_fireRisk <- trim(raster_fireRisk)
rm(list_rasters_maxHistoricalRisk)
gc()