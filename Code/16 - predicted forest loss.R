
# This script regresses forest pixels lost per fire onto proportion of grass-dominate
# pixels in each watershed, mean annual rainfall, and the upper quartile of fire risk
# at the time of fire.

library(raster); library(sf)
library(ggplot2)
library(doParallel)
registerDoParallel(cores = 10)

rasterOptions(maxmemory = 5e+10,
              chunksize = 1e+09)




##### data setup #####

# define islands
vec_islands <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')

# load watershed shapefile
sf_watershed <-
  read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii watersheds/Watersheds.shp")

# load max historical fire risk (from monthly data)
list_rasters_maxHistoricalRisk <- list()
for(i in 1:length(vec_islands)){
  list_rasters_maxHistoricalRisk[[i]] <- 
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14c median and max historical fire risk prob by island/',
                  'max fire risk ', vec_islands[[i]], '.tif'))
}
rm(i)

# merge all islands into one raster
raster_fireRisk <- do.call(merge, list_rasters_maxHistoricalRisk)
raster_fireRisk <- trim(raster_fireRisk)
rm(list_rasters_maxHistoricalRisk)
gc()

# re-project watershed shapefile to match rasters
sf_watershed <- st_transform(sf_watershed, crs(raster_fireRisk))

# # create master dummy - ALL GRID INDICES ARE BASED ON THIS MASTER DUMMY
# raster_dummy_master <- raster_fireRisk
# raster_dummy_master[!is.na(raster_dummy_master)] <- 1
# gc()
# 
# writeRaster(raster_dummy_master,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                               'raster_dummy_master.tif'),
#             overwrite = TRUE)
raster_dummy_master <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                'raster_dummy_master.tif'))




##### find grid indices of each fire #####

# Create list of indices indicating pixels covered in each fire perimeter.
# List elements correspond to fire perimeter list.

# # load fire perimeters
# sf_firePerimeters <-
#   read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")
# 
# # re-project fire shapefile to match rasters
# sf_firePerimeters <- st_transform(sf_firePerimeters,
#                                   crs(raster_dummy_master))
# 
# # split fire perimeters by individual fire
# sf_firePerimeters_split <-
#   split(sf_firePerimeters, sf_firePerimeters$kmlname)
# rm(sf_firePerimeters)
# gc()
# 
# # loop through each fire and find indices of pixels burned
# list_gridIndices_fires <-
#   lapply(sf_firePerimeters_split,
#          function(f){
#     ras_f <- mask(raster_dummy_master, f)
#     gc()
#     which(!is.na(values(ras_f)))
#   })
# 
# saveRDS(list_gridIndices_fires,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_gridIndices_fires.rds'))

list_gridIndices_fires <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_fires.rds'))

gc()




##### find grid indices of watershed #####

# Create list of indices indicating pixels covered in each watershed.
# List elements correspond to watershed list.

# # split watersheds by individual watershed
# sf_watersheds_split <-
#   split(sf_watershed, sf_watershed$objectid)
# rm(sf_watershed)
# gc()
# 
# # loop through each watershed and find indices of pixels in each watershed
# list_gridIndices_watersheds <-
#   lapply(sf_watersheds_split,
#          function(w){
#            ras_w <- mask(raster_dummy_master, w)
#            gc()
#            which(!is.na(values(ras_w)))
#          })
# 
# saveRDS(list_gridIndices_watersheds,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_gridIndices_watersheds.rds'))

list_gridIndices_watersheds <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_watersheds.rds'))

gc()




##### find grid indices of forest-dominated pixels each year #####

# define forest-dominant
val_forestDominant <- 0.40

# list forest cover rasters
list_rasters_forestCover <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages',
             pattern = 'wood')

# # loop through the rasters and find the indices of forest-dominant pixels
# list_gridIndices_forestDominantByYear <-
#   lapply(list_rasters_forestCover,
#          function(r){
#            
#            # load raster and match projection to master
#            ras <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
#                                 r))
#            ras <- trim(ras)
#            ras <- projectRaster(ras, raster_dummy_master)
#            gc()
#            
#            # find indices of forest-dominant pixels
#            which(values(ras) > val_forestDominant &
#                    !is.na(values(ras))
#                  )
#          })
# 
# saveRDS(list_gridIndices_forestDominantByYear,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_gridIndices_forestDominantByYear.rds'))

list_gridIndices_forestDominantByYear <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_forestDominantByYear.rds'))

gc()

# # loop through the rasters and find the indices of NON-forest-dominant pixels
# list_gridIndices_nonForestDominantByYear <-
#   lapply(list_rasters_forestCover,
#          function(r){
#            
#            # load raster and match projection to master
#            ras <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
#                                 r))
#            ras <- trim(ras)
#            ras <- projectRaster(ras, raster_dummy_master)
#            gc()
#            
#            # find indices of forest-dominant pixels
#            which(values(ras) < val_forestDominant &
#                    !is.na(values(ras))
#            )
#          })
# 
# saveRDS(list_gridIndices_nonForestDominantByYear,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_gridIndices_nonForestDominantByYear.rds'))

list_gridIndices_nonForestDominantByYear <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_nonForestDominantByYear.rds'))

gc()




##### find grid indices of grass-dominated pixels by year #####

# define grass-dominant
val_grassDominant <- 0.40

# list grass cover rasters
list_rasters_grassCover <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages',
             pattern = 'herb')

# # loop through the rasters and find the indices of grass-dominant pixels
# list_gridIndices_grassDominantByYear <-
#   lapply(list_rasters_grassCover,
#          function(r){
# 
#            # load raster and match projection to master
#            ras <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
#                                 r))
#            ras <- trim(ras)
#            ras <- projectRaster(ras, raster_dummy_master)
#            gc()
# 
#            # find indices of grass-dominant pixels
#            which(values(ras) > val_grassDominant &
#                    !is.na(values(ras))
#                  )
#          })
# 
# saveRDS(list_gridIndices_grassDominantByYear,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_gridIndices_grassDominantByYear.rds'))

list_gridIndices_grassDominantByYear <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_grassDominantByYear.rds'))




##### find mean rainfall of each watershed (mm/yr) #####

# # load rainfall raster
# raster_rainAnn <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
# 
# # mask annual rainfall raster with each watershed shapefile
# raster_rainAnn_split <-
#   lapply(sf_watershed_split, function(w){
#     ras_w <- mask(raster_rainAnn, w)
#     if(length(unique(values(ras_w))) > 1){
#       ras_w <- trim(ras_w)
#     } else {
#       ras_w <- NA
#     }
#     
#     gc()
#     ras_w
#   })
# 
# # for each watershed, find the mean annual rainfall
# vec_watershed_rainAnn <-
#   sapply(raster_rainAnn_split,
#          function(w){
#            if(length(w) > 1){
#              mean(values(w), na.rm = TRUE)
#            } else {
#              NA
#            }
#          })
# 
# saveRDS(vec_watershed_rainAnn,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'vec_watershed_rainAnn.rds'))

vec_watershed_rainAnn <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'vec_watershed_rainAnn.rds'))




##### find which watershed(s) each fire occurred in #####

# For each fire's individual pixels, find which watershed it belongs to.
# This is done because a fire can cross watershed boundaries.
list_fireGrid_watershed_index <- list()
for(f in seq_along(list_gridIndices_fires)){
  list_fireGrid_watershed_index[[f]] <- list()
  for(g in seq_along(list_gridIndices_fires[[f]])){
    list_fireGrid_watershed_index[[f]][[g]] <-
      names(list_gridIndices_watersheds)[vapply(list_gridIndices_watersheds,
                                                is.element,
                                                el = list_gridIndices_fires[[f]][[g]],
                                                FUN.VALUE = FALSE)]
  }
}

saveRDS(list_fireGrid_watershed_index,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                      'list_fireGrid_watershed_index.rds'))
