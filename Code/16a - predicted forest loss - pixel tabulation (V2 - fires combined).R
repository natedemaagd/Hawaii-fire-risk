
# This script regresses forest pixels lost per fire onto proportion of grass-dominate
# pixels in each watershed, mean annual rainfall, and the upper quartile of fire risk
# at the time of fire. The setup is inefficient since it is built off the results of V1
# of this script.

library(raster); library(sf)
library(tidyverse)
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
# # remove fires after 2016
# sf_firePerimeters <-
#   sf_firePerimeters[sf_firePerimeters$Year < 2016,]
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

# list_fireGrid_watershed_index <- list()
# for(f in seq_along(list_gridIndices_fires)){
#   list_fireGrid_watershed_index[[f]] <- list()
#   for(g in seq_along(list_gridIndices_fires[[f]])){
#     list_fireGrid_watershed_index[[f]][[g]] <-
#       names(list_gridIndices_watersheds)[vapply(list_gridIndices_watersheds,
#                                                 is.element,
#                                                 el = list_gridIndices_fires[[f]][[g]],
#                                                 FUN.VALUE = FALSE)]
#   }
# }
# 
# list_fireGrid_watershed_index <-
#   lapply(list_fireGrid_watershed_index, as.numeric)
# 
# saveRDS(list_fireGrid_watershed_index,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_fireGrid_watershed_index.rds'))

list_fireGrid_watershed_index <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_fireGrid_watershed_index.rds'))




##### create dataframe of pixel-level data #####

# # get grid coords
# dat_coords <- as.data.frame(raster_dummy_master, xy = TRUE)[1:2]
# 
# # initiate data.frame
# dat_pixels <- data.frame(pixel_ID = 1:length(raster_dummy_master),
#                          watershed_ID = NA,
#                          x = dat_coords$x,
#                          y = dat_coords$y,
#                          burned_dummy = 0)
# rm(dat_coords)
# gc()
# 
# # assign watershed ID to each pixel
# for(w in seq_along(list_gridIndices_watersheds)){
#   dat_pixels$watershed_ID[list_gridIndices_watersheds[[w]]] <-
#     as.numeric(names(list_gridIndices_watersheds)[[w]])
# }
# rm(w)
# gc()
# 
# # get IDs of pixels that burned
# vec_burnedPixels <- unique(unlist(list_gridIndices_fires))
# dat_pixels$burned_dummy[vec_burnedPixels] <- 1
# rm(vec_burnedPixels)
# gc()
# 
# # add 1999 and 2016 forest cover values to data.frame
# raster_forest1999 <-
#   raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
#                 "HI_FracLC_wood_",
#                 1999, ".tif"))
# raster_forest2016 <-
#   raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
#                 "HI_FracLC_wood_",
#                 2016, ".tif"))
# raster_forest1999 <- trim(projectRaster(raster_forest1999, raster_dummy_master, method = 'ngb')); gc()
# raster_forest2016 <- trim(projectRaster(raster_forest2016, raster_dummy_master, method = 'ngb')); gc()
# dat_pixels$forestCover1999 <- values(raster_forest1999)
# dat_pixels$forestCover2016 <- values(raster_forest2016)
# rm(raster_forest1999, raster_forest2016)
# gc()
# 
# # calculate change in forest cover
# dat_pixels$forestCoverChange <-
#   dat_pixels$forestCover2016 - dat_pixels$forestCover1999
# 
# # add max historical fire risk
# dat_pixels$maxHistFireRisk <-
#   values(raster_fireRisk)
# 
# # remove ocean pixels
# dat_pixels <- dat_pixels[!is.na(dat_pixels$watershed_ID),]
# gc()
# 
# # find pixels where forest started above 40 and ended below 40
# dat_pixels$forestLost <-
#   with(dat_pixels,
#        ifelse(forestCover2016 < 0.40 & forestCover1999 >= 0.40, 1, 0))
# 
# # add find non-ocean grass- and forest-dominated pixels
# grassPixels1999_noOcean <-
#   data.frame(
#     pixel_ID = dat_pixels$pixel_ID[which(dat_pixels$pixel_ID %in%
#                                            list_gridIndices_grassDominantByYear[[1]]
#     )
#     ],
#     isGrass1999 = 1
#   )
# dat_pixels <- left_join(dat_pixels, grassPixels1999_noOcean, 'pixel_ID')
# dat_pixels$isGrass1999[is.na(dat_pixels$isGrass1999)] <- 0
# 
# forestPixels1999_noOcean <-
#   data.frame(
#     pixel_ID = dat_pixels$pixel_ID[which(dat_pixels$pixel_ID %in%
#                                            list_gridIndices_forestDominantByYear[[1]]
#     )
#     ],
#     isForest1999 = 1
#   )
# dat_pixels <- left_join(dat_pixels, forestPixels1999_noOcean, 'pixel_ID')
# dat_pixels$isForest1999[is.na(dat_pixels$isForest1999)] <- 0
# 
# rm(forestPixels1999_noOcean, grassPixels1999_noOcean)
# gc()
# 
# saveRDS(dat_pixels,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'dat_pixels.rds'))

dat_pixels <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'dat_pixels.rds'))




##### aggregate pixel-level data to watershed-level #####

# keep only burned pixels
dat_pixels_burned <- dat_pixels[dat_pixels$burned_dummy == 1,]

# sum number of forest pixels lost per watershed
dat_watershed_burnedForestLoss_totalPixels <-
  aggregate(dat_pixels_burned$forestLost,
            list(dat_pixels_burned$watershed_ID),
            sum, na.rm = TRUE)
colnames(dat_watershed_burnedForestLoss_totalPixels) <-
  c('watershed_ID', 'forestLoss_totalBurnedPixels')

# get 75th percentile of max fire risk in each watershed
dat_watershed_burnedForestLoss_fireRisk75th <-
  aggregate(dat_pixels$maxHistFireRisk,
            list(dat_pixels$watershed_ID),
            function(w){
              quantile(w, probs = 0.75)[[1]]
            })
colnames(dat_watershed_burnedForestLoss_fireRisk75th) <-
  c('watershed_ID', 'fireRisk_75thPctileMaxHistorical')

# get 90th percentile of max fire risk in each watershed
dat_watershed_burnedForestLoss_fireRisk90th <-
  aggregate(dat_pixels$maxHistFireRisk,
            list(dat_pixels$watershed_ID),
            function(w){
              quantile(w, probs = 0.90)[[1]]
            })
colnames(dat_watershed_burnedForestLoss_fireRisk90th) <-
  c('watershed_ID', 'fireRisk_90thPctileMaxHistorical')

# combine into one data.frame
dat_watershed_burnedForestLoss_fireRisk <-
  left_join(dat_watershed_burnedForestLoss_fireRisk75th,
            dat_watershed_burnedForestLoss_fireRisk90th,
            'watershed_ID')
rm(dat_watershed_burnedForestLoss_fireRisk75th,
   dat_watershed_burnedForestLoss_fireRisk90th)

# add mean annual rainfall to each watershed
dat_watershed_burnedForestLoss_fireRisk$rainfall_mmMeanAnn <-
  vec_watershed_rainAnn[dat_watershed_burnedForestLoss_fireRisk$watershed_ID]

# proportion of grass-dominated pixels in each watershed
raster_grass1999 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_herb_",
                1999, ".tif"))
raster_grass1999 <- trim(projectRaster(raster_grass1999, raster_dummy_master, method = 'ngb')); gc()
dat_grassCover <-
  data.frame(grassPct = values(raster_grass1999),
             watershed_ID = NA)
for(w in seq_along(list_gridIndices_watersheds)){   # assign watershed to each grass pixel
  dat_grassCover$watershed_ID[list_gridIndices_watersheds[[w]]] <-
    as.numeric(names(list_gridIndices_watersheds)[[w]])
}
rm(w)
gc()
dat_grassCover <- dat_grassCover[!is.na(dat_grassCover$watershed_ID),]
gc()
dat_grassCover$dummy <- 1
dat_grassCover$dummy_grassDom <-
  ifelse(dat_grassCover$grassPct >= 0.40, 1, 0)
dat_watershed_grassDom <-
  aggregate(dat_grassCover$dummy_grassDom,
            list(dat_grassCover$watershed_ID),
            sum, na.rm = TRUE)
colnames(dat_watershed_grassDom) <-
  c('watershed_ID', 'numPixels_grassDom')
dat_watershed_numPixels <-
  aggregate(dat_grassCover$dummy,
            list(dat_grassCover$watershed_ID),
            sum, na.rm = TRUE)
colnames(dat_watershed_numPixels) <-
  c('watershed_ID', 'numPixels_total')
dat_watershed_grassDom <- left_join(dat_watershed_grassDom,
                                    dat_watershed_numPixels,
                                    'watershed_ID')
dat_watershed_grassDom$proportion_grassDom <-
  dat_watershed_grassDom$numPixels_grassDom /
  dat_watershed_grassDom$numPixels_total

# number of grass and forest pixels per watershed
dat_watershed_numGrassForestPixels1999 <-
  aggregate(list(dat_pixels$isGrass1999,
                 dat_pixels$isForest1999),
            list(dat_pixels$watershed_ID),
            sum)
colnames(dat_watershed_numGrassForestPixels1999) <-
  c('watershed_ID', 'numGrassPixels1999', 'numForestPixels1999')

# combine data
dat_watershed <-
  left_join(dat_watershed_burnedForestLoss_totalPixels,
            dat_watershed_burnedForestLoss_fireRisk,
            'watershed_ID')
dat_watershed <-
  left_join(dat_watershed,
            dat_watershed_grassDom,
            'watershed_ID')
dat_watershed$numPixels_grassDom <- NULL
dat_watershed <-
  left_join(dat_watershed,
            dat_watershed_numGrassForestPixels1999,
            'watershed_ID')
rm(dat_watershed_numGrassForestPixels1999, dat_watershed_grassDom,
   dat_watershed_burnedForestLoss_fireRisk, dat_watershed_burnedForestLoss_totalPixels,
   dat_watershed_numPixels)

saveRDS(dat_watershed,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                      'dat_watershed.rds'))
dat_watershed <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'dat_watershed.rds'))

rm(dat_grassCover, dat_pixels, dat_pixels_burned, dat_watershed_burnedForestLoss_fireRisk,
   dat_watershed_burnedForestLoss_totalPixels, dat_watershed_grassDom,
   dat_watershed_numPixels)
gc()
