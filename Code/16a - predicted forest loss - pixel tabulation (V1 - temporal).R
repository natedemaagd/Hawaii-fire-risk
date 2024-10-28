
# This script regresses forest pixels lost per fire onto proportion of grass-dominate
# pixels in each watershed, mean annual rainfall, and the upper quartile of fire risk
# at the time of fire.

library(raster); library(sf); library(pscl)
library(ggplot2)
library(doParallel)
library(dplyr)
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




##### by fire-watershed combo, find number of pixels that lost 40%+ of their forest #####

# create vector of years fires occurred
vec_fireYear <-
  as.numeric(substr(names(list_gridIndices_fires),
                    1, 4)
  )

# create vector of landcover raster years
vec_landcoverYear <- 1999:2016

# # Loop through fires - each main element is a fire. Each sub-element is number of
# # forest-dominant pixels that lose more than 40% of their forest within each watershed.
# # Watershed is named according to its index in `list_gridIndices_watersheds`.
# a <- Sys.time()
# list_pixelLevelForestLoss <- list()
# for(f in 173:length(list_gridIndices_fires)){
# #for(f in seq_along(list_gridIndices_fires)){
# 
#   list_pixelLevelForestLoss[[f]] <- list()
# 
#   if(vec_fireYear[[f]] <= 1999 | vec_fireYear[[f]] >= 2016 |
#      vec_fireYear[[f]] %in% 2013:2015){
# 
#     list_pixelLevelForestLoss[[f]] <- NA  # landcover data only for 1999-2016, so skip if fire outside this range
# 
#   } else {
# 
#     # define year of fire
#     yearOfFire <- vec_fireYear[[f]]
# 
#     # load landcover rasters before and after fire
#     ras_before <-
#       raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/",
#                     "HI_FracLC_wood_",
#                     yearOfFire - 1,
#                     ".tif"))
#     ras_after <-
#       raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/",
#                     "HI_FracLC_wood_",
#                     yearOfFire + 1,
#                     ".tif"))
# 
#     # get difference raster
#     ras_difference <- overlay(ras_after, ras_before,
#                               fun = function(x, y){return(x - y)})
#     gc()
# 
#     # reproject difference raster to match master raster
#     ras_difference <- trim(ras_difference); gc()
#     ras_difference <- projectRaster(ras_difference, raster_dummy_master,
#                                     method = 'ngb')
#     gc()
# 
#     # find intersection of pixels that are in fire f AND forest-dominant before the fire
#     vec_pixelsForestDominant <-
#       list_gridIndices_forestDominantByYear[[which(vec_landcoverYear == yearOfFire - 1)]]
#     vec_pixelsFirePerimeter <-
#       list_gridIndices_fires[[f]]
#     vec_pixelsFireForestDominant <- intersect(vec_pixelsForestDominant,
#                                               vec_pixelsFirePerimeter)
#     gc()
# 
#     # get watershed indices of each pixel in the intersection `vec_pixelsFireForestDominant`
#     vec_firePixels_watershedIndices <-
#       list_fireGrid_watershed_index[[f]]
#     vec_firePixels_watershedIndices <-
#       vec_firePixels_watershedIndices[vec_pixelsFirePerimeter %in% vec_pixelsFireForestDominant]
# 
#     # split `vec_pixelsFireForestDominant` by watershed
#     list_pixelsFireForestDominant_byWatershed <-
#       split(vec_pixelsFireForestDominant,
#             vec_firePixels_watershedIndices)
# 
#     # count number of forest-dominant pixels in each watershed that lost at least 20% of their forest
#     vec_forestDifference <- values(ras_difference)
#     list_pixelLevelForestLoss[[f]] <- list()
#     for(w in seq_along(list_pixelsFireForestDominant_byWatershed)){
#       vec_significantForestLoss <- vec_forestDifference[list_pixelsFireForestDominant_byWatershed[[w]]]
#       list_pixelLevelForestLoss[[f]][[w]] <-
#         length(vec_significantForestLoss[vec_significantForestLoss <= -0.40])
#     }
#     names(list_pixelLevelForestLoss[[f]]) <-
#       names(list_pixelsFireForestDominant_byWatershed)
# 
#     rm(ras_before, ras_after, yearOfFire, ras_difference,
#        vec_pixelsForestDominant, vec_pixelsFirePerimeter, vec_pixelsFireForestDominant,
#        vec_forestDifference, vec_significantForestLoss,
#        vec_firePixels_watershedIndices)
#     gc()
#     print(paste0(f, ' - ', Sys.time()))
# 
#   }
# }
# Sys.time() - a
# rm(f, w)
# gc()
# 
# # name list elements
# names(list_pixelLevelForestLoss) <- 1:length(list_pixelLevelForestLoss)
# 
# # if length of list element is 0 (missing data), replace with NA
# for(i in 1:length(list_pixelLevelForestLoss)){
#   if(length(list_pixelLevelForestLoss[[i]]) == 0){
#     list_pixelLevelForestLoss[[i]] <- NA
#   }
# }
# 
# saveRDS(list_pixelLevelForestLoss,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'list_pixelLevelForestLoss_40pctLoss_raw.rds'))

list_pixelLevelForestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_pixelLevelForestLoss_40pctLoss_raw.rds'))




##### initiate dataframe - observations are fire-watershed combos #####

# initiate data.frame
dat_fire <- data.frame(
  fireWatershedID = names(unlist(list_pixelLevelForestLoss))
)
dat_fire$fireID <-
  as.numeric(sub("\\.[^.]*$", "", dat_fire$fireWatershedID))
dat_fire$watershedID <-
  as.numeric(gsub(".*\\.", "", dat_fire$fireWatershedID))
dat_fire$watershedID <-
  with(dat_fire,
       ifelse(watershedID == fireWatershedID, NA, watershedID)
  )
dat_fire$yearOfFire <- NA
for(i in 1:nrow(dat_fire)){
  dat_fire$yearOfFire[[i]] <- vec_fireYear[[dat_fire$fireID[[i]]]]
}




##### fire risk data - 75th percentile of monthly max fire risk within watershed in year of fire (dry season months only) #####

# # for each fire-watershed combo (observation) calculate 75th percentile of same-year fire risk
# vec_drySeasonMonths <- c('05', '06', '07', '08', '09')
# dat_fire$fireRisk_75thPctileMax <- NA
# for(i in 1:nrow(dat_fire)){
#   
#   if(is.na(dat_fire$watershedID[[i]])){
#     
#     dat_fire$fireRisk_75thPctileMax[[i]] <- NA
#     
#   } else {
#     
#     # define year of fire
#     yearOfFire <- dat_fire[i, 'yearOfFire']
#     
#     # define year-months of rasters to load
#     vec_rasterYearMonths <-
#       paste0(yearOfFire, vec_drySeasonMonths)
#     
#     # get final raster names for year-months
#     vec_rasterNames <-
#       list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
#                  pattern = '.tif',
#                  full.names = TRUE)
#     vec_rasterNames <-
#       grep(pattern = paste(vec_rasterYearMonths, collapse = "|"),
#            x = vec_rasterNames,
#            value = TRUE)
#     rm(vec_rasterYearMonths)
#     
#     # load dry season fire risk rasters for year of fire
#     list_rasFireRisk <-
#       lapply(vec_rasterNames, raster)
#     
#     # split rasters by island and stack
#     list_rasFireRiskStacks <-
#       list(stack(list_rasFireRisk[1:5]),
#            stack(list_rasFireRisk[6:10]),
#            stack(list_rasFireRisk[11:15]),
#            stack(list_rasFireRisk[16:20])
#       )
#     rm(list_rasFireRisk); gc()
#     
#     # get max fire risk of each pixel
#     list_rasFireRiskMax <-
#       lapply(list_rasFireRiskStacks,
#              function(s){max(s)})
#     rm(list_rasFireRiskStacks); gc()
#     
#     # merge maxed rasters into one raster
#     raster_fireRiskMax <- do.call(merge, list_rasFireRiskMax)
#     raster_fireRiskMax <- trim(raster_fireRiskMax)
#     rm(list_rasFireRiskMax); gc()
#     
#     # get pixels associated with watershed i
#     vec_watershedPixels <-
#       unlist(list_gridIndices_watersheds[names(list_gridIndices_watersheds) ==
#                                            dat_fire$watershedID[[i]]
#                                          ]
#              )
#     
#     # get fire risks associated with these pixels
#     vec_fireRiskVals <- values(raster_fireRiskMax)[vec_watershedPixels]
#     
#     # return 75th percentile of these fire risks as dataframe value
#     dat_fire$fireRisk_75thPctileMax[[i]] <-
#       quantile(vec_fireRiskVals, probs = 0.75)[[1]]
#     
#     gc()
#     
#   }
# }
# 
# rm(i, yearOfFire, vec_rasterNames, vec_watershedPixels,
#    vec_fireRiskVals)
# 
# saveRDS(dat_fire,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
#                       'dat_fire_withFireRisk.rds')
#         )

dat_fire <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                           'dat_fire_withFireRisk.rds')
                    )

# add forest pixels lost
dat_fire$forestPixelsLost <-
  unlist(list_pixelLevelForestLoss)
rm(list_pixelLevelForestLoss)
dat_fire <-
  dat_fire[c("fireWatershedID", "fireID", "watershedID", "yearOfFire",
             "forestPixelsLost", "fireRisk_75thPctileMax")]




##### add number of forest and grass pixels year before fire #####

# add forest and grass pixels
dat_fire$numForestPixelsBeforeFire <- NA
dat_fire$numGrassPixelsBeforeFire <- NA
for(i in 1:nrow(dat_fire)){
  
  # get watershed index and year of fire
  i_watershed <- dat_fire$watershedID[[i]]
  i_year      <- dat_fire$yearOfFire[[i]]
  
  if(is.na(i_watershed) | is.na(i_year)){
    
    next
    
  } else {
    
    # get grid indices of watershed i_watershed
    i_watershed_gridIndices <-
      unlist(list_gridIndices_watersheds[i_watershed])
    
    # get grid indices of forest and grass pixels in year i_year
    i_forestPixels_gridIndices <-
      unlist(list_gridIndices_forestDominantByYear[which(vec_landcoverYear == i_year)])
    i_grassPixels_gridIndices <-
      unlist(list_gridIndices_grassDominantByYear[which(vec_landcoverYear == i_year)])
    
    # find intersection of grass/forest pixels with watershed pixels
    i_watershed_forestPixels <-
      intersect(i_watershed_gridIndices, i_forestPixels_gridIndices)
    i_watershed_grassPixels <-
      intersect(i_watershed_gridIndices, i_grassPixels_gridIndices)
    
    # return number of pixels
    dat_fire$numForestPixelsBeforeFire[[i]] <-
      length(i_watershed_forestPixels)
    dat_fire$numGrassPixelsBeforeFire[[i]] <-
      length(i_watershed_grassPixels)
    
    rm(i_forestPixels_gridIndices, i_grassPixels_gridIndices, i_watershed,
       i_watershed_forestPixels, i_watershed_grassPixels, i_watershed_gridIndices,
       i_year)
    gc()
    
  }
}

rm(i)




##### add rainfall #####

# load data from non-temporal model
dat_aggregated <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'dat_watershed.rds'))
dat_watershedRainfall <-
  dat_aggregated[c('watershed_ID', 'rainfall_mmMeanAnn')]
colnames(dat_watershedRainfall) <-
  c('watershedID', 'rainfall_mmMeanAnn')

# merge data
dat_fire <- left_join(dat_fire, dat_watershedRainfall, 'watershedID')

# save data
saveRDS(dat_fire,
        paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
               'dat_watershed_temporal.rds'))
