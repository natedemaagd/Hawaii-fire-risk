
# This script estimates percent forest lost within fire footprints as a function
# of fire risk within the footprints.

library(raster)
library(doParallel)
library(sf)
registerDoParallel(cores = 3)


# load previously-generated rasters and fire indices
raster_dummy_master <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                'raster_dummy_master.tif'))
list_gridIndices_fires <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_fires.rds'))
list_gridIndices_forestDominantByYear <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'list_gridIndices_forestDominantByYear.rds'))
vec_fireYear <-
  as.numeric(substr(names(list_gridIndices_fires),
                    1, 4)
  )
vec_landcoverYear <- 1999:2016

# load fire perimeters
sf_firePerims <-
  read_sf(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/",
                 "2019_1999_Hawaii_Fire_Perimeters.shp")
  )




##### generate dataset #####

# for each fire perimeter, find total number of forest pixels in year before fire
list_forestPixelLoss <- list()
for(f in 1:length(list_gridIndices_fires)){
  
  # if fire occurred in 1999 or 2019, skip it
  if(vec_fireYear[[f]] %in% c(1999, 2016:2019)){
    
    list_forestPixelLoss[[f]] <- NA
    
  } else {
    
    # get year of fire
    f_fireYear <- vec_fireYear[[f]]
    
    # find intersection of pixel IDs in fire perimeter f and forest pixels in
    # year prior to fire and year after fire
    f_forestBeforeFire <-
      intersect(unlist(list_gridIndices_fires[f]),
                unlist(list_gridIndices_forestDominantByYear[which(vec_landcoverYear ==
                                                              f_fireYear - 1)]
                       )
                )
    f_forestAfterFire <-
      intersect(unlist(list_gridIndices_fires[f]),
                unlist(list_gridIndices_forestDominantByYear[which(vec_landcoverYear ==
                                                              f_fireYear + 1)]
                       )
                )
    
    # return data
    list_forestPixelLoss[[f]] <-
      c(length(f_forestBeforeFire),
        length(f_forestAfterFire)
        )
    
    if(length(f_forestBeforeFire) == 0){
      list_forestPixelLoss[[1]] <- 0
    }
    if(length(f_forestAfterFire) == 0){
      list_forestPixelLoss[[2]] <- 0
    }
    
    names(list_forestPixelLoss[[f]]) <-
      c('forestPixels_beforeFire', 'forestPixels_afterFire')
    
    
  }
}
rm(f, f_fireYear, f_forestAfterFire, f_forestBeforeFire)

# stack dataset
dat_forestLoss <-
  as.data.frame(do.call(rbind, list_forestPixelLoss))
colnames(dat_forestLoss) <-
  c('forestPixels_beforeFire', 'forestPixels_afterFire')

# calculate pixels lost and proportion lost
dat_forestLoss$forestPixels_change <-
  with(dat_forestLoss,
       forestPixels_afterFire - forestPixels_beforeFire)
dat_forestLoss$forestPixels_pctChange <-
  with(dat_forestLoss,
       forestPixels_change/forestPixels_beforeFire) * 100

rm(list_forestPixelLoss, list_gridIndices_forestDominantByYear)




##### add fire risk to forest loss data #####

# create new data.frame columns to store data
dat_forestLoss$fireProb_mean <- NA
dat_forestLoss$fireProb_median <- NA
dat_forestLoss$fireProb_75thPctile <- NA

# load fire risk raster for year-month of fire and get statistics of
# values within fire perimeter
for(f in 1:length(list_gridIndices_fires)){
  
  # get year-month of fire
  f_yearMonth <-
    substr(names(list_gridIndices_fires)[[f]],
           1,7)
  
  # load rasters for the year-month (all islands)
  vec_rasterNames <-
    list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
               pattern = gsub('_', '', f_yearMonth),
               full.names = TRUE)
  list_rastersFireRisk <-
    lapply(vec_rasterNames, raster)
  
  # merge rasters to create statewide raster
  f_raster <- do.call(merge, list_rastersFireRisk)
  
  # isolate raster to fire perimeter f
  f_raster <-
    mask(f_raster, sf_firePerims[1,])
  
  # get statistics of fire probabilities
  dat_forestLoss$fireProb_mean[[f]] <-
    mean(values(f_raster),
         na.rm = TRUE)
  dat_forestLoss$fireProb_median[[f]] <-
    median(values(f_raster),
           na.rm = TRUE)
  dat_forestLoss$fireProb_75thPctile[[f]] <-
    quantile(values(f_raster), probs = 0.75,
             na.rm = TRUE)
  gc()
  
}
