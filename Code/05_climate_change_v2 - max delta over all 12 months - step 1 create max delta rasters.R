
# this script takes all fire risk delta rasters and finds the max delta for each pixel among all 12 months, then combines them into one map per island
# note it loads delta rasters where insignificant deltas are zeroed out, so rasters written here have significant deltas zeroed out as well

library(raster); library(ggplot2); library(snow)

# list delta raster filenames
vec_deltaFilenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/',
                                 pattern= '.tif')
vec_deltaFilenames <- grep('rcp85', vec_deltaFilenames, value = TRUE)  # remove RCP 4.5 filenames

# define islands and season
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')
vec_season <- c('Dry', 'Wet')
vec_seasonWet <- c('10','11','12','01','02','03','04')
vec_seasonDry <- c('05', '06', '07', '08', '09')
vec_year <- c('2070', '2100')
vec_model <- c('sta', 'dyn')  # statistical and dynamical models




##### create max delta rasters separated by wet and dry season #####

# for each island, season, and model, find max delta for each pixel
for(i in 1:length(vec_islands)){
  for(s in 1:length(vec_season)){
    for(y in 1:length(vec_year)){
      for(m in 1:length(vec_model)){
        
        
        
        ### subset filenames ###
        
        # list filenames for island i
        vec_deltaFilenames_isy <- grep(vec_islands[[i]], vec_deltaFilenames,
                                       value = TRUE)
        
        # subset filenames to include only season s
        if(vec_season[[s]] == 'Wet'){
          patterns <- paste0('month', vec_seasonWet)
          vec_deltaFilenames_isy <- grep(paste(patterns, collapse = "|"), 
                                         vec_deltaFilenames_isy, value = TRUE)  # wet season
        } else {
          patterns <- paste0('month', vec_seasonDry)
          vec_deltaFilenames_isy <- grep(paste(patterns, collapse = "|"),
                                         vec_deltaFilenames_isy, value = TRUE)  # dry season
        }
        
        # subset filenames to include only year y
        vec_deltaFilenames_isy <- grep(vec_year[[y]], vec_deltaFilenames_isy,
                                       value = TRUE)
        
        # subset filenames to include only model m
        if(vec_model[[m]] == 'dyn' & vec_year[[y]] == '2070'){
          next
        } else {
          vec_deltaFilenames_isy <- grep(vec_model[[m]], vec_deltaFilenames_isy,
                                         value = TRUE)
        }
        
        
        
        ### read rasters from filenames ###
        
        list_rasters <-
          lapply(vec_deltaFilenames_isy, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/',
                          r))
          })
        
        
        
        ### perform raster calculations ###
        
        # stack rasters
        rasterStack <- stack(list_rasters)
        rm(list_rasters); gc()
        
        # find max month-wise delta for each pixel
        beginCluster(5)
        r_maxDelta <- clusterR(rasterStack, calc, args = list(max))
        endCluster()
        
        # write raster
        writeRaster(r_maxDelta,
                    filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                                      'max delta', ' ', vec_islands[[i]], ' ', vec_year[[y]], ' ', vec_season[[s]], ' ', vec_model[[m]], '.tif'),
                    overwrite = TRUE)
        
        gc()
        
      }
    }
  }
}




##### create max delta rasters - total annual #####

# list filenames - rasters separated by wet-dry season
vec_deltaFilenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                                 pattern = '.tif')

# for each island, season, and model, find max delta for each pixel
for(i in 1:length(vec_islands)){
    for(y in 1:length(vec_year)){
      for(m in 1:length(vec_model)){
        
        
        
        ### subset filenames ###
        
        # list filenames for island i
        vec_deltaFilenames_isy <- grep(vec_islands[[i]], vec_deltaFilenames,
                                       value = TRUE)
        
        # subset filenames to include only year y
        vec_deltaFilenames_isy <- grep(vec_year[[y]], vec_deltaFilenames_isy,
                                       value = TRUE)
        
        # subset filenames to include only model m
        if(vec_model[[m]] == 'dyn' & vec_year[[y]] == '2070'){
          next
        } else {
          vec_deltaFilenames_isy <- grep(vec_model[[m]], vec_deltaFilenames_isy,
                                         value = TRUE)
        }
        
        
        
        ### read rasters from filenames ###
        
        list_rasters <-
          lapply(vec_deltaFilenames_isy, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                          r))
          })
        
        
        
        ### perform raster calculations ###
        
        # stack rasters
        rasterStack <- stack(list_rasters)
        rm(list_rasters); gc()
        
        # find max month-wise delta for each pixel
        beginCluster(5)
        r_maxDelta <- clusterR(rasterStack, calc, args = list(max))
        endCluster()
        
        # write raster
        writeRaster(r_maxDelta,
                    filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                                      'max delta ', vec_islands[[i]], ' ', vec_year[[y]], ' overall annual ', vec_model[[m]], '.tif'),
                    overwrite = TRUE)
        
        gc()
        
      }
    }
}

