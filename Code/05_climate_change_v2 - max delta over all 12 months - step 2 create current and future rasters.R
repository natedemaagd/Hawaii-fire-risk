
# This script calculates average annual fire risk for each island and model, using monthly rasters
# The rasters zero out pixels with insignificant deltas based on SEs.

library(raster); library(ggplot2)

# list rasters
vec_filenamesCurrent <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                                   pattern = '.tif')
vec_filenamesFuture <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                                  pattern = '.tif')

# remove RCP 4.5 and SE rasters
vec_filenamesCurrent <-
  vec_filenamesCurrent[-c(grep('_SE', vec_filenamesCurrent))]
vec_filenamesFuture <-
  vec_filenamesFuture[-c(grep('_SE', vec_filenamesFuture))]
vec_filenamesFuture <- grep('rcp85', vec_filenamesFuture, value = TRUE)

# definitions
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')
vec_season <- c('Dry', 'Wet')
vec_seasonWet <- c('10','11','12','01','02','03','04')
vec_seasonDry <- c('05', '06', '07', '08', '09')
vec_year <- c('2070', '2100')
vec_model <- c('sta', 'dyn')




##### create max current probability rasters by season #####

for(i in 1:length(vec_islands)){
  for(s in 1:length(vec_season)){
    
    # get subset of filenames for island i
    vec_filenamesCurrent_is <-
      grep(vec_islands[[i]], vec_filenamesCurrent, value = TRUE)
    
    # get subset of these that are months in season s
    if(s == 1){
      months_in_season_s <-
        paste0('month', vec_seasonDry)
    } else {
      months_in_season_s <-
        paste0('month', vec_seasonWet)
    }
    vec_filenamesCurrent_is <-
      grep(paste(months_in_season_s, collapse = "|"), 
           vec_filenamesCurrent_is, value = TRUE)
    rm(months_in_season_s)
    
    # read in rasters
    list_rasters <- lapply(vec_filenamesCurrent_is, function(r){
      raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                    r))
    })
    
    # stack rasters
    rasterStack <- stack(list_rasters)
    rm(list_rasters); gc()
    
    # get max fire prob
    beginCluster(8)
    raster_final <- clusterR(rasterStack, calc, args = list(max))
    endCluster()
    
    # write raster
    writeRaster(raster_final, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
                                                'mean current fire prob ', vec_islands[[i]], ' ', vec_season[[s]], ' season.tif'),
                overwrite = TRUE)
    
    gc()
    
  }
}




##### create current max probability - overall annual #####

for(i in 1:length(vec_islands)){
    
    # get subset of filenames for island i
    vec_filenamesCurrent_is <-
      grep(vec_islands[[i]], vec_filenamesCurrent, value = TRUE)
    
    # read in rasters
    list_rasters <- lapply(vec_filenamesCurrent_is, function(r){
      raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                    r))
    })
    
    # stack rasters
    rasterStack <- stack(list_rasters)
    rm(list_rasters); gc()
    
    # get max fire prob
    beginCluster(8)
    raster_final <- clusterR(rasterStack, calc, args = list(max))
    endCluster()
    
    # write raster
    writeRaster(raster_final, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
                                                'mean current fire prob ', vec_islands[[i]], ' overall annual.tif'),
                overwrite = TRUE)
    
    gc()
}

rm(raster_final, rasterStack, vec_filenamesCurrent_is)
gc()




##### create future mean probability by season #####

# for each island, season, and model, find max delta for each pixel
for(i in 1:length(vec_islands)){
  for(s in 1:length(vec_season)){
    for(y in 1:length(vec_year)){
      for(m in 1:length(vec_model)){
        
        
        
        ### subset filenames ###
        
        # list filenames for island i
        vec_filenamesFuture_isy <- grep(vec_islands[[i]], vec_filenamesFuture,
                                       value = TRUE)
        
        # subset filenames to include only season s
        if(vec_season[[s]] == 'Wet'){
          patterns <- paste0('month', vec_seasonWet)
          vec_filenamesFuture_isy <- grep(paste(patterns, collapse = "|"), 
                                         vec_filenamesFuture_isy, value = TRUE)  # wet season
        } else {
          patterns <- paste0('month', vec_seasonDry)
          vec_filenamesFuture_isy <- grep(paste(patterns, collapse = "|"),
                                         vec_filenamesFuture_isy, value = TRUE)  # dry season
        }
        rm(patterns)
        
        # subset filenames to include only year y
        vec_filenamesFuture_isy <- grep(vec_year[[y]], vec_filenamesFuture_isy,
                                       value = TRUE)
        
        # subset filenames to include only model m
        if(vec_model[[m]] == 'dyn' & vec_year[[y]] == '2070'){
          next
        } else {
          vec_filenamesFuture_isy <- grep(vec_model[[m]], vec_filenamesFuture_isy,
                                         value = TRUE)
        }
        
        
        
        ### read rasters from filenames ###
        
        list_rasters <-
          lapply(vec_filenamesFuture_isy, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                          r))
          })
        
        
        
        ### perform raster calculations ###
        
        # stack rasters
        rasterStack <- stack(list_rasters)
        rm(list_rasters); gc()
        
        # find max month-wise delta for each pixel
        beginCluster(8)
        r_maxDelta <- clusterR(rasterStack, calc, args = list(max))
        endCluster()
        
        # write raster
        writeRaster(r_maxDelta,
                    filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
                                      'mean future fire prob', ' ', vec_islands[[i]], ' ', vec_year[[y]], ' ', vec_season[[s]], ' ', vec_model[[m]], '.tif'),
                    overwrite = TRUE)
        
        gc()
        
      }
    }
  }
}

rm(r_maxDelta, rasterStack, vec_filenamesFuture_isy)
gc()




##### create future mean probability - overall annual #####

for(i in 1:length(vec_islands)){
  for(m in 1:length(vec_model)){
    for(y in 1:length(vec_year)){
        
        
        
        ### subset filenames ###
        
        # list filenames for island i
        vec_filenamesFuture_isy <- grep(vec_islands[[i]], vec_filenamesFuture,
                                        value = TRUE)
        
        # subset filenames to include only year y
        vec_filenamesFuture_isy <- grep(vec_year[[y]], vec_filenamesFuture_isy,
                                        value = TRUE)
        
        # subset filenames to include only model m
        if(vec_model[[m]] == 'dyn' & vec_year[[y]] == '2070'){
          next
        } else {
          vec_filenamesFuture_isy <- grep(vec_model[[m]], vec_filenamesFuture_isy,
                                          value = TRUE)
        }
        
        
        
        ### read rasters from filenames ###
        
        list_rasters <-
          lapply(vec_filenamesFuture_isy, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                          r))
          })
        
        
        
        ### perform raster calculations ###
        
        # stack rasters
        rasterStack <- stack(list_rasters)
        rm(list_rasters); gc()
        
        # find max month-wise delta for each pixel
        beginCluster(8)
        r_maxDelta <- clusterR(rasterStack, calc, args = list(max))
        endCluster()
        
        # write raster
        writeRaster(r_maxDelta,
                    filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
                                      'mean future fire prob', ' ', vec_islands[[i]], ' ', vec_year[[y]], ' overall annual ', vec_model[[m]], '.tif'),
                    overwrite = TRUE)
        
        gc()
        
    }
  }
}
