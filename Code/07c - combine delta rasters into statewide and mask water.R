
# This script combines county-level climate change deltas into statewide rasters,
# and masks water features.

library(raster)
library(sf)




##### load data #####

# list all delta rasters
list_rast <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance',
             full.names = TRUE, pattern = 'delta')

# separate by season
vec_wetMonths <- c(11,12,1,2,3,4)
vec_dryMonths <- c(5,6,7,8,9,10)
vec_months <- as.numeric(sapply(list_rast, function(char){substr(char, nchar(char) - 22, nchar(char) - 21)}))
list_rastDry <- list_rast[vec_months %in% vec_dryMonths]
list_rastWet <- list_rast[vec_months %in% vec_wetMonths]

# subset by scenario
list_master <-
  list(list_rast_sta45midDry = list_rastDry[grep('sta_rcp45_2070', list_rastDry)],
       list_rast_sta45midWet = list_rastWet[grep('sta_rcp45_2070', list_rastWet)],
       list_rast_sta45endDry = list_rastDry[grep('sta_rcp45_2100', list_rastDry)],
       list_rast_sta45endWet = list_rastWet[grep('sta_rcp45_2100', list_rastWet)],
       list_rast_sta85midDry = list_rastDry[grep('sta_rcp85_2070', list_rastDry)],
       list_rast_sta85midWet = list_rastWet[grep('sta_rcp85_2070', list_rastWet)],
       list_rast_sta85endDry = list_rastDry[grep('sta_rcp85_2100', list_rastDry)],
       list_rast_sta85endWet = list_rastWet[grep('sta_rcp85_2100', list_rastWet)],
       list_rast_dyn45endDry = list_rastDry[grep('dyn_rcp45_2100', list_rastDry)],
       list_rast_dyn45endWet = list_rastWet[grep('dyn_rcp45_2100', list_rastWet)],
       list_rast_dyn85endDry = list_rastDry[grep('dyn_rcp85_2100', list_rastDry)],
       list_rast_dyn85endWet = list_rastWet[grep('dyn_rcp85_2100', list_rastWet)])

rm(list_rast, list_rastDry, list_rastWet, vec_dryMonths, vec_months, vec_wetMonths)

# load landcover rasters for water masking
ras_lc <- raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                        "mhi_s0_baseline_names.tif"))
ras_bare <- raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                          "HI_FracLC_bare_2016.tif"))




##### create statewide rasters #####

# list islands for aggregating wet and dry seasons
vec_islands <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')

# aggregate rasters to statewide
list_statewide_rasters <- list()
for(s in 1:length(list_master)){
  
  #create max delta raster for each island
  list_rast_s_i <- list()
  for(i in 1:length(vec_islands)){
    
    # get rasters for island i in scenario s
    list_rast_i <- list_master[[s]][grep(vec_islands[[i]], list_master[[s]])]
    list_rast_i <- lapply(list_rast_i, raster)
    
    # create max raster from stack island i, scenario s
    stack_i <- stack(list_rast_i)
    list_rast_s_i[[i]] <- max(stack_i, na.rm = TRUE)
  }
  
  # merge rasters for each county into statewide raster
  list_statewide_rasters[[s]] <-
    do.call(raster::merge, list_rast_s_i)
  gc()
}

rm(i, s, list_rast_s_i, list_rast_i, stack_i, vec_islands)




##### mask water #####

# crop land cover raster
ras_bare_cropped <-
  crop(ras_bare, list_statewide_rasters[[1]])

# make dummy raster for masking
ras_bare_cropped_dummy <- ras_bare_cropped
ras_bare_cropped_dummy[ras_bare_cropped_dummy >= 0.99] <- 1
ras_bare_cropped_dummy[ras_bare_cropped_dummy < 1] <- 0

# mask water
list_statewide_rasters_waterMasked <- list()
for(i in 1:length(list_statewide_rasters)) {
  
  # load raster
  ras <- list_statewide_rasters[[i]]
  
  # mask
  ras_masked <- mask(ras, ras_bare_cropped_dummy,
                     maskvalue = 1, updatevalue = 0)
  
  # create raster
  list_statewide_rasters_waterMasked[[i]] <- ras_masked
  
  rm(ras, ras_masked)
  gc()
  print(i)
}

rm(ras_bare, ras_bare_cropped, ras_bare_cropped_dummy, i)




##### mask high risk on Oahu #####

# load mask
shp_mask <- read_sf(dsn = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/Oahu fire risk delta mask",
                    layer = 'oahu_delta_mask')

# change crs to match rasters
shp_mask <- st_transform(shp_mask, crs(list_statewide_rasters_waterMasked[[1]]))

# convert to raster
ras_mask <- rasterize(shp_mask, list_statewide_rasters_waterMasked[[1]])

# mask rasters
for(s in 1:length(list_statewide_rasters_waterMasked)){
  
  list_statewide_rasters_waterMasked[[s]] <- mask(list_statewide_rasters_waterMasked[[s]], ras_mask, maskvalue = 1, updatevalue = 0)
  gc()
}





##### write rasters - seasonal #####

# write rasters
for(s in 1:length(list_statewide_rasters_waterMasked)){
  
  model <- substr(names(list_master)[[s]], nchar(names(list_master)[[s]]) - 10, nchar(names(list_master)[[s]]) - 8)
  season <- tolower(substr(names(list_master)[[s]], nchar(names(list_master)[[s]]) - 2, nchar(names(list_master)[[s]])))
  rcp <- substr(names(list_master)[[s]], nchar(names(list_master)[[s]]) - 7, nchar(names(list_master)[[s]]) - 6)
  year <- ifelse(substr(names(list_master)[[s]], nchar(names(list_master)[[s]]) - 5, nchar(names(list_master)[[s]]) - 3) == 'mid', '2070', '2100')
  writeRaster(list_statewide_rasters_waterMasked[[s]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/',
                                'delta_max_future_fire_prob_',
                                model, '_rcp', rcp, '_', year, '_statewide_', season, '_waterMasked.tif'),
              overwrite = TRUE)
}




##### create annual from seasonal rasters then write #####

for(i in seq(1, length(list_statewide_rasters_waterMasked), 2)){
  
  # stack rasters for wet and dry seasons
  rasStack <- stack(list_statewide_rasters_waterMasked[[i]],
                    list_statewide_rasters_waterMasked[[i+1]])
  ras <- max(rasStack, na.rm = TRUE)
  
  # save
  model <- substr(names(list_master)[[i]], nchar(names(list_master)[[i]]) - 10, nchar(names(list_master)[[i]]) - 8)
  rcp <- substr(names(list_master)[[i]], nchar(names(list_master)[[i]]) - 7, nchar(names(list_master)[[i]]) - 6)
  year <- ifelse(substr(names(list_master)[[i]], nchar(names(list_master)[[i]]) - 5, nchar(names(list_master)[[i]]) - 3) == 'mid', '2070', '2100')
  writeRaster(list_statewide_rasters_waterMasked[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/',
                                'delta_max_future_fire_prob_',
                                model, '_rcp', rcp, '_', year, '_statewide_annual_waterMasked.tif'),
              overwrite = TRUE)
  gc()
}

rm(i)
