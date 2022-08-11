library(raster); library(ggplot2)



##### current fire prob - mean #####

# list current fire prob rasters without SE rasters
raster_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean', pattern = '.tif')
raster_filenames <- sapply(raster_filenames, function(x) gsub(pattern = '_SE', replacement = '', x))
raster_filenames <- raster_filenames[!duplicated(raster_filenames)]

# keep only filenames for islands/island groups we care about (remove duplicates)
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
raster_filenames_split <- sapply(raster_filenames, function(x) strsplit(x, '_'))

# get unique groups
raster_groups <- unique(sapply(raster_filenames_split, function(x) x[[5]]))

# # for kauai, keep only KauaiOnlyModel
# kauai_files <- sapply(raster_filenames_split, function(x) x[[5]] == 'Kauai')
# kauai_files <- raster_filenames_split[kauai_files]
# kauai_files <- kauai_files[seq(2, 24, 2)]
# kauai_files <- names(kauai_files)
# 
# # get all non-kauai files
# nonkauai_files <- sapply(raster_filenames_split, function(x) x[[5]] != 'Kauai')
# nonkauai_files <- raster_filenames[nonkauai_files]
# 
# # merge kauai and non-kauai filenames
# raster_filenames <- c(nonkauai_files, kauai_files)
# rm(raster_filenames_split, kauai_files, nonkauai_files)

# get raster filename island group
raster_filenames_islandGroup <- strsplit(raster_filenames, '_')
raster_filenames_islandGroup <- sapply(raster_filenames_islandGroup, function(x) x[[5]])

# for each island group, load corresponding rasters
raster_list_dry <- list()
raster_list_wet <- list()
for(i in 1:length(raster_groups)){
  
  # load island group i's rasters
  rasters <- raster_filenames[raster_filenames_islandGroup == raster_groups[[i]]]
  rasters <- lapply(rasters, function(x) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/', x)))
  
  # separate by wet and dry season
  rasters_wet <- rasters[c(11,12,1,2,3,4)]
  rasters_dry <- rasters[c(5,6,7,8,9,10)]
  rm(rasters); gc()
  
  # get wet and dry season averages
  rasters_wet <- stack(rasters_wet); rasters_wet_mean <- calc(rasters_wet, mean)
  rasters_dry <- stack(rasters_dry); rasters_dry_mean <- calc(rasters_dry, mean)
  
  # # plot using ggplot
  # rasters_dry_mean_df <- as.data.frame(rasters_dry_mean, xy=TRUE)
  # rasters_dry_mean_df <- rasters_dry_mean_df[!is.na(rasters_dry_mean_df$layer),]
  # ggplot(data = rasters_dry_mean_df) + geom_tile(aes(x=x, y=y, color = layer, fill = layer))
  
  # save rasters to list
  raster_list_dry[[i]] <- rasters_dry_mean; names(raster_list_dry)[[i]] <- raster_groups[[i]]
  raster_list_wet[[i]] <- rasters_wet_mean; names(raster_list_wet)[[i]] <- raster_groups[[i]]
}
gc()

# combine all wet and dry season rasters into statewide rasters
names(raster_list_dry) <- names(raster_list_wet) <- c('x', 'y')
raster_statewide_dry <- do.call(raster::merge, raster_list_dry)
raster_statewide_wet <- do.call(raster::merge, raster_list_wet)

# export rasters
writeRaster(raster_statewide_dry, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_dry_statewide.tif', overwrite = TRUE)
writeRaster(raster_statewide_wet, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_wet_statewide.tif', overwrite = TRUE)

for(i in 1:length(raster_list_dry)){
  
  writeRaster(raster_list_dry[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_dry_', raster_groups[[i]], '.tif'), overwrite = TRUE)
  writeRaster(raster_list_wet[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_wet_', raster_groups[[i]], '.tif'), overwrite = TRUE)
  
}




##### current fire prob - median #####

# list current fire prob rasters without SE rasters
raster_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean', pattern = '.tif')
raster_filenames <- sapply(raster_filenames, function(x) gsub(pattern = '_SE', replacement = '', x))
raster_filenames <- raster_filenames[!duplicated(raster_filenames)]

# keep only filenames for islands/island groups we care about (remove duplicates)
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
raster_filenames_split <- sapply(raster_filenames, function(x) strsplit(x, '_'))

# get unique groups
raster_groups <- unique(sapply(raster_filenames_split, function(x) x[[5]]))

# # for kauai, keep only KauaiOnlyModel
# kauai_files <- sapply(raster_filenames_split, function(x) x[[5]] == 'Kauai')
# kauai_files <- raster_filenames_split[kauai_files]
# kauai_files <- kauai_files[seq(2, 24, 2)]
# kauai_files <- names(kauai_files)
# 
# # get all non-kauai files
# nonkauai_files <- sapply(raster_filenames_split, function(x) x[[5]] != 'Kauai')
# nonkauai_files <- raster_filenames[nonkauai_files]
# 
# # merge kauai and non-kauai filenames
# raster_filenames <- c(nonkauai_files, kauai_files)
# rm(raster_filenames_split, kauai_files, nonkauai_files)

# get raster filename island group
raster_filenames_islandGroup <- strsplit(raster_filenames, '_')
raster_filenames_islandGroup <- sapply(raster_filenames_islandGroup, function(x) x[[5]])

# for each island group, load corresponding rasters
raster_list_dry <- list()
raster_list_wet <- list()
for(i in 1:length(raster_groups)){
  
  # load island group i's rasters
  rasters <- raster_filenames[raster_filenames_islandGroup == raster_groups[[i]]]
  rasters <- lapply(rasters, function(x) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/', x)))
  
  # separate by wet and dry season
  rasters_wet <- rasters[c(11,12,1,2,3,4)]
  rasters_dry <- rasters[c(5,6,7,8,9,10)]
  rm(rasters); gc()
  
  # get wet and dry season averages
  rasters_wet <- stack(rasters_wet); rasters_wet_mean <- calc(rasters_wet, median)
  rasters_dry <- stack(rasters_dry); rasters_dry_mean <- calc(rasters_dry, median)
  
  # # plot using ggplot
  # rasters_dry_mean_df <- as.data.frame(rasters_dry_mean, xy=TRUE)
  # rasters_dry_mean_df <- rasters_dry_mean_df[!is.na(rasters_dry_mean_df$layer),]
  # ggplot(data = rasters_dry_mean_df) + geom_tile(aes(x=x, y=y, color = layer, fill = layer))
  
  # save rasters to list
  raster_list_dry[[i]] <- rasters_dry_mean; names(raster_list_dry)[[i]] <- raster_groups[[i]]
  raster_list_wet[[i]] <- rasters_wet_mean; names(raster_list_wet)[[i]] <- raster_groups[[i]]
}
gc()

# combine all wet and dry season rasters into statewide rasters
names(raster_list_dry) <- names(raster_list_wet) <- c('x', 'y')
raster_statewide_dry <- do.call(raster::merge, raster_list_dry)
raster_statewide_wet <- do.call(raster::merge, raster_list_wet)

# export rasters
writeRaster(raster_statewide_dry, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_current_fire_prob_dry_statewide.tif', overwrite = TRUE)
writeRaster(raster_statewide_wet, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_current_fire_prob_wet_statewide.tif', overwrite = TRUE)

for(i in 1:length(raster_list_dry)){
  
  writeRaster(raster_list_dry[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_current_fire_prob_dry_', raster_groups[[i]], '.tif'), overwrite = TRUE)
  writeRaster(raster_list_wet[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_current_fire_prob_wet_', raster_groups[[i]], '.tif'), overwrite = TRUE)
  
}




##### deltas - mean #####

# load delta files (significant deltas)
delta_files <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance', pattern = '.tif')

# separate files by model and island, find all unique model-island combinations
delta_files_separated <- strsplit(delta_files, '_')
delta_modelsAndIslands <- sapply(delta_files_separated, function(x) paste(x[1:10], collapse = '_'))
delta_modelsAndIslands <- delta_modelsAndIslands[!duplicated(delta_modelsAndIslands)]
rm(delta_files_separated)

# for each model-island, get create mean delta raster by season
raster_list_dry <- list()
raster_list_wet <- list()
for(i in 1:length(delta_modelsAndIslands)){
  
  # list then load monthly raster names for the model-island combo i
  rasters <- grep(delta_modelsAndIslands[[i]], delta_files, value=TRUE)
  rasters <- lapply(rasters, function(x) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/', x)))
  
  # separate by wet and dry season
  rasters_wet <- rasters[c(11,12,1,2,3,4)]
  rasters_dry <- rasters[c(5,6,7,8,9,10)]
  rm(rasters); gc()
  
  # get wet and dry season averages
  rasters_wet <- stack(rasters_wet); rasters_wet_mean <- calc(rasters_wet, mean)
  rasters_dry <- stack(rasters_dry); rasters_dry_mean <- calc(rasters_dry, mean)
  
  # # plot using ggplot
  # rasters_dry_mean_df <- as.data.frame(rasters_dry_mean, xy=TRUE)
  # rasters_dry_mean_df <- rasters_dry_mean_df[!is.na(rasters_dry_mean_df$layer),]
  # ggplot(data = rasters_dry_mean_df) + geom_tile(aes(x=x, y=y, color = layer, fill = layer))
  
  # save rasters to list
  raster_list_dry[[i]] <- rasters_dry_mean; names(raster_list_dry)[[i]] <- delta_modelsAndIslands[[i]]
  raster_list_wet[[i]] <- rasters_wet_mean; names(raster_list_wet)[[i]] <- delta_modelsAndIslands[[i]]
  
  gc()
  
}
gc()

# # combine all wet and dry season rasters into statewide rasters
# names(raster_list_dry) <- names(raster_list_wet) <- c('x', 'y')
# raster_statewide_dry <- do.call(raster::merge, raster_list_dry)
# raster_statewide_wet <- do.call(raster::merge, raster_list_wet)
# 
# # export rasters
# writeRaster(raster_statewide_dry, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_delta_dry_statewide.tif', overwrite = TRUE)
# writeRaster(raster_statewide_wet, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_delta_wet_statewide.tif', overwrite = TRUE)

for(i in 1:length(raster_list_dry)){
  
  writeRaster(raster_list_dry[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/', delta_modelsAndIslands[[i]], '_dry.tif'), overwrite = TRUE)
  writeRaster(raster_list_wet[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/', delta_modelsAndIslands[[i]], '_wet.tif'), overwrite = TRUE)
  
}




##### deltas - median #####

# load delta files (significant deltas)
delta_files <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance', pattern = '.tif')

# separate files by model and island, find all unique model-island combinations
delta_files_separated <- strsplit(delta_files, '_')
delta_modelsAndIslands <- sapply(delta_files_separated, function(x) paste(x[1:10], collapse = '_'))
delta_modelsAndIslands <- delta_modelsAndIslands[!duplicated(delta_modelsAndIslands)]
rm(delta_files_separated)

# for each model-island, get create median delta raster by season
raster_list_dry <- list()
raster_list_wet <- list()
for(i in 1:length(delta_modelsAndIslands)){
  
  # list then load monthly raster names for the model-island combo i
  rasters <- grep(delta_modelsAndIslands[[i]], delta_files, value=TRUE)
  rasters <- lapply(rasters, function(x) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/', x)))
  
  # separate by wet and dry season
  rasters_wet <- rasters[c(11,12,1,2,3,4)]
  rasters_dry <- rasters[c(5,6,7,8,9,10)]
  rm(rasters); gc()
  
  # get wet and dry season averages
  rasters_wet <- stack(rasters_wet); rasters_wet_median <- calc(rasters_wet, median)
  rasters_dry <- stack(rasters_dry); rasters_dry_median <- calc(rasters_dry, median)
  
  # # plot using ggplot
  # rasters_dry_median_df <- as.data.frame(rasters_dry_median, xy=TRUE)
  # rasters_dry_median_df <- rasters_dry_median_df[!is.na(rasters_dry_median_df$layer),]
  # ggplot(data = rasters_dry_median_df) + geom_tile(aes(x=x, y=y, color = layer, fill = layer))
  
  # save rasters to list
  raster_list_dry[[i]] <- rasters_dry_median; names(raster_list_dry)[[i]] <- delta_modelsAndIslands[[i]]
  raster_list_wet[[i]] <- rasters_wet_median; names(raster_list_wet)[[i]] <- delta_modelsAndIslands[[i]]
  
  gc()
  
}
gc()

delta_modelsAndIslands_median <- gsub('mean', 'median', delta_modelsAndIslands)

# # combine all wet and dry season rasters into statewide rasters
# names(raster_list_dry) <- names(raster_list_wet) <- c('x', 'y')
# raster_statewide_dry <- do.call(raster::merge, raster_list_dry)
# raster_statewide_wet <- do.call(raster::merge, raster_list_wet)
# 
# # export rasters
# writeRaster(raster_statewide_dry, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_delta_dry_statewide.tif', overwrite = TRUE)
# writeRaster(raster_statewide_wet, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/median_delta_wet_statewide.tif', overwrite = TRUE)

for(i in 1:length(raster_list_dry)){
  
  writeRaster(raster_list_dry[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/', delta_modelsAndIslands_median[[i]], '_dry.tif'), overwrite = TRUE)
  writeRaster(raster_list_wet[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/', delta_modelsAndIslands_median[[i]], '_wet.tif'), overwrite = TRUE)
  
}
