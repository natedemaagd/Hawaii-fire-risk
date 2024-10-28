library(raster); library(doParallel)
registerDoParallel(cores = detectCores()-2)

# list all rasters
rasters_significance_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/significance_rasters', pattern = '.tif')

# find groups: model, rcp, year, island
rasters_groups <- strsplit(rasters_significance_filenames, '_')
rasters_groups <- sapply(rasters_groups, function(c) paste(c[1:10], collapse = '_'))
rasters_groups <- rasters_groups[!duplicated(rasters_groups)]

# for each group, load rasters and sum significance pixels across all months
foreach(i = 1:length(rasters_groups), .packages = 'raster') %dopar% {
  
  # load relevant rasters
  rasters_to_load <- rasters_significance_filenames[grep(rasters_groups[[i]], rasters_significance_filenames)]
  rasters_significance <- lapply(rasters_to_load, function(r){ raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/significance_rasters/', r))})
  
  # sum across all months to get total number of times (months) each pixel is significant
  rasters_significance <- stack(rasters_significance)
  rasters_significance_annual <- calc(rasters_significance, sum)
  
  # save raster
  writeRaster(rasters_significance_annual, paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/significance_rasters_sumAnnual/', rasters_groups[[i]], '_sumAnnual.tif'), overwrite = TRUE)
}
