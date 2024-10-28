
setwd('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas')

library(raster); library(ggplot2); library(doParallel)
registerDoParallel(cores = 8)


# load a delta raster and its corresponding significance raster, use significance raster to mask insignificant deltas. Then plot and export
filenames_deltas <- list.files('delta_rasters', '.tif')
filenames_signif <- list.files('significance_rasters', '.tif')
foreach(i = 1:length(filenames_deltas), .packages = 'raster') %dopar% {
  
  # load rasters
  r_delta  <- raster(paste0('delta_rasters/', filenames_deltas[[i]]))
  r_signif <- raster(paste0('significance_rasters/', filenames_signif[[i]]))
  
  # if significance dummy = 0, then change delta to 0
  r_delta_signifOnly <- r_delta
  r_delta_signifOnly[r_signif == 0] <- 0
  
  # export raster
  writeRaster(r_delta_signifOnly, filename = paste0('delta_rasters_with_significance/', sub(pattern = '.tif', replacement = '_signifDeltasOnly.tif', x = filenames_deltas[[i]])),
              overwrite = TRUE)
  
}
