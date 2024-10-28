
library(raster); library(doParallel)
registerDoParallel(cores = 5)

# define directories for current mean fire probs and future fire probs
dir_current <- 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/'
dir_future  <- 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/'

# list all future raster filenames - to be used to load both future and corresponding current raster
filenames_future <- list.files(dir_future, pattern = '.tif')
filenames_future <- filenames_future[-grep('_SE', filenames_future, fixed=TRUE)]  # remove SE files - we can load them using the base filename
filenames_future <- filenames_future[!(filenames_future %in% c('delta_v2.tif', 'deltav2.tif'))]  # remove deltav2 files (what are these???)

# for each month, island, model, period: calculate difference raster
foreach(i = 1:length(filenames_future), .packages = 'raster') %dopar% {
  
  # load raster i and its SEs
  r_future    <- raster(paste0(dir_future, filenames_future[[i]]))
  r_future_se <- raster(paste0(dir_future, strsplit(filenames_future[[i]], split = '[.]')[[1]][[1]], '_SE.tif'))
  
  # load current-climate raster corresponding to future raster i, and its SEs
  r_current    <- raster(paste0(dir_current, paste0(strsplit(filenames_future[[i]], split = '_')[[1]][c(2:5,9:10)], collapse = '_')))
  r_current_se <- raster(sub(paste0(dir_current, paste0(strsplit(filenames_future[[i]], split = '_')[[1]][c(2:5,9:10)], collapse = '_')), pattern = '.tif', replacement = '_SE.tif'))
  
  # calculate difference raster: future - current
  r_delta <- r_future - r_current
  
  # create value and SE table to see if 95% CI overlaps between current and future
  df <- data.frame(val_current_upper = values(r_current) + 1.96*values(r_current_se),
                   val_current_lower = values(r_current) - 1.96*values(r_current_se),
                   val_future_upper  = values(r_future)  + 1.96*values(r_future_se),
                   val_future_lower  = values(r_future)  - 1.96*values(r_future_se))
  
  # check if either the lower or upper bound of the future value is within the 95% CI of the current value
  df$future_lower_in_current <- ifelse(df$val_future_lower >= df$val_current_lower & df$val_future_lower <= df$val_current_upper, 1, 0)
  df$future_upper_in_current <- ifelse(df$val_future_upper >= df$val_current_lower & df$val_future_lower <= df$val_current_upper, 1, 0)
  df$future_in_current <- df$future_lower_in_current + df$future_upper_in_current  # add the two - both ends of future CI must be outside current CI
  df$future_in_current <- ifelse(df$future_in_current > 1, 1, df$future_in_current)  # keep values as max 1. NOTE: if 1, delta is INSIGNIFICANT
  df$future_significant <- ifelse(df$future_in_current == 1, 0, 1)  # flip values so 1 = significant change
  df$future_significant[is.na(df$val_current_upper)] <- NA  # keep NAs (water pixels)
  
  # add significance dummy to raster
  r_significance <- r_future
  values(r_significance) <- df$future_significant
  
  # write rasters
  writeRaster(r_delta, paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters/delta_',
                              filenames_future[[i]]), overwrite = TRUE)
  
  writeRaster(r_significance, paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/significance_rasters/significance95pctDummy_',
                                     filenames_future[[i]]), overwrite = TRUE)
  
  gc()
}
