
# This script creates rasters/maps of categorized risk using the risk cutoffs

library(raster); library(terra)




##### load data #####

# load cutoff data
dat_cutoffs <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds.rds'))

# list county-level mean risk rasters
list_ras <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
             pattern = 'max_current', full.names = TRUE)
list_ras <- list_ras[grep('waterMasked', list_ras)]  # keep only water-masked rasters
list_islands <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')
list_ras <- unique(grep(paste(list_islands, collapse = "|"),
                        list_ras, value = TRUE)
                   )




##### create categorical rasters #####

# for each county, use county-specific cutoff values to bin risk values
list_rasBinned <- list()
for(r in 1:length(list_ras)){
  
  # get island name
  val_island <- strsplit(list_ras[[r]], split = '_')
  val_island <- val_island[[1]][[7]]
  
  # load raster
  ras <- raster(list_ras[[r]])
  
  # convert to data.frame
  dat_ras <- as.data.frame(ras)
  colnames(dat_ras) <- 'val_risk'
  
  # sort each risk value according to bins
  val_riskBreaks <- dat_cutoffs[dat_cutoffs$island == val_island,]
  val_riskBreaks <- c(0, t(val_riskBreaks[1, 2:4]), 1)
  dat_ras$riskBin <-
    cut(dat_ras$val_risk,
        breaks = val_riskBreaks,
        include.lowest = TRUE,
        labels = 1:4)
  dat_ras$riskBin <- as.numeric(as.character(dat_ras$riskBin))
  gc()
  
  # generate raster using binned risks
  ras_binned <- ras
  values(ras_binned) <- dat_ras$riskBin
  list_rasBinned[[r]] <- ras_binned
  
  rm(val_island, ras, dat_ras, val_riskBreaks, ras_binned)
  gc()
}

rm(r)




##### save county-level rasters #####

for(r in 1:length(list_rasBinned)){
  
  # name raster
  names(list_rasBinned)[[r]] <-
    paste0('binned_', strsplit(list_ras[[r]], '/')[[1]][[9]])
  
  # write raster
  writeRaster(list_rasBinned[[r]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned/binned_',
                                strsplit(list_ras[[r]], '/')[[1]][[9]]),
              overwrite = TRUE)
}

rm(r)




##### create statewide rasters #####

# create vector of seasons
list_season <- c('dry', 'wet')

# merge rasters by season
list_rasStatewide <- list()
for(s in 1:length(list_season)){
  
  # get rasters for season s
  list_rasSeason <- grep(list_season[[s]],
                         names(list_rasBinned))
  list_rasSeason <- list_rasBinned[list_rasSeason]
  
  # convert to SpatRaster (terra package)
  list_rasSeason <- lapply(list_rasSeason, rast)
  
  # merge all rasters
  sprc_rasSeason <- sprc(list_rasSeason)
  ras_season <- terra::merge(sprc_rasSeason)
  
  # save raster
  list_rasStatewide[[s]] <- ras_season
  writeRaster(ras_season,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned/',
                                substr(names(list_rasSeason)[[1]], 1, 29),
                                list_season[[s]], '_statewide_waterMasked.tif'),
              overwrite = TRUE)
  gc()
}

rm(list_season, list_rasSeason, sprc_rasSeason, s,
   dat_cutoffs, list_rasBinned, ras_season, list_islands, list_ras)
gc()


##### create annual statewide raster #####

# load rasters
list_ras <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned',
                       pattern = 'statewide', full.names = TRUE)
list_ras <- lapply(list_ras, raster)

# find pixel-level max of each raster
list_ras <- stack(list_ras)
ras_maxAnnualRiskBinned <- max(list_ras)

# write raster
writeRaster(ras_maxAnnualRiskBinned,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned/binned_',
                              substr(names(list_ras)[[1]], 1, 22),
                              'annual_statewide_waaterMasked.tif'),
            overwrite = TRUE)
