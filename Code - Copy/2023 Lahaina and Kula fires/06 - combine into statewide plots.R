
# This script combines the monthly rasters created in 05 into annual statewide rasters

library(raster)

# define islands
vec_islands <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')




##### combine raw risk #####

# take max risk from each month

# load rasters
list_annualMaxRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
             pattern = 'annual_max_fire_prob_', full.names = TRUE)
list_annualMaxRisk <-
  lapply(list_annualMaxRisk, raster)

# merge into statewide raster
raster_statewideRisk <-
  do.call(merge, list_annualMaxRisk)

# save raster
writeRaster(raster_statewideRisk,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
                              'annual_max_fire_prob_statewide.tif'))

rm(list_annualMaxRisk, raster_statewideRisk); gc()




##### combine month of max risk #####

# load rasters
list_annualMonthRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
             pattern = 'annual_monthofMax_fire_prob_', full.names = TRUE)
list_annualMonthRisk <-
  lapply(list_annualMonthRisk, raster)

# merge into statewide raster
raster_statewideMonth <-
  do.call(merge, list_annualMonthRisk)

# save raster
writeRaster(raster_statewideMonth,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
                              'annual_monthofMax_fire_prob_statewide.tif'))

rm(list_annualMonthRisk, raster_statewideMonth); gc()




##### combine binned max risk #####

# load rasters
list_binnedRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Rasters/',
             pattern = 'annualMaxRisk', full.names = TRUE)
list_binnedRisk <-
  lapply(list_binnedRisk, raster)

# merge into statewide raster
raster_binnedRisk <-
  do.call(merge, list_binnedRisk)

# save raster
writeRaster(raster_binnedRisk,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
                              'annual_binned_fire_prob_statewide.tif'))

rm(list_annualMonthRisk, raster_binnedRisk); gc()
