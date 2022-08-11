# this file formats all monthly rainfall rasters by island

library('raster')


##### 30m landcover baseline #####

# load landcover raster
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')

# define island extents
island_extents <- list(
  extent(-156.2, -154.8, 18.8, 20.28),
  extent(-156.75, -155.9, 20.4, 21.1),
  extent(-156.8, -156.45,20.45, 20.61),
  extent( -157.5, -156.5,  21.05, 21.3),
  extent(-158.4, -157.5,  21.2, 21.8),
  extent(-159.9, -159.2 , 21.8, 22.3),
  extent( -157.1, -156.75,  20.66, 20.95),
  extent(-157.5, -155.9, 20.45, 21.3)
)
# BI     # maui, kahoolawe   # kahoolawe   # molokai   # oahu     # kauai   # lanai   # maui county (maui, lanai, molokai, kahoolawe)
names(island_extents) <- c('extha', 'extma',            'extkah',     'extmol',   'extoa',   'extka',  'extlan', 'extmaco')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')




##### load and format rainfall rasters #####

# load
all_rasters_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2020_HI_monthly_rainfall_grids/statewide/rf_mm', pattern = '.tif')
all_rasters <- lapply(all_rasters_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2020_HI_monthly_rainfall_grids/statewide/rf_mm/', r)))

# format to match landcover raster
all_rasters <- lapply(all_rasters, function(r) resample(r, raster_lc))
saveRDS(all_rasters, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_historical_rainfall_resampled.rds')

# split each historical rainfall raster by island and write raster
year <- unique(substr(all_rasters_names, 1, 4))
month <- unique(substr(all_rasters_names, 6, 7))

for(i in 1:length(island_extents)){
  for(y in 1:length(year)){
    for(m in 1:length(month)){
      writeRaster(crop(all_rasters[[which(all_rasters_names == paste0(year[[y]], '_', month[[m]], '_statewide_rf_mm.tif'))]],
                       island_extents[[i]]),
                  filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_same_month/rf_same_month',
                                   island[[i]], year[[y]], month[[m]], '.tif', sep = '_'))
      gc()
    }
  }
}
