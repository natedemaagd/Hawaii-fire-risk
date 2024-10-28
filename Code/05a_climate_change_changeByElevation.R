
# This script compares current fire risk to future fire risk by elevation

library(terra)
library(ggplot2)
library(tidyverse)

# vectors used for loops
islands <- c('Kauai', 'Oahu', 'MauiCounty', 'Hawaii')




##### load data #####

# elevation data
rast_elevation <-
  rast(paste0("C:/Users/nated/OneDrive/Desktop/",
              "HI_DEM_30m.tif"))

# fire risk rasters - current mean annual
rast_riskMeanCurrentWet <- rast("D:/Unsynced projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_wet_statewide_waterMasked.tif")
rast_riskMeanCurrentDry <- rast("D:/Unsynced projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_Dry_statewide_waterMasked.tif")
rast_riskMeanCurrent <- (rast_riskMeanCurrentDry + rast_riskMeanCurrentWet) / 2
# writeRaster(rast_riskMeanCurrent,
#             filename = "D:/Unsynced projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/mean_current_fire_prob_annual_statewide_waterMasked.tif",
#             overwrite = TRUE)
rm(rast_riskMeanCurrentDry, rast_riskMeanCurrentWet)
gc()

# fire risk rasters - future mean annual
list_rast_future_filenames <- list.files('D:/Unsynced projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                                         full.names = TRUE, pattern = 'waterMasked')
list_rast_future_filenames <-
  list_rast_future_filenames[grep('rcp85', list_rast_future_filenames)]  # keep only RCP 8.5




##### create required future climate rasters #####

# NOTE: no mid-century dynamical data available

# mid-century statistical
list_rast_2070_sta <-
  list_rast_future_filenames[grep('2070', list_rast_future_filenames)]  # list files
list_rast_2070_sta_byIsland <- list()
for(i in 1:length(islands)){  # for each island, get mean of wet and dry
  
  # keep only island i and load them (dry and wet raster)
  list_island_i <- list_rast_2070_sta[grep(islands[[i]], list_rast_2070_sta)]
  list_island_i <- lapply(list_island_i, rast)
  
  # get mean of dry and wet
  rast_island_i <- (list_island_i[[1]] + list_island_i[[2]]) / 2
  
  # add to list
  list_rast_2070_sta_byIsland[[i]] <- rast_island_i
  rm(list_island_i, rast_island_i)
  gc()
}
rm(i)
rast_2070_sta <- do.call(terra::merge, list_rast_2070_sta_byIsland)  # merge islands into one raster
rast_2070_sta <- rast_riskMeanCurrent + rast_2070_sta  # convert delta raster to actual future values
rm(list_rast_2070_sta, list_rast_2070_sta_byIsland)
gc()

# late-century dynamical
list_rast_2100_dyn <-
  list_rast_future_filenames[grep('2100', list_rast_future_filenames)]  # keep only 2100
list_rast_2100_dyn <- list_rast_2100_dyn[grep('dyn', list_rast_2100_dyn)]  # keep only dynamical
list_rast_2100_dyn_byIsland <- list()
for(i in 1:length(islands)){  # for each island, get mean of wet and dry
  
  # keep only island i and load them (dry and wet raster)
  list_island_i <- list_rast_2100_dyn[grep(islands[[i]], list_rast_2100_dyn)]
  list_island_i <- lapply(list_island_i, rast)
  
  # get mean of dry and wet
  rast_island_i <- (list_island_i[[1]] + list_island_i[[2]]) / 2
  
  # add to list
  list_rast_2100_dyn_byIsland[[i]] <- rast_island_i
  rm(list_island_i, rast_island_i)
  gc()
}
rm(i)
rast_2100_dyn <- do.call(terra::merge, list_rast_2100_dyn_byIsland)  # merge islands into one raster
rast_2100_dyn <- rast_riskMeanCurrent + rast_2100_dyn  # convert delta raster to actual future values
rm(list_rast_2100_dyn, list_rast_2100_dyn_byIsland)
gc()

# late-century statistical
list_rast_2100_sta <-
  list_rast_future_filenames[grep('2100', list_rast_future_filenames)]  # keep only 2100
list_rast_2100_sta <- list_rast_2100_sta[grep('sta', list_rast_2100_sta)]  # keep only statistical
list_rast_2100_sta_byIsland <- list()
for(i in 1:length(islands)){  # for each island, get mean of wet and dry
  
  # keep only island i and load them (dry and wet raster)
  list_island_i <- list_rast_2100_sta[grep(islands[[i]], list_rast_2100_sta)]
  list_island_i <- lapply(list_island_i, rast)
  
  # get mean of dry and wet
  rast_island_i <- (list_island_i[[1]] + list_island_i[[2]]) / 2
  
  # add to list
  list_rast_2100_sta_byIsland[[i]] <- rast_island_i
  rm(list_island_i, rast_island_i)
  gc()
}
rm(i)
rast_2100_sta <- do.call(terra::merge, list_rast_2100_sta_byIsland)  # merge islands into one raster
rast_2100_sta <- rast_riskMeanCurrent + rast_2100_sta  # convert delta raster to actual future values
rm(list_rast_2100_sta, list_rast_2100_sta_byIsland)
gc()




##### convert to plottable data #####

# stack all rasters
rast_stack <-
  c(rast_elevation, rast_riskMeanCurrent, rast_2070_sta, rast_2100_sta, rast_2100_dyn)

# convert to data.frame
dat_allRast <- as.data.frame(rast_stack)
colnames(dat_allRast) <- c('elevation_m', 'riskMeanCurrent', 'sta2070', 'sta2100', 'dyn2100')

# remove ocean pixels
dat_allRast <- dat_allRast[dat_allRast$elevation_m > 0,]
gc()

# all values should be greater than or equal to 0
dat_allRast[dat_allRast < 0] <- 0

# melt data
dat_melt <-
  data.frame(fireRisk = with(dat_allRast,
                             c(riskMeanCurrent,
                               sta2070,
                               sta2100,
                               dyn2100)),
             elevation_m = dat_allRast$elevation_m)
dat_melt$model <-
  rep(c('Current', 'Mid-century statistical', 'Late-century statistical', 'Late-century dynamical'),
      each = nrow(dat_allRast))
dat_melt$model <-
  factor(dat_melt$model,
         levels = c('Current', 'Mid-century statistical', 'Late-century statistical', 'Late-century dynamical'))
rm(dat_allRast, rast_riskMeanCurrent, rast_2070_sta, rast_2100_dyn, rast_2100_dyn, rast_2100_sta, rast_elevation, rast_stack, islands, list_rast_future_filenames)
gc()




##### plot #####

p <- ggplot(data = dat_melt) +
  geom_hex(aes(x = elevation_m, y = fireRisk)) +
  facet_wrap(vars(model), ncol = 4) +
  scale_fill_gradient(low = 'gray60', high = 'dodgerblue4') +
  labs(x = 'Elevation (m)', y = 'Fire risk', fill = 'Pixel density') +
  theme(text = element_text(size = 18),
        legend.position = 'bottom', legend.key.width = unit(1, 'in'))
ggsave(plot = p,
       filename = paste0('C:/Users/nated/OneDrive/Desktop/',
                         '05a climate change by elevation.png'),
       height = 6, width = 10, dpi = 300, units = 'in')
             
             
             