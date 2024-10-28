
# this file creates the ignition rasters, cropped by island, needed for the prediction stacks

library(raster)

# load ignition and landcover reference rasters
raster_ign <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Statewide_Ign_Dens/Statewide_Ign_Per_Sq_Mi.tif')
raster_lc  <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')

# re-project ignition raster
raster_ign <- projectRaster(raster_ign, raster_lc)

# apply ignition data transformation used in the model
raster_ign <- raster_ign^.025

# define islands, months, years
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

# resample ignition raster to landcover baseline raster
raster_ign <- resample(raster_ign, raster_lc)

# create ignition rasters
for(i in 1:length(island)){
  writeRaster(crop(raster_ign,
                   island_extents[[i]]),
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/ign_trns/ign_trns_', island[[i]], '.tif'),
              overwrite = TRUE)
}

