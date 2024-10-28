
# this file creates rasters for all islands. months, and years - herb/wood landcover only

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




##### landcover rasters #####

# landcover rasters - herb
landcover_herb <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', pattern = '_herb_')
landcover_herb <- lapply(landcover_herb, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))
landcover_herb <- lapply(landcover_herb, function(r) resample(r, raster_lc))
gc()

island_landcover_herb <- list()
for(i in 1:length(island_extents)){
  island_landcover_herb[[i]] <- list()
  for(y in 1:length(landcover_herb)){
    island_landcover_herb[[i]][[y]] <- crop(landcover_herb[[y]], island_extents[[i]])
  }
}
rm(i)
gc()

# landcover rasters - wood
landcover_wood <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', pattern = '_wood_')
landcover_wood <- lapply(landcover_wood, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))
landcover_wood <- lapply(landcover_wood, function(r) resample(r, raster_lc))
gc()

island_landcover_wood <- list()
for(i in 1:length(island_extents)){
  island_landcover_wood[[i]] <- list()
  for(y in 1:length(landcover_wood)){
    island_landcover_wood[[i]][[y]] <- crop(landcover_wood[[y]], island_extents[[i]])
  }
}
rm(i)
gc()

# landcover rasters - bare
landcover_bare <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', pattern = '_bare_')
landcover_bare <- lapply(landcover_bare, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))
landcover_bare <- lapply(landcover_bare, function(r) resample(r, raster_lc))
gc()

island_landcover_bare <- list()
for(i in 1:length(island_extents)){
  island_landcover_bare[[i]] <- list()
  for(y in 1:length(landcover_bare)){
    island_landcover_bare[[i]][[y]] <- crop(landcover_bare[[y]], island_extents[[i]])
  }
}
rm(i)
gc()




##### save raster lists #####

# create vectors for naming raster stacks
year <- c(1999,2016)
#year <- year[-which(year == 2014)]  # no raster for 2014
month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# save herb cover rasters
for(i in 1:length(island)){
  for(y in 1:length(year)){
    writeRaster(island_landcover_herb[[i]][[y]],
                filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/herbcov_yearof/herbcov_yearof', island[[i]], year[[y]], '.tif', sep= '_'),
                overwrite = TRUE)
  }
}

# save wood cover rasters
for(i in 1:length(island)){
  for(y in 1:length(year)){
    writeRaster(island_landcover_wood[[i]][[y]],
                filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/woodcov_yearof/woodcov_yearof', island[[i]], year[[y]], '.tif', sep= '_'),
                overwrite = TRUE)
  }
}

# save bare cover rasters
for(i in 1:length(island)){
  for(y in 1:length(year)){
    writeRaster(island_landcover_bare[[i]][[y]],
                filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/barecov_yearof/barecov_yearof', island[[i]], year[[y]], '.tif', sep= '_'),
                overwrite = TRUE)
  }
}
