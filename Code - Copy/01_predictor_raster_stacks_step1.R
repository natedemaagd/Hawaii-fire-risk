
# this file creates rasters for all islands. months, and years - all variables except herb/wood landcover (which is in step 2)

library(raster); library(ggplot2); library(viridis)




##### 30m landcover baseline #####

# load landcover raster
raster_lc <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/CAH_LandCover_HI_GAP_revised_2017/CAH_LandCover.tif")

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



##### rainfall rasters ####

# list files
rainfall_files <- list.files('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State', pattern = '.txt')

# read files
rainfall_rasters <- list()
for(i in 1:length(rainfall_files)){
  rainfall_rasters[[i]] <- raster(paste0('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/', rainfall_files[[i]]))
}

months_chr <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', 'ann')

names(rainfall_rasters) <- months_chr

rm(rainfall_files, i)

# convert in to mm
rainfall_rasters <- lapply(rainfall_rasters, function(r) r*25.4)


### create rainfall 3-mo stacks

# create 3-month stacks
rainfall_cum3mo <- list(Reduce("+", rainfall_rasters[10:12]),
                        Reduce("+", rainfall_rasters[c(11:12,1)]),
                        Reduce("+", rainfall_rasters[c(12,1:2)]),
                        Reduce("+", rainfall_rasters[1:3]),
                        Reduce("+", rainfall_rasters[2:4]),
                        Reduce("+", rainfall_rasters[3:5]),
                        Reduce("+", rainfall_rasters[4:6]),
                        Reduce("+", rainfall_rasters[5:7]),
                        Reduce("+", rainfall_rasters[6:8]),
                        Reduce("+", rainfall_rasters[7:9]),
                        Reduce("+", rainfall_rasters[8:10]),
                        Reduce("+", rainfall_rasters[9:11]))

# # plot 3-month rainfall stacks
# rainfall_val_range <- c(sapply(rainfall_cum3mo, values))      # get range of values to standardize legend
# rainfall_val_range <- range(rainfall_val_range, na.rm = TRUE)
# 
# for(i in 1:length(rainfall_cum3mo)){
#   
#   # create dataframe from raster
#   rdf <- as.data.frame(rainfall_cum3mo[[i]],xy=TRUE)
#   rdf <- rdf[!is.na(rdf$layer),]
#   
#   # plot dataframe
#   ggplot(data = rdf)+
#     geom_tile(aes(x=x, y=y, fill=layer)) +
#     coord_equal() +
#     scale_fill_viridis(limits=rainfall_val_range, name = 'Rainfall (mm)',
#                        breaks=c(rainfall_val_range[[1]], (rainfall_val_range[[1]]+rainfall_val_range[[2]])/2, rainfall_val_range[[2]]),
#                        labels=c(round(rainfall_val_range[[1]]), round((rainfall_val_range[[1]]+rainfall_val_range[[2]])/2), round(rainfall_val_range[[2]]))) +
#     theme(panel.background = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
#   
#   # save plot
#   ggsave(paste0('D:/OneDrive - hawaii.edu/Documents/Projects/PICASC LandSea/Figures and tables/Figures/Maps/cumulative_3mo_rainfall_stacks_250m_mm/month', months_chr[[i]], '.png'))
# }

# # plot cumulative rainfall over year to check data
# total_cum3mo_rainfall <- sapply(rainfall_cum3mo, function(r) sum(values(r), na.rm = TRUE))
# ggplot() + geom_line(aes(x = 1:12, y = total_cum3mo_rainfall)) +
#   scale_x_continuous(breaks = 1:12, labels = 1:12)
# ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/PICASC LandSea/Figures and tables/Figures/Maps/cumulative_3mo_rainfall_stacks_250m_mm/total_3mo_statewide_linegraph.png',
#        height = 4, width = 7)

# save raster stacks
for(i in 1:length(rainfall_cum3mo)){
  writeRaster(rainfall_cum3mo[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_3mo', months_chr[[i]], '.tif'),
              overwrite = TRUE)
}

rm(months_chr, i)




##### disaggregate to 30m and crop #####

# re-project landcover raster
# raster_lc <- projectRaster(raster_lc, crs = crs(rainfall_rasters[[1]]))
# writeRaster(raster_lc, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')

# downscale rainfall rasters to 30m
rainfall_rasters_30m <- lapply(rainfall_rasters, function(r) resample(r, raster_lc))
rainfall_cum3mo_30m  <- lapply(rainfall_cum3mo,  function(r) resample(r, raster_lc))
saveRDS(rainfall_rasters_30m, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_downscale_30m.tif')
saveRDS(rainfall_cum3mo_30m,  file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_cum3month_downscale_30m.tif')
rainfall_rasters_30m <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_downscale_30m.tif')
rainfall_cum3mo_30m  <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_cum3month_downscale_30m.tif')

# crop same-month rainfall rasters
island_rainfall_same_month <- list()
for(i in 1:length(island_extents)){
  island_rainfall_same_month[[i]] <- list()
  for(m in 1:12){
    island_rainfall_same_month[[i]][[m]] <- crop(rainfall_rasters_30m[[m]], island_extents[[i]])
  }
}
rm(i,m)

# crop 3-mo cumulative rainfall
island_rainfall_3mo <- list()
for(i in 1:length(island_extents)){
  island_rainfall_3mo[[i]] <- list()
  for(m in 1:12){
    island_rainfall_3mo[[i]][[m]] <- crop(rainfall_cum3mo_30m[[m]], island_extents[[i]])
  }
}
rm(i,m)

# annual rainfall
rainfall_annual <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
rainfall_annual <- resample(rainfall_annual, raster_lc)
island_rainfall_annual <- list()
for(i in 1:length(island_extents)){
  island_rainfall_annual[[i]] <- crop(rainfall_annual, island_extents[[i]])
}
rm(i)

saveRDS(island_rainfall_annual, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_annual_downscale_30m.tif')
island_rainfall_annual <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_rasters_annual_downscale_30m.tif')

# soil moisture
soil_mst <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_EVAP_min_monthly_soil_mst.tif")
soil_mst <- resample(soil_mst, raster_lc)
island_soil_moisture <- list()
for(i in 1:length(island_extents)){
  island_soil_moisture[[i]] <- crop(soil_mst, island_extents[[i]])
}
rm(i)

# mean annual temp
ann_temp <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_EVAP_mean_annual_temp_statewide_250m.tif")
ann_temp <- resample(ann_temp, raster_lc)
island_temp_ann <- list()
for(i in 1:length(island_extents)){
  island_temp_ann[[i]] <- crop(ann_temp, island_extents[[i]])
}
rm(i)


##### create rest of covar rasters #####

# 0-dummy rasters
island_0dummy_rasters <- island_temp_ann
for(i in 1:length(island_extents)){
  values(island_0dummy_rasters[[i]]) <- 0
}
island_0dummy_rasters <- lapply(island_0dummy_rasters, function(r) {names(r) <- 'dummy_raster'; r})

# previous month rainfall
island_rainfall_previous_month <- island_rainfall_same_month
for(i in 1:length(island_rainfall_previous_month)){
  island_rainfall_previous_month[[i]] <- c(island_rainfall_previous_month[[i]][[12]], island_rainfall_previous_month[[i]])
  island_rainfall_previous_month[[i]][[13]] <- NULL
}

save.image("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/01_predictor_raster_stacks_step1.RData")

load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/01_predictor_raster_stacks_step1.RData")



##### write rasters #####

# create vectors for naming raster stacks
year <- 1999:2016
year <- year[-which(year == 2014)]  # no raster for 2014
month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
island <- c('Hawaii', 'MauiKahoolawe', 'Kahoolawe', 'Molokai', 'Oahu', 'Kauai', 'Lanai', 'MauiCounty')

# dummy rasters
for(i in 1:length(island_0dummy_rasters)){
  writeRaster(island_0dummy_rasters[[i]][[1]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/island_dummy/dummy', island[[i]], '.tif', sep = '_'),
              overwrite = TRUE)
}

# rainfall 3 months
for(i in 1:length(island_rainfall_3mo)){
  for(m in 1:12){
    writeRaster(island_rainfall_3mo[[i]][[m]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_cumrf_3mo/monthly_cumrf_3mo', island[[i]], month[[m]], '.tif', sep = '_'),
                overwrite = TRUE)
    gc()
  }
}

# rainfall annual
for(i in 1:length(island_rainfall_annual)){
  writeRaster(island_rainfall_annual[[i]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/monthly_cumrf_12mo/monthly_cumrf_12mo', island[[i]], '.tif', sep = '_'),
              overwrite = TRUE)
}

# rainfall previous month
for(i in 1:length(island_rainfall_previous_month)){
  for(m in 1:12){
    writeRaster(island_rainfall_previous_month[[i]][[m]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_1mo_prior/rf_1mo_prior', island[[i]], month[[m]], '.tif', sep = '_'),
                overwrite = TRUE)
    gc()
  }
}


# rainfall same month
for(i in 1:length(island_rainfall_same_month)){
  for(m in 1:12){
    writeRaster(island_rainfall_same_month[[i]][[m]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rainfall_same_month/rainfall_same_month', island[[i]], month[[m]], '.tif', sep = '_'),
                overwrite = TRUE)
    gc()
  }
}

# soil moisture
for(i in 1:length(island_soil_moisture)){
  writeRaster(island_soil_moisture[[i]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst', island[[i]], '.tif', sep = '_'),
              overwrite = TRUE)
}

# annual temperature
for(i in 1:length(island_temp_ann)){
  writeRaster(island_temp_ann[[i]], filename = paste('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp', island[[i]], '.tif', sep = '_'),
              overwrite = TRUE)
}
