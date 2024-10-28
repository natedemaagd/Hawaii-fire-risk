library(raster); library(rgdal); library(ggplot2)
library(doParallel); registerDoParallel(cores = detectCores()-2)

# load woody cover rasters
rasters_woodcov_filenames <-  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split',
                              pattern = 'FracLC_wood')

# load fire shapefiles
shp_fires <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters', '2019_1999_Hawaii_Fire_Perimeters')
shp_fires_list <- split(shp_fires, shp_fires$Year)
shp_fires_list1 <- shp_fires_list[!(names(shp_fires_list) %in% c(1999, 2016:max(shp_fires$Year)))]  # limit fires so we can use fire year +/- 1 to look at before/after fire year cover (i.e. we don't have woodcov rasters for all years)




##### forest loss after fire #####

# # for each fire, load +/- 1 year and find difference in woodcover for the burned region
# rasters_woodcov_year <- as.numeric(substr(rasters_woodcov_filenames, 16, 19))
# 
# woodcov_beforeAfter_list <- list()
# for(i in 1:length(shp_fires_list1)){
# 
#     # get year of fire shapefile i
#     year <- as.numeric(names(shp_fires_list1)[[i]])
# 
#     # load required woodcov rasters
#     raster_before <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_', year-1, '.tif'))
#     raster_after  <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_', year+1, '.tif'))
# 
#     # crop woodcover of year -1 and year +1 using fire shapefile i
#     raster_before <- mask(raster_before, shp_fires_list1[[i]])
#     raster_after  <- mask(raster_after,  shp_fires_list1[[i]])
# 
#     # change in % woody cover
#     woodcov_beforeAfter <- as.data.frame(raster_before, xy = TRUE)
#     colnames(woodcov_beforeAfter) <- c('x', 'y', 'before')
#     woodcov_beforeAfter$after <- values(raster_after)
#     woodcov_beforeAfter <- woodcov_beforeAfter[complete.cases(woodcov_beforeAfter),]
#     woodcov_beforeAfter$change <- woodcov_beforeAfter$after - woodcov_beforeAfter$before
# 
#     # save dataframe to list
#     woodcov_beforeAfter_list[[i]] <- woodcov_beforeAfter
# 
#     gc()
# 
# }
# 
# # merge all data.frames together
# woodcov_beforeAfter <- do.call(rbind, woodcov_beforeAfter_list)
# 
# # change in woodcover before and after fire for different initial (before) woodcover thresholds
# woodcov_beforeAfter_summary <- as.data.frame(rbind(summary(woodcov_beforeAfter$change),
#                                                    summary(woodcov_beforeAfter$change[woodcov_beforeAfter$before > 0.10]),
#                                                    summary(woodcov_beforeAfter$change[woodcov_beforeAfter$before > 0.25]),
#                                                    summary(woodcov_beforeAfter$change[woodcov_beforeAfter$before > 0.50]),
#                                                    summary(woodcov_beforeAfter$change[woodcov_beforeAfter$before > 0.75]),
#                                                    summary(woodcov_beforeAfter$change[woodcov_beforeAfter$before > 0.90])))
# woodcov_beforeAfter_summary$initial_minimum_woodcov <- c(0, 0.1, 0.25, 0.50, 0.75, 0.90)
# 
# # save data
# saveRDS(woodcov_beforeAfter, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr.rds')
# saveRDS(woodcov_beforeAfter_summary, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr_summary.rds')

woodcov_beforeAfter <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr.rds')
woodcov_beforeAfter_summary <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr_summary.rds')

# plot
woodcov_beforeAfter$plot_cutoff <- 0
woodcov_beforeAfter$plot_cutoff <- ifelse(woodcov_beforeAfter$before > 0.10, 0.10, woodcov_beforeAfter$plot_cutoff)
woodcov_beforeAfter$plot_cutoff <- ifelse(woodcov_beforeAfter$before > 0.25, 0.25, woodcov_beforeAfter$plot_cutoff)
woodcov_beforeAfter$plot_cutoff <- ifelse(woodcov_beforeAfter$before > 0.50, 0.50, woodcov_beforeAfter$plot_cutoff)
woodcov_beforeAfter$plot_cutoff <- ifelse(woodcov_beforeAfter$before > 0.75, 0.75, woodcov_beforeAfter$plot_cutoff)
woodcov_beforeAfter$plot_cutoff <- ifelse(woodcov_beforeAfter$before > 0.90, 0.90, woodcov_beforeAfter$plot_cutoff)
woodcov_beforeAfter$plot_cutoff <- as.character(woodcov_beforeAfter$plot_cutoff*100)
woodcov_beforeAfter$plot_cutoff <- factor(woodcov_beforeAfter$plot_cutoff, levels = c('0', '10', '25', '50', '75', '90'))

ggplot(data = woodcov_beforeAfter[woodcov_beforeAfter$plot_cutoff != '0',]) + geom_density(aes(change*100, color = plot_cutoff, fill = plot_cutoff), alpha = 0.2) +
  labs(fill = 'Min. wood cover (%)', color = 'Min. wood cover (%)', x = 'Change in wood cov (%)', y = 'Density')




##### separate forest loss by rainfall #####

# # load mean annual rainfall
# raster_rainfall <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
# 
# # convert rainfall raster to dataframe
# df_rainfall <- as.data.frame(raster_rainfall, xy = TRUE)
# df_rainfall <- df_rainfall[complete.cases(df_rainfall),]
# colnames(df_rainfall) <- c('x', 'y', 'ann_rain_mm')
# 
# # for each unique burned forest pixel, find closest rainfall pixel
# pixels_burned <- woodcov_beforeAfter[!duplicated(woodcov_beforeAfter[c('x', 'y')]),]
# pixels_burned_rainfall <- foreach(i = 1:nrow(pixels_burned), .combine = 'c') %dopar% {
#   
#   # for burned pixel i, find distance to each rainfall pixel
#   dist_vec <- sqrt((pixels_burned$x[[i]]-df_rainfall$x)^2 + (pixels_burned$y[[i]]-df_rainfall$y)^2)
#   
#   # return rainfall value of minimum distance
#   return(df_rainfall[which.min(dist_vec), 'ann_rain_mm'])
#   gc()
# }
# 
# # add rainfall to unique burned pixels then merge to full data
# pixels_burned$ann_rain_mm <- pixels_burned_rainfall
# pixels_burned <- pixels_burned[c('x', 'y', 'ann_rain_mm')]
# woodcov_beforeAfter <- merge(woodcov_beforeAfter, pixels_burned, c('x', 'y'))
# saveRDS(woodcov_beforeAfter, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr_withAnnRainfall.rds')
woodcov_beforeAfter <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/06a_woodCover_beforeAfter_1yr_withAnnRainfall.rds')

# separate out min 75% woodcov and add rainfall quintiles
woodcov_beforeAfter_min75woodcov <- woodcov_beforeAfter[woodcov_beforeAfter$plot_cutoff == '75',]
woodcov_beforeAfter_min75woodcov <- within(woodcov_beforeAfter_min75woodcov, rainfall_quintile <- cut(ann_rain_mm, quantile(ann_rain_mm, probs=0:5/5), include.lowest=TRUE))

# for min 75% woodcov, plot change in woodcov by rainfall quantile
ggplot(data = woodcov_beforeAfter_min75woodcov) + geom_density(aes(change*100, color = rainfall_quintile, fill = rainfall_quintile), alpha = 0.2) +
  labs(fill = 'Mean annual rainfall (mm)', color = 'Mean annual rainfall (mm)', x = 'Change in wood cov (%)', y = 'Density')




##### change in forest area (hectares) #####

# get resolution of landcover rasters
raster_resolution <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/', rasters_woodcov_filenames[[1]]))
raster_resolution <- 30  # grid cell is 30x30 meters
woodcov_beforeAfter$change_sqm <- raster_resolution^2 * woodcov_beforeAfter$change

total_change_hectares <- sum(woodcov_beforeAfter$change_sqm) / 10000
