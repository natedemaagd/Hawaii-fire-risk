
# This script uses the rasters created step 1 to plot maps of the max change in fire risk probability.
# The delta rasters zero out pixels with insignificant deltas based on SEs.

library(raster); library(ggplot2); library(viridis);library(gridExtra); library(cowplot)
library(scales); library(sf)

# list rasters
vec_filenamesCurrent <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/', pattern = '.tif')
vec_filenamesFuture <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/', pattern = '.tif')
vec_filenamesDelta <- c(list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/', pattern = '.tif'),
                        list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/total annual/', pattern = '.tif'))

# load risk thresholds
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
rm(dat_fire, dat_fire2); gc()

# load mountain peak shapefiles
sf_mountaintops <- read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii mountaintops/masked_area.shp")

# definitions
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')
vec_season <- c('Dry', 'Wet', 'overall annual')
vec_seasonWet <- c('10','11','12','01','02','03','04')
vec_seasonDry <- c('05', '06', '07', '08', '09')
vec_year <- c('2070', '2100')
vec_model <- c('sta', 'dyn')




##### plot 4-panel annual average climate change figures #####

# create viridis color scale for delta plots
vec_viridis <- viridis(8)
vec_viridis <- c(vec_viridis[[1]], 'lightgray',
                 vec_viridis[[4]], vec_viridis[[8]])

# load fire risk thresholds
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
rm(dat_fire,  dat_fire2)

# create plots - loop by island
for(i in 1:length(vec_islands)){
  
  
  ### panel 1: current climate ###
  
  # load current climate raster
  r_current <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
                             'mean current fire prob ',
                             vec_islands[[i]],
                             ' overall annual.tif'))
  
  # on first loop, re-project mountaintop shapefile
  if(i == 1){
    sf_mountaintops <- st_transform(sf_mountaintops, crs(r_current))
  }
  
  # mask mountaintops
  r_current <- mask(r_current, sf_mountaintops,
                    updatevalue = 0, inverse = TRUE)
  
  # create legend breaks and labels
  vec_legendBreaksCurrent <- seq(min(values(r_current), na.rm = TRUE),
                                 max(values(r_current), na.rm = TRUE),
                                 length.out = 4)
  vec_legendLabelsCurrent <- sprintf("%.2f",
                                     round(vec_legendBreaksCurrent, 4) * 100)
  
  # convert raster to data.frame
  plotdatCurrent <- as.data.frame(r_current, xy = TRUE)
  colnames(plotdatCurrent) <- c('x', 'y', 'prob')
  plotdatCurrent <- plotdatCurrent[!is.na(plotdatCurrent$prob),]
  gc()
  
  # # create plot
  # p_current <- ggplot(data = plotdatCurrent,
  #                     aes(x = x, y = y, fill = prob)) +
  #   geom_raster() +
  #   coord_equal() +
  #   scale_fill_viridis(limits = range(vec_legendBreaksCurrent),
  #                      breaks = vec_legendBreaksCurrent,
  #                      labels = vec_legendLabelsCurrent,
  #                      option = 'turbo') +
  #   theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
  #         panel.background = element_blank(), text = element_text(size = 16),
  #         legend.key.height= unit(0.75, 'cm')) +
  #   labs(fill = 'Annual fire\nprobability (%)',
  #        title = 'Current')
  
  # create alternate plot: categories instead of continuous
  plotdatCurrent$probCategory <-
    with(plotdatCurrent,
         ifelse(prob >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_75pctileBurned'],
                'Very high',
         ifelse(prob >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_50pctileBurned'] &
                prob <  highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_75pctileBurned'],
                'High',
         ifelse(prob >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_25pctileBurned'] &
                prob <  highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_50pctileBurned'],
                'Moderate',
         ifelse(prob <  highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
                                           'hiRiskThresh_25pctileBurned'],
                'Low',
                NA
                )
         )
         )
         )
         )
  plotdatCurrent$probCategory <-
    factor(plotdatCurrent$probCategory,
           levels = c('Very high', 'High', 'Moderate', 'Low'))
  
  p_current <- ggplot(data = plotdatCurrent,
                      aes(x = x, y = y, fill = probCategory)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = rev(turbo(4)), drop = FALSE) +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          panel.background = element_blank(), text = element_text(size = 16),
          legend.key.height= unit(0.75, 'cm')) +
    labs(fill = 'Annual fire\nprobability',
         title = 'Current')
  
  
  
  
  ### panels 2-4: deltas ###
  
  
  # load required delta rasters
  list_rDeltas <- list(
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                  'max delta ', vec_islands[[i]], ' 2070 overall annual sta.tif')),
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                  'max delta ', vec_islands[[i]], ' 2100 overall annual sta.tif')),
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
                  'max delta ', vec_islands[[i]], ' 2100 overall annual dyn.tif'))
  )
  names(list_rDeltas) <- c('Mid-century statistical',
                           'End-of-century statistical',
                           'End-of-century dynamical')
  
  # mask delta rasters with mountaintop shapefile
  list_rDeltas <- lapply(
    list_rDeltas, function(r){
      mask(r, sf_mountaintops,
           updatevalue = 0, inverse = TRUE)
    }
  )
  
  # create data.frame of delta rasters
  plotdatDelta <- lapply(list_rDeltas, function(r){
    as.data.frame(r, xy = TRUE)
  })
  plotdatDelta <- lapply(plotdatDelta, function(df){
    colnames(df) <- c('x', 'y', 'delta')
    return(df)
  })
  for(m in 1:length(plotdatDelta)){
    plotdatDelta[[m]]$model <-
      names(list_rDeltas)[[m]]
  }
  plotdatDelta <- lapply(plotdatDelta, function(df){
    df[!is.na(df$delta),]
  })
  gc()
  plotdatDelta <- do.call(rbind, plotdatDelta)
  plotdatDelta$delta[plotdatDelta$delta > quantile(plotdatDelta$delta, probs = 0.9999)[[1]]] <-
    quantile(plotdatDelta$delta, probs = 0.9999)[[1]]  # REPLACE OUTLIERS WITH 99.9 PERCENTILE
  
  # create data.frame with only statistically-insignificant deltas
  plotdatDeltaInsig <- plotdatDelta[plotdatDelta$delta == 0,]
  
  # create standard legend for all delta plots in county i
  vec_legendBreaksDelta <- seq(min(plotdatDelta$delta),
                               max(plotdatDelta$delta),
                               length.out = 4)
  vec_legendBreaksDelta <- vec_legendBreaksDelta[order(vec_legendBreaksDelta)]
  vec_legendLabelsDelta <- sprintf("%.2f",
                                     round(vec_legendBreaksDelta, 4) * 100)
  vec_legendLabelsDelta[[length(vec_legendLabelsDelta)]] <-
    paste('>', vec_legendLabelsDelta[[length(vec_legendLabelsDelta)]])
  
  # adjust delta color scale as needed (kauai has no drop in fire prob so remove the first color)
  if(i == 2){
    vec_viridisDelta <- vec_viridis[-1]
    vec_colorVals <- rescale(c(min(vec_legendBreaksDelta),
                               mean(c(0, max(vec_legendBreaksDelta))),
                               max(vec_legendBreaksDelta)))
  } else {
    vec_viridisDelta <- vec_viridis
    vec_colorVals <- rescale(c(min(vec_legendBreaksDelta),
                               0,
                               mean(c(0, max(vec_legendBreaksDelta))),
                               max(vec_legendBreaksDelta)))
  }
  
  # # plot delta maps
  # p_deltas <- list()
  # for(m in 1:length(list_rDeltas)){
  #   p_deltas[[m]] <- ggplot() +
  #     
  #     # plot deltas
  #     geom_raster(data = plotdatDelta[plotdatDelta$model ==
  #                                       names(list_rDeltas)[[m]],],
  #                 aes(x = x, y = y, fill = delta)) +
  #     scale_fill_gradientn(limits = range(vec_legendBreaksDelta),
  #                          breaks = vec_legendBreaksDelta,
  #                          labels = vec_legendLabelsDelta,
  #                          values = vec_colorVals,
  #                          colors = vec_viridisDelta) +
  #     
  #     # gray-out statistically-insignificant deltas
  #     geom_raster(data = plotdatDeltaInsig[plotdatDeltaInsig$model ==
  #                                            names(list_rDeltas)[[m]],],
  #                 aes(x = x, y = y), fill = 'lightgray') +
  #     
  #     coord_equal() +
  #     theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
  #           panel.background = element_blank(), text = element_text(size = 16),
  #           legend.key.height= unit(0.75, 'cm')) +
  #     labs(fill = 'Change in fire\nprobability (%)',
  #          title = names(list_rDeltas)[[m]])
  # }
  
  # alternate plot: increase or decrease rather than percent change
  plotdatDelta$deltaCategory <-
    with(plotdatDelta,
         ifelse(delta < 0, 'Decrease',
         ifelse(delta > 0, 'Increase',
         ifelse(delta == 0, 'No change',
                NA)
         )
         )
         )
  plotdatDelta$deltaCategory <-
    factor(plotdatDelta$deltaCategory,
           levels = c('Increase', 'No change', 'Decrease'))
  
  p_deltas <- list()
  for(m in 1:length(list_rDeltas)){
    p_deltas[[m]] <- ggplot() +
      
      # plot deltas
      geom_raster(data = plotdatDelta[plotdatDelta$model ==
                                        names(list_rDeltas)[[m]],],
                  aes(x = x, y = y, fill = deltaCategory)) +
      scale_fill_manual(values = c(turbo(2)[[2]], 'lightgray', turbo(2)[[1]]),
                        drop = FALSE) +
      
      # gray-out statistically-insignificant deltas
      geom_raster(data = plotdatDeltaInsig[plotdatDeltaInsig$model ==
                                             names(list_rDeltas)[[m]],],
                  aes(x = x, y = y), fill = 'lightgray') +
      
      coord_equal() +
      theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
            panel.background = element_blank(), text = element_text(size = 16),
            legend.key.height= unit(0.75, 'cm')) +
      labs(fill = 'Change in fire\nprobability',
           title = names(list_rDeltas)[[m]])
  }
  
  gc()
  
  # # alternate plot: increase/decrease bins instead of continuous values
  # plotdatDelta$deltaCategory <-
  #   with(plotdatDelta,
  #        ifelse(delta < 0, 'Decrease',
  #        ifelse(delta > 0 & delta <= quantile(delta, probs = 0.75), 'Slight increase',
  #        ifelse(delta > quantile(delta, probs = 0.75), 'Increase',
  #        ifelse(delta == 0, 'No change',
  #               NA)
  #        )
  #        )
  #        )
  #        )
  # plotdatDelta$deltaCategory <-
  #   factor(plotdatDelta$deltaCategory,
  #          levels = c('Increase', 'Slight increase', 'No change', 'Decrease')
  #          )
  # 
  # p_deltas <- list()
  # for(m in 1:length(list_rDeltas)){
  #   p_deltas[[m]] <- ggplot() +
  #     
  #     # plot deltas
  #     geom_raster(data = plotdatDelta[plotdatDelta$model ==
  #                                       names(list_rDeltas)[[m]],],
  #                 aes(x = x, y = y, fill = deltaCategory)) +
  #     scale_fill_manual(values = c(turbo(3)[[3]], turbo(3)[[2]], 'lightgray', turbo(3)[[1]]),
  #                       drop = FALSE) +
  #     
  #     # gray-out statistically-insignificant deltas
  #     geom_raster(data = plotdatDeltaInsig[plotdatDeltaInsig$model ==
  #                                            names(list_rDeltas)[[m]],],
  #                 aes(x = x, y = y), fill = 'lightgray') +
  #     
  #     coord_equal() +
  #     theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
  #           panel.background = element_blank(), text = element_text(size = 16),
  #           legend.key.height= unit(0.75, 'cm')) +
  #     labs(fill = 'Change in fire\nprobability',
  #          title = names(list_rDeltas)[[m]])
  # }
  
  
  
  
  ### create panel of maps ###
  
  # plot grid of maps
  plot_grid(p_current, p_deltas[[1]], p_deltas[[3]], p_deltas[[2]],
            nrow = 2, ncol = 2)
  
  # save grid as png
  ggsave2(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Current vs future climate/',
                            'panel - annual - current and deltas ', vec_islands[[i]], '.png'),
          width = 10, height = 8, dpi = 300)
  
  dev.off()
  gc()
}











# ##### OLD CODE ----- create standardized legend color scales #####
# 
# # levels color scale
# rasters_allAnnualMaxLevelsCurrent <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
#                                                 pattern = 'annual')
# rasters_allAnnualMaxLevelsCurrent <- lapply(rasters_allAnnualMaxLevelsCurrent, function(r){
#   raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
#                 r))
# })
# rasters_allAnnualMaxLevelsFuture <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
#                                                pattern = 'annual')
# rasters_allAnnualMaxLevelsFuture <- lapply(rasters_allAnnualMaxLevelsFuture, function(r){
#   raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
#                 r))
# })
# rasters_allAnnualMaxLevels <- c(rasters_allAnnualMaxLevelsCurrent, rasters_allAnnualMaxLevelsFuture)
# vec_levelsVals <- unlist(sapply(rasters_allAnnualMaxLevels, values))
# dat_limitsLevels <- range(vec_levelsVals, na.rm = TRUE)
# dat_breaksLevels <- c(seq(dat_limitsLevels[[1]], dat_limitsLevels[[2]],
#                           length.out = 7))
# dat_breaksLevels <- dat_breaksLevels[order(dat_breaksLevels)]
# dat_breaksLevelsLabels <- round(dat_breaksLevels, 2)
# #vec_levelsColors <- terrain.colors(length(dat_breaksLevelsLabels))
# vec_levelsColors <- viridis(n = length(dat_breaksLevelsLabels))
# 
# # delta color scale
# rasters_allAnnualMaxDeltas <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
#                                          pattern = 'annual')
# rasters_allAnnualMaxDeltas <- lapply(rasters_allAnnualMaxDeltas, function(r){
#   raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
#                 r))
# })
# vec_deltaVals <- unlist(sapply(rasters_allAnnualMaxDeltas, values))
# vec_deltaVals[vec_deltaVals > quantile(vec_deltaVals, probs = c(0.999), na.rm = TRUE)[[1]]] <-
#   quantile(vec_deltaVals, probs = c(0.999), na.rm = TRUE)[[1]]   # replace outliers with max non-outlier value for plotting purposes
# dat_limitsDelta <- range(vec_deltaVals, na.rm = TRUE)
# if(max(dat_limitsDelta) > max(dat_limitsLevels)){
#   dat_limitsDelta[[2]] <- max(dat_limitsLevels)  # if it happens, replace max delta with max fire prob
# }
# dat_breaksDelta <- c(seq(dat_limitsDelta[[1]], dat_limitsDelta[[2]],
#                          length.out = 5))
# dat_breaksDelta <- dat_breaksDelta[order(dat_breaksDelta)]
# dat_breaksDeltaLabels <- round(dat_breaksDelta, 2)
# vec_deltaColors <- c('darkblue', 'green4', 'yellow', 'orange', 'red')
# 
# rm(rasters_allAnnualMaxDeltas, rasters_allAnnualMaxLevels,
#    rasters_allAnnualMaxLevelsCurrent, rasters_allAnnualMaxLevelsFuture)
# gc()
# 
# 
# 
# 
# ##### OLD CODE ----- plots - seasonal #####
# 
# for(i in 1:length(vec_islands)){
#   for(s in 1:length(vec_season)){
#     for(m in 1:length(vec_model)){
#       for(y in 1:length(vec_year)){
#         
#         # we don't have mid-century data for dynamical model - skip it
#         if(vec_year[[y]] == '2070' & vec_model[[m]] == 'dyn') next
#         
#         
#         
#         
#         ### determine rasters for scenario ###
#         
#         # get filename for current risk
#         vec_filenamesCurrent_is <- grep(vec_islands[[i]], vec_filenamesCurrent, value = TRUE)
#         vec_filenamesCurrent_is <- grep(vec_season[[s]], vec_filenamesCurrent_is, value = TRUE)
#         
#         # get filename for future risk
#         vec_filenamesFuture_is <- grep(vec_islands[[i]], vec_filenamesFuture, value = TRUE)
#         vec_filenamesFuture_is <- grep(vec_season[[s]], vec_filenamesFuture_is, value = TRUE)
#         vec_filenamesFuture_is <- grep(vec_model[[m]], vec_filenamesFuture_is, value = TRUE)
#         vec_filenamesFuture_is <- grep(vec_year[[y]], vec_filenamesFuture_is, value = TRUE)
#         
#         # get filename for delta
#         vec_filenamesDelta_is <- grep(vec_islands[[i]], vec_filenamesDelta, value = TRUE)
#         vec_filenamesDelta_is <- grep(vec_season[[s]], vec_filenamesDelta_is, value = TRUE)
#         vec_filenamesDelta_is <- grep(vec_model[[m]], vec_filenamesDelta_is, value = TRUE)
#         vec_filenamesDelta_is <- grep(vec_year[[y]], vec_filenamesDelta_is, value = TRUE)
#         
#         
#         
#         
#         ### read rasters and convert to data.frames ###
#         
#         # current
#         rCurrent <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
#                                   vec_filenamesCurrent_is))
#         datCurrent <- as.data.frame(rCurrent, xy = TRUE)
#         datCurrent <- datCurrent[complete.cases(datCurrent),]; gc()
#         colnames(datCurrent) <- c('x', 'y', 'prob')
#         
#         # future
#         rFuture <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
#                                   vec_filenamesFuture_is))
#         datFuture <- as.data.frame(rFuture, xy = TRUE)
#         datFuture <- datFuture[complete.cases(datFuture),]; gc()
#         colnames(datFuture) <- c('x', 'y', 'prob')
#         
#         # delta
#         rDelta <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/05_climate_change_max_probability_delta/wet-dry season/',
#                                  vec_filenamesDelta_is))
#         datDelta <- as.data.frame(rDelta, xy = TRUE)
#         datDelta <- datDelta[complete.cases(datDelta),]; gc()
#         colnames(datDelta) <- c('x', 'y', 'prob')
#         
#         rm(rCurrent, rFuture, rDelta); gc()
#         
#         
#         
#         
#         ### combine current and future data.frames ###
#         
#         # create time period variable
#         datCurrent$period <- 'Current'
#         datFuture$period <- 'Future'
#         
#         # combine data.frames
#         datCombinedCurrentFuture <- rbind(datCurrent, datFuture)
#         rm(datCurrent, datFuture); gc()
#         
#         # add column that cuts probability vector according to risk levels
#         datCombinedCurrentFuture$riskLevel <-
#           cut(datCombinedCurrentFuture$prob,
#               breaks = c(0,
#                          highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                             c("hiRiskThresh_25pctileBurned", "hiRiskThresh_50pctileBurned", "hiRiskThresh_75pctileBurned")],
#                          1),
#               labels = c('Low', 'Moderate', 'High', 'Very high'),
#               include.lowest = TRUE, right = FALSE)
#         
#         
#         
#         
#         ### prepare current/future data.frame for plotting ###
#         
#         # convert probability to percentage
#         datCombinedCurrentFuture$prob_pct <- datCombinedCurrentFuture$prob * 100
#         
#         # for plotting purposes, replace outlier values with 99.99th percentile
#         upperLimit <- quantile(datCombinedCurrentFuture$prob_pct, probs = 0.9999)
#         datCombinedCurrentFuture$prob_pct[datCombinedCurrentFuture$prob_pct > upperLimit] <- upperLimit
#         rm(upperLimit)
#         
#         
#         
#         
#         ### plot combined current/future probability map - levels ###
# 
#         plot_currentFuture <- ggplot(data = datCombinedCurrentFuture,
#                aes(x = x, y = y, fill = prob_pct)) +
#           geom_tile() +
#           scale_fill_gradientn(colors = vec_levelsColors,
#                                limits = dat_limitsLevels * 100,
#                                breaks = dat_breaksLevels * 100,
#                                labels = dat_breaksLevelsLabels * 100) +
#           facet_grid(cols = vars(period)) +
#           coord_equal() +
#           theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
#                 panel.background = element_blank(), text = element_text(size = 15),
#                 legend.key.height= unit(0.75, 'cm')) +
#           labs(fill = 'Annual fire\nprobability (%)')
#         
#         
#         
#         
#         ### prepare delta data.frame for plotting ###
#         
#         # convert probability to percentage
#         datDelta$prob_pct <- datDelta$prob * 100
#         
#         # if change in probability is greater than the max probability from current/future data.frame, change it to that max value
#           # this is due to adjusting the values for outliers in the current/future data.frame
#         datDelta$prob_pct[datDelta$prob_pct > max(dat_limitsLevels)*100] <-
#           max(dat_limitsLevels)*100
#         # datDelta$prob_pct[datDelta$prob_pct < -1 * max(datCombinedCurrentFuture$prob_pct)] <-
#         #   -1 * max(datCombinedCurrentFuture$prob_pct)
#         
#         # # create plot legend range and breaks
#         # dat_limitsDelta <- c(floor(min(datDelta$prob_pct)), ceiling(max(datDelta$prob_pct)))
#         # dat_breaksDelta <- c(seq(dat_limitsDelta[[1]],
#         #                          dat_limitsDelta[[2]],
#         #                          length.out = 4),
#         #                      0)  # include 0 as a legend label
#         # dat_breaksDelta <- dat_breaksDelta[order(dat_breaksDelta)]  # order legend breaks
#         
#         
#         # get deltas that are 0 (insignificant change in probability)
#         datDeltaZero <- datDelta[datDelta$prob_pct == 0,]
#         
#         # # if all deltas are 0, create new limits and breaks vectors
#         # if(dat_limitsDelta[[1]] == dat_limitsDelta[[2]]){
#         #   dat_limitsDelta <- c(-1, 1)
#         #   dat_breaksDelta <- seq(-1, 1, length.out = 5)
#         # }
#         
#         # # create break labels
#         # dat_breaksDeltaLabels <- round(dat_breaksDelta, 2)
#         # # if any break value is too close to 0, remove the label
#         # breakDiff <- abs(0 - dat_breaksDelta)
#         # dat_breaksDeltaLabels <-
#         #   as.character(ifelse(breakDiff < 3 & breakDiff !=0, '', dat_breaksDeltaLabels))
#         
#         # # create delta color palettes
#         # if(length(dat_breaksDelta[dat_breaksDelta < 0] == 0)){
#         #   vec_deltaColors <- c('green4', 'yellow', 'orange', 'orangered', 'red')
#         # } else if(length(dat_breaksDelta[dat_breaksDelta < 0] == 1)) {
#         #   vec_deltaColors <- c('darkgreen', 'green4', 'yellow', 'orange', 'red')
#         # } else if(length(dat_breaksDelta[dat_breaksDelta < 0] == 2)) {
#         #   vec_deltaColors <- c('darkolivegreen', 'darkgreen', 'green4', 'yellow', 'red')
#         # } else if(length(dat_breaksDelta[dat_breaksDelta < 0] == 3)) {
#         #   vec_deltaColors <- c('darkslategrey', 'darkolivegreen', 'darkgreen', 'green4', 'red')
#         # } else if(length(dat_breaksDelta[dat_breaksDelta < 0] == 4)) {
#         #   vec_deltaColors <- c('darkblue', 'darkslategrey', 'darkolivegreen', 'darkgreen', 'green4')
#         # }
#         
#         
#         
#         
#         
#         # ### plot probability delta map with insignificant deltas grayed out ###
#         # 
#         # plot_delta <- ggplot() +
#         #   
#         #   # plot deltas
#         #   geom_tile(data = datDelta,
#         #             aes(x = x, y = y, fill = prob_pct)) +
#         #   scale_fill_gradientn(colors = vec_deltaColors,
#         #                        limits = dat_limitsDelta * 100,
#         #                        breaks = dat_breaksDelta * 100,
#         #                        labels = dat_breaksDeltaLabels * 100) +
#         #   
#         #   # plot insignificant deltas and gray them out
#         #   geom_tile(data = datDeltaZero,
#         #             aes(x = x, y = y), fill = 'lightgray') +
#         #   
#         #   # set other options
#         #   coord_equal() +
#         #   theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
#         #         panel.background = element_blank(), text = element_text(size = 15),
#         #         legend.key.height= unit(0.75, 'cm')) +
#         #   labs(fill = 'Change in fire\nprobability (%)')
#         # 
#         # 
#         # 
#         # 
#         # ### combine plots ###
#         # 
#         # plot_grid(plot_currentFuture, plot_delta, nrow = 2)
#         # 
#         # ggsave2(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Current vs future climate/',
#         #                           'change in probability ', vec_islands[[i]], ' ', vec_year[[y]], ' ', vec_season[[s]], ' season ', vec_model[[m]], '.png'),
#         #         width = 8, height = 5, dpi = 300)
#         
#       }
#     }
#   }
# }

