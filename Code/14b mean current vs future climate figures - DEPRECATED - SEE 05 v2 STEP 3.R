
# this script creates the mean current climate figures used inthe published report

library(raster); library(ggplot2); library(scales); library(reshape2)
library(gridExtra);  library(cowplot); library(snow); library(tidyverse)
library(ggpubr)




##### data loading #####

# list all mean rasters - current climate
list_filenamesCurrent <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                                    pattern = '.tif')

# list all mean rasters - future climate (RCP 8.5 only)
list_filenamesFuture <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                                   pattern = '.tif')
list_filenamesFuture <-
  list_filenamesFuture[-grep('rcp45', list_filenamesFuture)]  # remove RCP 4.5 scenarios

# list all delta rasters w/ insignificant deltas = 0
list_filenamesSignifDelta <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/',
                                        pattern = '.tif')
list_filenamesSignifDelta <-
  list_filenamesSignifDelta[-grep('rcp45', list_filenamesSignifDelta)]  # remove RCP 4.5 scenarios

# define islands, time period
list_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')
list_timePeriods <- c('2070', '2100')
list_models <- c('_dyn_', '_sta_')
list_seasons <- c('wet', 'dry', 'annual')

# define wet and dry season months
list_wetSeason <- c('month05','month06','month07','month08','month09')
list_drySeason <- c('month10','month11','month12','month01','month02','month03','month04')




##### create plots #####

for(i in 1:length(list_islands)){
  for(t in 1:length(list_timePeriods)){
    for(m in 1:length(list_models)){
      for(s in 1:length(list_seasons))
      
      # skip dynamical mid-century
      if(t == 1 & m == 1) next
      
      
      
      
      ##### load rasters #####
      
      # get rasters: mean current climate, separated by pred value and SE
      list_filenamesCurrent_islandi <-
        list_filenamesCurrent[grep(list_islands[[i]], list_filenamesCurrent)]  # subset: island i
      if(s == 1){
        list_filenamesCurrent_islandi <-
          list_filenamesCurrent_islandi[grep(paste(list_wetSeason, collapse = '|'),
                                             list_filenamesCurrent_islandi)]   # subset: season s
      } else if(s == 2){
        list_filenamesCurrent_islandi <-
          list_filenamesCurrent_islandi[grep(paste(list_drySeason, collapse = '|'),
                                             list_filenamesCurrent_islandi)]
      } else if(s == 3){
        list_filenamesCurrent_islandi <- list_filenamesCurrent_islandi
      }
      list_filenamesCurrent_SE_islandi <-
        list_filenamesCurrent_islandi[grep('_SE', list_filenamesCurrent_islandi)]
      list_filenamesCurrent_islandi <-
        list_filenamesCurrent_islandi[-grep('_SE', list_filenamesCurrent_islandi)]
      list_probRastersCurrent <- lapply(list_filenamesCurrent_islandi, function(r){
        raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                      r))
      })
      
      # get rasters: mean future climate, separated by pred value and SE
      list_filenamesFuture_islandi <-
        list_filenamesFuture[grep(list_islands[[i]], list_filenamesFuture)]  # subset: island i
      list_filenamesFuture_islandi_timePeriodt <-
        list_filenamesFuture_islandi[grep(list_timePeriods[[t]], list_filenamesFuture_islandi)]  # subset: time period t
      list_filenamesFuture_islandi_timePeriodt <-
        list_filenamesFuture_islandi_timePeriodt[grep(list_models[[m]], list_filenamesFuture_islandi_timePeriodt)]  # subset: model m
      if(s == 1){
        list_filenamesFuture_islandi_timePeriodt <-
          list_filenamesFuture_islandi_timePeriodt[grep(paste(list_wetSeason, collapse = '|'),
                                     list_filenamesFuture_islandi_timePeriodt)]   # subset: season s
      } else if(s == 2){
        list_filenamesFuture_islandi_timePeriodt <-
          list_filenamesFuture_islandi_timePeriodt[grep(paste(list_drySeason, collapse = '|'),
                                     list_filenamesFuture_islandi_timePeriodt)]
      } else if(s == 3){
        list_filenamesFuture_islandi_timePeriodt <- list_filenamesFuture_islandi_timePeriodt
      }
      list_filenamesFuture_SE_islandi_timePeriodt <-
        list_filenamesFuture_islandi_timePeriodt[grep('_SE', list_filenamesFuture_islandi_timePeriodt)]
      list_filenamesFuture_islandi_timePeriodt <-
        list_filenamesFuture_islandi_timePeriodt[-grep('_SE', list_filenamesFuture_islandi_timePeriodt)]
      list_probRastersFuture_timePeriodt <- lapply(list_filenamesFuture_islandi_timePeriodt, function(r){
        raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                      r))
      })
      
      # get rasters: deltas with insignificant deltas zeroed out
      list_filenamesSignifDelta_islandi <-
        list_filenamesSignifDelta[grep(list_islands[[i]], list_filenamesSignifDelta)]  # subset island i
      list_filenamesSignifDelta_islandi_timePeriodt <-
        list_filenamesSignifDelta_islandi[grep(list_timePeriods[[t]], list_filenamesSignifDelta_islandi)]  # subset: time period t
      list_filenamesSignifDelta_islandi_timePeriodt <-
        list_filenamesSignifDelta_islandi_timePeriodt[grep(list_models[[m]], list_filenamesSignifDelta_islandi_timePeriodt)]  # subset: model m
      if(s == 1){
        list_filenamesSignifDelta_islandi_timePeriodt <-
          list_filenamesSignifDelta_islandi_timePeriodt[grep(paste(list_wetSeason, collapse = '|'),
                                     list_filenamesSignifDelta_islandi_timePeriodt)]   # subset: season s
      } else if(s == 2){
        list_filenamesSignifDelta_islandi_timePeriodt <-
          list_filenamesSignifDelta_islandi_timePeriodt[grep(paste(list_drySeason, collapse = '|'),
                                     list_filenamesSignifDelta_islandi_timePeriodt)]
      } else if(s == 3){
        list_filenamesSignifDelta_islandi_timePeriodt <- list_filenamesSignifDelta_islandi_timePeriodt
      }
      list_deltaRasters_timePeriodt <- lapply(list_filenamesSignifDelta_islandi_timePeriodt, function(r){
        raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_deltas/delta_rasters_with_significance/',
                      r))
      })
      
      rm(list_filenamesCurrent_islandi, list_filenamesCurrent_SE_islandi,
         list_filenamesFuture_islandi, list_filenamesFuture_islandi_timePeriodt, list_filenamesFuture_SE_islandi_timePeriodt,
         list_filenamesSignifDelta_islandi, list_filenamesSignifDelta_islandi_timePeriodt)
      
      
      
      
      ##### get mean of rasters #####
      
      # stack rasters
      rstackDelta       <- stack(list_deltaRasters_timePeriodt)
      rstackProbCurrent <- stack(list_probRastersCurrent)
      rstackProbFuture  <- stack(list_probRastersFuture_timePeriodt)
      rm(list_deltaRasters_timePeriodt, list_probRastersCurrent, list_probRastersFuture_timePeriodt)
      
      # get pixel-wise mean
      beginCluster(8)
      meanDelta <- clusterR(rstackDelta, mean)
      meanProbCurrent <- clusterR(rstackProbCurrent, mean)
      meanProbFuture <- clusterR(rstackProbFuture, mean)
      endCluster()
      
      # convert to data.frames and merge into one data.frame
      datDelta <- as.data.frame(meanDelta, xy = TRUE)
      colnames(datDelta) <- c('x', 'y', 'Delta')
      datDelta <- datDelta[!is.na(datDelta$Delta),]
      datProbCurrent <- as.data.frame(meanProbCurrent, xy = TRUE)
      colnames(datProbCurrent) <- c('x', 'y', 'Current')
      datProbCurrent <- datProbCurrent[!is.na(datProbCurrent$Current),]
      datProbFuture <- as.data.frame(meanProbFuture, xy = TRUE)
      colnames(datProbFuture) <- c('x', 'y', 'Future')
      datProbFuture <- datProbFuture[!is.na(datProbFuture$Future),]
      dat <- left_join(datDelta, datProbCurrent, by = c('x', 'y'))
      dat <- left_join(dat, datProbFuture, by = c('x', 'y'))
      rm(datDelta, datProbCurrent, datProbFuture,
         meanDelta, meanProbCurrent, meanProbFuture,
         rstackDelta, rstackProbCurrent, rstackProbFuture)
      gc()
      
      
      
      
      ##### plot absolute values #####
      
      # melt data.frame
      plotdat <- melt(dat, id.vars = c('x', 'y'),
                      measure.vars = c('Delta', 'Current', 'Future'))
      plotdat <- plotdat[complete.cases(plotdat),]
      plotdat0 <- plotdat[plotdat$value == 0 & plotdat$variable == 'Delta',]
      rm(dat); gc()
      
      # convert values to percents
      plotdat$pct <- plotdat$value * 100
      
      # get range of values for pcts and deltas
      range_pct <- range(plotdat$pct[plotdat$variable %in% c('Current', 'Future')])
      range_delta <- range(plotdat$pct[plotdat$variable == 'Delta'])
      
      # create plot breaks
      breaks_pct <- seq(min(range_pct),
                        max(range_pct),
                        length.out = 4)
      breaks_delta <- seq(min(range_delta),
                          max(range_delta),
                          length.out = 4)
      
      # plot current fire risk
      p_current <- ggplot(data = plotdat[plotdat$variable == 'Current',]) +
        geom_tile(aes(x = x, y = y, fill = pct)) +
        coord_equal() +
        scale_fill_gradientn(colors = terrain.colors(7),
                             limits = range_pct,
                             breaks = breaks_pct,
                             labels = round(breaks_pct, 2)) +
        theme(legend.position = 'bottom', axis.text = element_blank(),
              axis.ticks = element_blank(), axis.title = element_blank(),
              panel.background = element_blank(),
              legend.key.width = unit(0.5, 'in'),
              text = element_text(size = 11)) +
        labs(title = 'Current fire risk', fill = NULL)
      
      # plot future fire risk
      p_future <- ggplot(data = plotdat[plotdat$variable == 'Future',]) +
        geom_tile(aes(x = x, y = y, fill = pct)) +
        coord_equal() +
        scale_fill_gradientn(colors = terrain.colors(7),
                             limits = range_pct,
                             breaks = breaks_pct,
                             labels = round(breaks_pct, 2)) +
        theme(legend.position = 'bottom', axis.text = element_blank(),
              axis.ticks = element_blank(), axis.title = element_blank(),
              panel.background = element_blank(),
              legend.key.width = unit(0.5, 'in'),
              text = element_text(size = 11)) +
        labs(title = paste0('Fire risk, ', list_timePeriods[[t]]), fill = NULL)
      
      # plot fire risk delta
      p_delta <- ggplot() +
        geom_tile(data = plotdat[plotdat$variable == 'Delta',],
                  aes(x = x, y = y, fill = pct)) +
        scale_fill_gradientn(colors = terrain.colors(7),
                             limits = range_delta,
                             breaks = breaks_delta,
                             labels = round(breaks_delta, 2)) +
        geom_tile(data = plotdat0,
                  aes(x = x, y = y), fill = 'gray') +
        coord_equal() +
        theme(legend.position = 'right', axis.text = element_blank(),
              axis.ticks = element_blank(), axis.title = element_blank(),
              panel.background = element_blank(),
              legend.key.width = unit(0.5, 'in'),
              text = element_text(size = 11)) +
        labs(title = 'Fire risk delta', fill = NULL)
      
      # combine current and future plots
      p_currentAndFuture <- ggarrange(p_current, p_future,
                                      common.legend = TRUE, legend = 'right')
      
      # combine p_currentAndFuture with p_delta
      p_combined <- ggarrange(p_currentAndFuture, p_delta, common.legend = FALSE,
                              nrow = 2)
      
      ggsave(plot = p_combined,
             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Current vs future climate/current vs future climate ',
                               list_models[[m]], ' rcp85 ', list_timePeriods[[t]], ' ', list_islands[[i]], ' ', list_seasons[[s]], '.png'),
             height = 4, width = 8, dpi = 300)
      
      gc()
      
    }
  }
}
