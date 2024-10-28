
# This script creates a panel of fire risk deltas under climate change

library(terra)
library(ggplot2)
library(cowplot)




##### load data #####

# get raster names - mid-century statistical RCP 4.5
list_rast_midSta45 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                               full.names = TRUE, pattern = 'sta_rcp45_2070')
list_rast_midSta45 <- list_rast_midSta45[grep('waterMasked', list_rast_midSta45)]
list_rast_midSta45_dry <- list_rast_midSta45[grep('dry', list_rast_midSta45)]
list_rast_midSta45_wet <- list_rast_midSta45[grep('wet', list_rast_midSta45)]
rm(list_rast_midSta45)

# get raster names - mid-century statistical RCP 8.5
list_rast_midSta85 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                               full.names = TRUE, pattern = 'sta_rcp85_2070')
list_rast_midSta85 <- list_rast_midSta85[grep('waterMasked', list_rast_midSta85)]
list_rast_midSta85_dry <- list_rast_midSta85[grep('dry', list_rast_midSta85)]
list_rast_midSta85_wet <- list_rast_midSta85[grep('wet', list_rast_midSta85)]
rm(list_rast_midSta85)

# get raster names - late-century statistical RCP 4.5
list_rast_lateSta45 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                                 full.names = TRUE, pattern = 'sta_rcp45_2100')
list_rast_lateSta45 <- list_rast_lateSta45[grep('waterMasked', list_rast_lateSta45)]
list_rast_lateSta45_dry <- list_rast_lateSta45[grep('dry', list_rast_lateSta45)]
list_rast_lateSta45_wet <- list_rast_lateSta45[grep('wet', list_rast_lateSta45)]
rm(list_rast_lateSta45)

# get raster names - late-century statistical RCP 8.5
list_rast_lateSta85 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                                 full.names = TRUE, pattern = 'sta_rcp85_2100')
list_rast_lateSta85 <- list_rast_lateSta85[grep('waterMasked', list_rast_lateSta85)]
list_rast_lateSta85_dry <- list_rast_lateSta85[grep('dry', list_rast_lateSta85)]
list_rast_lateSta85_wet <- list_rast_lateSta85[grep('wet', list_rast_lateSta85)]
rm(list_rast_lateSta85)

# get raster names - late-century dynamical RCP 4.5
list_rast_lateDyn45 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                                  full.names = TRUE, pattern = 'dyn_rcp45_2100')
list_rast_lateDyn45 <- list_rast_lateDyn45[grep('waterMasked', list_rast_lateDyn45)]
list_rast_lateDyn45_dry <- list_rast_lateDyn45[grep('dry', list_rast_lateDyn45)]
list_rast_lateDyn45_wet <- list_rast_lateDyn45[grep('wet', list_rast_lateDyn45)]
rm(list_rast_lateDyn45)

# get raster names - late-century dynamical RCP 8.5
list_rast_lateDyn85 <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                                  full.names = TRUE, pattern = 'dyn_rcp85_2100')
list_rast_lateDyn85 <- list_rast_lateDyn85[grep('waterMasked', list_rast_lateDyn85)]
list_rast_lateDyn85_dry <- list_rast_lateDyn85[grep('dry', list_rast_lateDyn85)]
list_rast_lateDyn85_wet <- list_rast_lateDyn85[grep('wet', list_rast_lateDyn85)]
rm(list_rast_lateDyn85)




##### create plots #####

listName <- list_rast_lateDyn45_dry

vec_islands <- c('Hawaii', 'Kauai', 'Maui County', 'Oahu')

# create function to plot rasters
fcn_plot <- function(listName){
  
  # load list of rasters
  list_rast <- lapply(listName, rast)
  names(list_rast) <- listName
  
  # convert to data.frames
  list_df <- lapply(list_rast, as.data.frame, xy = TRUE)
  names(list_df) <- listName
  gc()
  
  # add island name
  for(i in 1:length(list_df)){
    list_df[[i]]$island <- vec_islands[[i]]
    colnames(list_df[[i]]) <- c('x', 'y', 'delta', 'island')
  }
  
  # remove NAs
  list_df <- lapply(list_df, function(df) df[!is.na(df$delta),])
  gc()
  
  # cut risk delta into bins
  list_df <-
    lapply(list_df, function(df){
      cuts <- quantile(df$delta, probs=0:5/5, digits = 10)
      cuts <- cuts[!duplicated(cuts)]
      df$delta_bin <- cut(df$delta, cuts, include.lowest = TRUE)
    })
  
  # combine into one data.frame for plotting
  dat_plot <- do.call(rbind, list_df)
  rm(list_df, list_rast)
  gc()
  
  # # plot panels
  # ggplot(data = dat_plot, aes(x = x, y = y, fill = delta)) +
  #   geom_raster() +
  #   coord_equal() +
  #   scale_fill_viridis_c(option = 'plasma') +
  #   facet_wrap(vars(island), ncol = 2) +
  #   theme(text = element_text(size = 14))
  # gc()
  
  # plot each island individually then combine into panel manually
  list_plots <- list()
  for(i in 1:length(vec_islands)){
    list_plots[[i]] <-
      ggplot(data = dat_plot[dat_plot$island == vec_islands[[i]],],
             aes(x = x, y = y, fill = delta)) +
      geom_raster() +
      coord_equal() +
      scale_fill_viridis_c(option = 'plasma') +
      theme(text = element_text(size = 14),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())

  }
}
