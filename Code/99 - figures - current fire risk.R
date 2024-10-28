
# This script creates plots of current fire risk

library(terra)
library(ggplot2)
library(cowplot)
library(ggsn)




##### load rasters #####

# list rasters
list_rasters <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
             full.names = TRUE, pattern = 'max_current_fire_prob_annual')
list_rasters <- list_rasters[grep('waterMasked', list_rasters)]
list_rasters <- list_rasters[-grep('statewide', list_rasters)]

# load rasters
list_rasters <- lapply(list_rasters, rast)
names(list_rasters) <- c('Hawaii', 'Kauai', 'Maui County', 'Oahu')

# load risk cutoffs
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata')
rm(dat_fire, dat_fire2)




##### format for plotting #####

# convert to data.frames
list_df <- lapply(list_rasters, as.data.frame, xy = TRUE)
names(list_df) <- names(list_rasters)
list_df <- lapply(list_df, function(df){colnames(df) <- c('x', 'y', 'risk'); df} )
list_df <- lapply(list_df, function(df) df[!is.na(df$risk),] )
gc()

# combine into one data.frame (with aggregated pixels)
# list_rasters_aggregated <-
#   lapply(list_rasters, terra::aggregate,fun = 'max', fact = 3, na.rm = TRUE)
# list_df_aggregated <-
#   lapply(list_rasters_aggregated, as.data.frame, xy = TRUE)
list_df_aggregated <- list_df
list_df_aggregated <- lapply(list_df_aggregated, function(df){colnames(df) <- c('x', 'y', 'risk'); df} )
list_df_aggregated <- lapply(list_df_aggregated, function(df) df[!is.na(df$risk),] )
df <- do.call(rbind, list_df_aggregated)
gc()

# create breaks then plot each island individually
list_plots <- list()
list_plotdat <- list()
for(i in 1:length(list_df)){
  
  # get data.frame and rename columns for scalebar
  df_i <- list_df[[i]]
  colnames(df_i)[colnames(df_i) == 'x'] <- 'long'
  colnames(df_i)[colnames(df_i) == 'y'] <- 'lat'
  
  # create risk breaks
  island_i <- gsub(' ', '', names(list_df)[[i]])
  risk_cuts <- unlist(c(0, highRiskThresholds[highRiskThresholds$island == island_i, 2:4] * 100, 100))     # island-specific categories
  risk_cuts <- unlist(c(0, highRiskThresholds[highRiskThresholds$island == 'Statewide', 2:4] * 100, 100))  # statewide categories
  df_i$risk_bin <- cut(df_i$risk*100, breaks = risk_cuts, include.lowest = TRUE,
                       labels = c('Low', 'Moderate', 'High', 'Very high'))
  
  # plot
  p <-
    ggplot() +
    geom_raster(data = df_i, aes(x = long, y = lat, fill = risk_bin)) +
    scale_fill_viridis_d(option = 'viridis') +
    scalebar(data = df_i, dist = 10, dist_unit = "km",
             transform = TRUE, model = "WGS84", location = 'bottomleft') +
    north(data = df_i, symbol = 10) +
    labs(fill = 'Annual fire risk') +
    coord_equal() +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          text = element_text(size = 20))
  
  list_plotdat[[i]] <- df_i
  list_plots[[i]] <- p
  rm(df_i, island_i, risk_cuts)
  gc()
}

# create statewide plot
plotdat <- do.call(rbind, list_plotdat)
ggplot() +
  geom_raster(data = plotdat, aes(x = long, y = lat, fill = risk_bin)) +
  scale_fill_viridis_d(option = 'viridis') +
  scalebar(data = plotdat, dist = 50, dist_unit = "km",
           transform = TRUE, model = "WGS84", location = 'bottomleft') +
  north(data = plotdat, symbol = 10) +
  labs(fill = 'Annual fire risk') +
  coord_equal() +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 14))
