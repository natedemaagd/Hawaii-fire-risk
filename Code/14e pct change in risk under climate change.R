
# This script calculates and plots the change in fire risk under climate change.

library(terra); library(ggplot2); library(dplyr); library(viridis)




##### load data #####

# risk raster - current climate
ras_current <- rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/max_current_fire_prob_annual_statewide_waterMasked.tif')
ras_current[ras_current == 0] <- 1e-10  # for % change calculations, can't divide by 0

# risk rasters - future climate
list_ras_future <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final',
                              full.names = TRUE, pattern = '.tif')
list_ras_future <- list_ras_future[grep('delta_max_future', list_ras_future)]
list_ras_future <- list_ras_future[grep('statewide_annual_waterMasked', list_ras_future)]
list_ras_future <- list_ras_future[grep('rcp85', list_ras_future)]
list_ras_future <- lapply(list_ras_future, rast)
names(list_ras_future) <- c('dyn2100', 'sta2070', 'sta2100')




##### calculate percent change in risk #####

# convert future rasters from deltas to levels
list_ras_future <- lapply(list_ras_future, function(r) r + ras_current)

# use current and future rasters to calculate percent change for each pixel
list_ras_future_pctChange <-
  lapply(list_ras_future, function(r){ (r - ras_current) / ras_current * 100 })
names(list_ras_future_pctChange) <- names(list_ras_future)

# bin probabilities - 50%
dat_bins <-
  data.frame(from = c(-100, -50, -25, -10, -1, 1, 10, 25, 50, 100, 200),
             to   = c(-50, -25, -10, -1, 1, 10, 25, 50, 100, 200, Inf))
dat_bins$val = -4:6
dat_bins$label <- c('-100% to -50%',
                    '-49% to -25%',
                    '-24% to -10%',
                    '-9% to -1%',
                    'No change',
                    '1% to 10%',
                    '11% to 25%',
                    '26% to 50%',
                    '51% to 100%',
                    '101% to 200%',
                    '>200%')
dat_bins$label <-
  factor(dat_bins$label, levels = c('-100% to -50%',
                                    '-49% to -25%',
                                    '-24% to -10%',
                                    '-9% to -1%',
                                    'No change',
                                    '1% to 10%',
                                    '11% to 25%',
                                    '26% to 50%',
                                    '51% to 100%',
                                    '101% to 200%',
                                    '>200%'))
list_ras_future_pctChange_binned <-
  lapply(list_ras_future_pctChange, function(r){
    classify(r, rcl = as.matrix(dat_bins[1:3]), include.lowest = TRUE)
  })
names(list_ras_future_pctChange_binned) <- names(list_ras_future_pctChange)




##### create island-level plots #####

# define island extents
vec_extents <- list(
  ext(-156.2, -154.8, 18.8, 20.28),
  ext(-158.4, -157.5,  21.2, 21.8),
  ext(-159.9, -159.2 , 21.8, 22.3),
  ext(-157.5, -155.9, 20.45, 21.3)
)
names(vec_extents) <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')
vec_scenarios <- c('Dyn end of cent', 'Stat mid-cent', 'Stat end of cent')

# crop each scenario with each island
list_ras_future_pctChange_binned_byIsland <- list()
for(i in 1:length(vec_extents)){
  list_ras_future_pctChange_binned_byIsland[[i]] <- list()
  for(s in 1:length(list_ras_future_pctChange_binned)){
    
    list_ras_future_pctChange_binned_byIsland[[i]][[s]] <-
      crop(list_ras_future_pctChange_binned[[s]], vec_extents[[i]])
    gc()
  }
}

# name each raster according to island and scenario
for(i in 1:length(list_ras_future_pctChange_binned_byIsland)){
  names(list_ras_future_pctChange_binned_byIsland)[[i]] <- names(vec_extents)[[i]]
  for(s in 1:length(list_ras_future_pctChange_binned_byIsland[[i]]))
    names(list_ras_future_pctChange_binned_byIsland[[i]])[[s]] <- names(list_ras_future_pctChange_binned)[[s]]
}
rm(i, s, list_ras_future, list_ras_future_pctChange)




##### format data for plot #####

# upscale to make plotting faster
list_ras_future_pctChange_binned_byIsland_upscaled <- list()
for(i in 1:length(list_ras_future_pctChange_binned_byIsland)){
  list_ras_future_pctChange_binned_byIsland_upscaled[[i]] <- list()
  for(s in 1:length(list_ras_future_pctChange_binned_byIsland[[i]])){
    list_ras_future_pctChange_binned_byIsland_upscaled[[i]][[s]] <-
      terra::aggregate(list_ras_future_pctChange_binned_byIsland[[i]][[s]],
                       factor = 4, fun = 'modal', cores = 6)
  }
}

# name each raster according to island and scenario
for(i in 1:length(list_ras_future_pctChange_binned_byIsland_upscaled)){
  names(list_ras_future_pctChange_binned_byIsland_upscaled)[[i]] <- names(vec_extents)[[i]]
  for(s in 1:length(list_ras_future_pctChange_binned_byIsland_upscaled[[i]]))
    names(list_ras_future_pctChange_binned_byIsland_upscaled[[i]])[[s]] <- names(list_ras_future_pctChange_binned)[[s]]
}
rm(i, s)

# convert to data.frame
list_dat_future_pctChange_binned_byIsland_upscaled <- list()
for(i in 1:length(list_ras_future_pctChange_binned_byIsland)){
  list_dat_future_pctChange_binned_byIsland_upscaled[[i]] <- list()
  for(s in 1:length(list_ras_future_pctChange_binned_byIsland[[i]])){
    list_dat_future_pctChange_binned_byIsland_upscaled[[i]][[s]] <-
      as.data.frame(list_ras_future_pctChange_binned_byIsland[[i]][[s]], xy = TRUE)
  }
}
gc()

# add names to data.frames
for(i in 1:length(list_dat_future_pctChange_binned_byIsland_upscaled)){
  names(list_dat_future_pctChange_binned_byIsland_upscaled)[[i]] <- names(vec_extents)[[i]]
  for(s in 1:length(list_dat_future_pctChange_binned_byIsland_upscaled[[i]])){
    names(list_dat_future_pctChange_binned_byIsland_upscaled[[i]])[[s]] <- names(list_ras_future_pctChange_binned)[[s]]
    list_dat_future_pctChange_binned_byIsland_upscaled[[i]][[s]]$island <- names(vec_extents)[[i]]
    list_dat_future_pctChange_binned_byIsland_upscaled[[i]][[s]]$scenario <- vec_scenarios[[s]]
    colnames(list_dat_future_pctChange_binned_byIsland_upscaled[[i]][[s]])[[3]] <- 'val'
  }
}
rm(i, s)

# row bind
dat_pctChange <- unlist(list_dat_future_pctChange_binned_byIsland_upscaled, recursive = FALSE)
dat_pctChange <- do.call(rbind, dat_pctChange)
gc()

# add pct change labels
dat_pctChange <- left_join(dat_pctChange, dat_bins[c('val', 'label')])
gc()

# set scenario order
dat_pctChange$scenario <-
  factor(dat_pctChange$scenario,
         levels = c('Stat mid-cent', 'Stat end of cent', 'Dyn end of cent'))




##### create plot - present-day risk #####

# aggregate for easier plotting
ras_current_upscaled <- aggregate(ras_current, factor = 4, method = 'max')

# bin fire risks
dat_bins_currentRisk <-
  data.frame(from = c(0, 0.01, 0.05, 0.10, 0.20),
             to = c(0.01, 0.05, 0.10, 0.20, Inf))
dat_bins_currentRisk$val <- 1:nrow(dat_bins_currentRisk)
dat_bins_currentRisk$label <- c('0.00 to 0.01',
                                '0.02 to 0.05',
                                '0.06 to 0.10',
                                '0.11 to 0.20',
                                '>0.20')
dat_bins_currentRisk$label <- factor(dat_bins_currentRisk$label,
                                     levels = c('0.00 to 0.01',
                                                '0.02 to 0.05',
                                                '0.06 to 0.10',
                                                '0.11 to 0.20',
                                                '>0.20'))
ras_current_upscaled <-
  classify(ras_current_upscaled, rcl = as.matrix(dat_bins_currentRisk[1:3]), include.lowest = TRUE)

# convert to data.frame
dat_current <- as.data.frame(ras_current_upscaled, xy = TRUE)
colnames(dat_current) <- c('x', 'y', 'val')
dat_current <- left_join(dat_current, dat_bins_currentRisk)
gc()




##### create current climate plot #####

ggplot(data = dat_current,
       aes(x = x, y = y, fill = label)) +
  geom_raster(show.legend = TRUE) +
  coord_equal() +
  scale_fill_manual(values = c('lightgray', viridis(nrow(dat_bins_currentRisk)-1)),
                    drop = FALSE) +
  labs(fill = 'Fire risk') +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        panel.background = element_blank(), text = element_text(size = 20))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/14e pct change in risk from climate change/',
                         'current risk statewide.pdf'),
       dpi = 300, height = 5, width = 10, device = 'pdf')
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/14e pct change in risk from climate change/',
                         'current risk statewide.png'),
       dpi = 300, height = 5, width = 10, device = 'png')
gc()




##### create climate change plot - panel #####

# print plots
for(i in 1:length(vec_extents)){
  ggplot(data = dat_pctChange[dat_pctChange$island == names(vec_extents)[[i]],],
         aes(x = x, y = y, fill = label)) +
    geom_raster(show.legend = TRUE) +
    coord_equal() +
    scale_fill_manual(values = c(viridis(8)[1:4], 'lightgray', viridis(13)[8:13]),
                      drop = FALSE) +
    facet_grid(rows = vars(island), cols = vars(scenario)) +
    labs(fill = 'Change in\nfire risk') +
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          panel.background = element_blank(), text = element_text(size = 20))
  
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/14e pct change in risk from climate change/',
                           'pct change ', names(vec_extents)[[i]], '.pdf'),
         dpi = 300, height = 5, width = 10, device = 'pdf')
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/14e pct change in risk from climate change/',
                           'pct change ', names(vec_extents)[[i]], '.png'),
         dpi = 300, height = 5, width = 10, device = 'png')
}
