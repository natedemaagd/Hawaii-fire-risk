
# this script takes the frequency rasters made in 13d and plots them

library(raster); library(ggplot2); library(viridis); library(stringr)
library(grid); library(gridExtra); library(cowplot)

# list frequency rasters
list_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13d fire risk threshold frequency rasters',
                             pattern = '.tif')

# get related info from each file: island name and risk level
vec_island <- sapply(str_split(list_filenames,'_'), function(x) x[[2]])
vec_island <- substr(vec_island, 1, nchar(vec_island)-4)
vec_island[vec_island == 'MauiCounty'] <- 'Maui County'
vec_risk   <- sub("Fire.*", "", list_filenames)
vec_risk   <- R.utils::capitalize(vec_risk)

# load frequency rasters
list_freqRasters <- lapply(list_filenames, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13d fire risk threshold frequency rasters/', r)))

# melt rasters into single data.frame
dat_freq <- lapply(list_freqRasters, function(r) as.data.frame(r, xy = TRUE))
dat_freq <- lapply(dat_freq, function(df) df[!is.na(df[,3]),])
gc()
dat_freq <- lapply(dat_freq, function(df) {colnames(df) <- c('x', 'y', 'freq'); df})
for(i in 1:length(dat_freq)){
  dat_freq[[i]]$island <- vec_island[[i]]
  dat_freq[[i]]$risk   <- vec_risk[[i]]
}
dat_freq <- do.call(rbind, dat_freq)

# data reformatting
dat_freq$risk <- with(dat_freq, ifelse(risk == 'Low', 'Moderate',
                                ifelse(risk == 'Moderate', 'High',
                                ifelse(risk == 'High', 'Very high', NA)))   # adjust risk level labels to get rid of "Low"
                      )
dat_freq$risk <- factor(dat_freq$risk, levels = c('Moderate', 'High', 'Very high'))
dat_freq$freq <- dat_freq$freq/(12*length(1999:2016))  # divide total dum of dummy values by number of months in historical data (12 months * 18 years, 1999-2016)




##### plot - each island individually #####

# make plot for each island
islands <- c('Hawaii', 'Kauai', 'Maui County', 'Oahu')
plot_list <- list()
for(i in 1:length(islands)){
  
  # subset data for island i
  plotdat <- dat_freq[dat_freq$island == islands[[i]],]
  
  # plot melted data for island i
  p1 <- ggplot() +
    geom_raster(data = plotdat[plotdat$freq > 0,], aes(x = x, y = y, fill = freq)) +
    facet_wrap(vars(risk), ncol = length(unique(plotdat$risk))) +
    scale_fill_viridis(limits = c(0,1), breaks = seq(0,1,0.2))
  
  plot_list[[i]] <- p1 +
    geom_raster(data = plotdat[plotdat$freq == 0 & !(is.na(plotdat$freq)),], aes(x = x, y = y), fill = 'lightgray') +
    labs(x = NULL, y = NULL, fill = 'Frequency') +
    coord_equal() +
    theme(text = element_text(size = 15), panel.background = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(), legend.position = 'right')
  
  ggsave(p2, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Risk frequency plots/freqPlot_',
                               islands[[i]], '.png'),
         dpi = 300, width = 8, height = 3)
  
  rm(p1)
  gc()
}
names(plot_list) <- islands




##### save plots without legends #####

# extract legend from one plot to  save separately
plotLegend <- get_legend(plot_list[[1]])
ggsave(plotLegend, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Risk frequency plots/plotLegend.png',
       dpi = 300, height = 3, width = 1)
gc()

# save figures again, this time without their legends
for(i in 1:length(islands)){
  ggsave(plot_list[[i]] + theme(legend.position = 'none'),
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Risk frequency plots/freqPlot_',
                           islands[[i]], '_noLegend.png'),
         dpi = 300, width = 8, height = 3)
}
gc()



