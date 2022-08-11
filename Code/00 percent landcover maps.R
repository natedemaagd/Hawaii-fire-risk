
# this script plots percent herbaceous, woody, and bare landcover

library(raster); library(ggplot2); library(viridis)

# load rasters
list_rasters <- list(
  bare = raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_bare_2016.tif"),
  herb = raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_herb_2016.tif"),
  wood = raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_2016.tif")
)

# plot reach raster
for(r in 1:length(list_rasters)){
  
  # convert to data.frame
  dat <- as.data.frame(list_rasters[[r]], xy = TRUE)
  gc()
  colnames(dat) <- c('x', 'y', 'val')
  gc()
  dat <- dat[!is.na(dat$val),]
  gc()
  
  # plot
  p <- ggplot(data = dat, aes(x = x, y = y, fill = val * 100)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis() +
    labs(fill = paste0('% ', names(list_rasters)[[r]])) +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 20))
  ggsave(p, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Current landcover/',
                              '2016 percent ', names(list_rasters)[[r]], '.png'),
         dpi = 300, height = 6, width = 10)
  
  rm(dat, p); gc()
}
