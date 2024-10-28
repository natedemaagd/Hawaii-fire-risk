
# This script creates binned risk categories from the updated Aug 2023 fire risk
# rasters generated using Clay's fire model.

library(raster); library(ggplot2); library(viridis)


# set data directory
setwd('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters')

# set directory to save plots
dir_plots <- 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires'




##### load data #####

# define islands
vec_islands <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')

# load individual county-level rasters
list_files <- list.files(pattern = 'monthly_fire_prob')

# remove statewide raster
list_files <- list_files[!grepl('Statewide', list_files, fixed = TRUE)]

# load rasters
list_files <- lapply(list_files, raster)

# load fire risk threshold cutoffs
dat_fireRiskThresholds <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds.rds'))




##### create rasters of binned risk #####

# risk levels defined as:
# 100 = low
# 101 = medium
# 102 = high
# 103 = very high

list_rastersBinned <- list()
for(i in 1:length(list_files)){
  
  # change values to bins according to table of island-specific risk
  r <- list_files[[i]]
  r[r  < dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_25pctileBurned"]] <- 100
  r[r >= dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_25pctileBurned"] &
    r  < dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_50pctileBurned"]] <- 101
  r[r >= dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_50pctileBurned"] &
    r  < dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_75pctileBurned"]] <- 102
  r[r >= dat_fireRiskThresholds[dat_fireRiskThresholds == vec_islands[[i]], "hiRiskThresh_75pctileBurned"] &
    r  < 100]                                                                                               <- 103
  
  # save raster
  list_rastersBinned[[i]] <- r
  writeRaster(r,
              filename = paste0(dir_plots, '/binned risk 2023-08 raster ', vec_islands[[i]], '.tif'))
  rm(r); gc()
}

rm(i, list_files, dat_fireRiskThresholds); gc()




##### create plots of binned rasters #####

for(i in 1:length(list_rastersBinned)){
  
  # convert raster to data.frame
  dat <- as.data.frame(list_rastersBinned[[i]], xy = TRUE)
  colnames(dat) <- c('x', 'y', 'riskBin_code')
  dat <- dat[!is.na(dat$riskBin_code),]
  
  # create labels from bin codes
  dat$riskBin <- ifelse(dat$riskBin_code == 100, 'Low',
                 ifelse(dat$riskBin_code == 101, 'Medium',
                 ifelse(dat$riskBin_code == 102, 'High',
                 ifelse(dat$riskBin_code == 103, 'Very high', NA))))
  dat$riskBin <- factor(dat$riskBin,
                        levels = c('Low', 'Medium', 'High', 'Very high'))
  
  # create plot
  p <- ggplot(data = dat, aes(x = x, y = y, fill = riskBin)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = c('lightgray', viridis(3))) +
    labs(fill = 'Fire risk') +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15))
  
  # save plot
  ggsave(plot = p,
         filename = paste0(dir_plots, '/binned risk 2023-08 ', vec_islands[[i]], '.png'),
         height = 6, width = 10, dpi = 300)
  
  rm(dat, p); gc()
}

rm(i, dir_plots, list_rastersBinned, vec_islands); gc()
