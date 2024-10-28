
# This script plots mean fire risk seasonally and annually using mean climate
# and updated fuels.

library(raster)
library(ggplot2)
library(viridis)

# define islands
vec_islands <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')




##### create binned plots - mean and max annual fire risk #####

# define islands
vec_islands <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')

# list all annual fire risk rasters
list_annualMeanRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
             pattern = 'annual_mean_fire_prob_', full.names = TRUE)
list_annualMaxRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
             pattern = 'annual_max_fire_prob_', full.names = TRUE)

# load fire risk threshold cutoffs
dat_fireRiskThresholds <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds.rds'))

# create plots
for(i in 1:length(vec_islands)){
  
  # load rasters for island i: mean and max
  r_mean <- raster(list_annualMeanRisk[[i]])
  r_max <- raster(list_annualMaxRisk[[i]])
  
  # change values to bins according to table of island-specific risk
  r_mean[r_mean  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_25pctileBurned"]] <- 100
  r_mean[r_mean >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_25pctileBurned"] &
         r_mean  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_50pctileBurned"]] <- 101
  r_mean[r_mean >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_50pctileBurned"] &
         r_mean  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_75pctileBurned"]] <- 102
  r_mean[r_mean >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_75pctileBurned"] &
         r_mean  < 100]                                                                                               <- 103
  r_max[r_max  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_25pctileBurned"]] <- 100
  r_max[r_max >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_25pctileBurned"] &
        r_max  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_50pctileBurned"]] <- 101
  r_max[r_max >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_50pctileBurned"] &
        r_max  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_75pctileBurned"]] <- 102
  r_max[r_max >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], "hiRiskThresh_75pctileBurned"] &
        r_max  < 100]                                                                                               <- 103
  
  # save rasters
  writeRaster(r_mean,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Rasters/',
                                'annualMeanRisk_', vec_islands[[i]], '.tif'),
              overwrite = TRUE)
  writeRaster(r_max,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Rasters/',
                                'annualMaxRisk_', vec_islands[[i]], '.tif'),
              overwrite = TRUE)
  gc()
  
  # convert rasters to data.frame
  dat_mean <- as.data.frame(r_mean, xy = TRUE)
  colnames(dat_mean) <- c('x', 'y', 'riskBin_code')
  dat_mean <- dat_mean[!is.na(dat_mean$riskBin_code),]
  dat_max <- as.data.frame(r_max, xy = TRUE)
  colnames(dat_max) <- c('x', 'y', 'riskBin_code')
  dat_max <- dat_max[!is.na(dat_max$riskBin_code),]
  
  # create labels from bin codes
  dat_mean$riskBin <- ifelse(dat_mean$riskBin_code == 100, 'Low',
                        ifelse(dat_mean$riskBin_code == 101, 'Medium',
                               ifelse(dat_mean$riskBin_code == 102, 'High',
                                      ifelse(dat_mean$riskBin_code == 103, 'Very high', NA))))
  dat_mean$riskBin <- factor(dat_mean$riskBin,
                        levels = c('Low', 'Medium', 'High', 'Very high'))
  dat_max$riskBin <- ifelse(dat_max$riskBin_code == 100, 'Low',
                             ifelse(dat_max$riskBin_code == 101, 'Medium',
                                    ifelse(dat_max$riskBin_code == 102, 'High',
                                           ifelse(dat_max$riskBin_code == 103, 'Very high', NA))))
  dat_max$riskBin <- factor(dat_max$riskBin,
                             levels = c('Low', 'Medium', 'High', 'Very high'))
  
  # create plot
  p_mean <- ggplot(data = dat_mean, aes(x = x, y = y, fill = riskBin)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = c('lightgray', viridis(3))) +
    labs(fill = 'Fire risk') +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15))
  p_max <- ggplot(data = dat_max, aes(x = x, y = y, fill = riskBin)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = c('lightgray', viridis(3))) +
    labs(fill = 'Fire risk') +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15))
  
  # save plot
  ggsave(plot = p_mean,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Plots/meanAnnualRisk_', vec_islands[[i]], '.png'),
         height = 6, width = 10, dpi = 300)
  ggsave(plot = p_max,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Plots/maxAnnualRisk_', vec_islands[[i]], '.png'),
         height = 6, width = 10, dpi = 300)
  
  rm(dat_mean, dat_max, p_mean, p_max); gc()
  
}

rm(i, dat_fireRiskThresholds, r_max, r_mean, list_annualMaxRisk,
   list_annualMeanRisk)
gc()




##### create plots - month of max risk #####

# list month of max risk rasters
list_rasters <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters',
             pattern = 'monthofMax', full.names = TRUE)

# adjust vec_islands to be in alphabetical order (to match read-in files)
vec_islands <- vec_islands[order(vec_islands)]

for(i in 1:length(vec_islands)){
  
  # load island i month of max risk raster
  r <- raster(list_rasters[[i]])
  
  # convert to data.frame
  dat <- as.data.frame(r, xy = TRUE)
  colnames(dat) <- c('x', 'y', 'monthOfMaxRisk')
  dat <- dat[!is.na(dat$monthOfMaxRisk),]
  gc()
  
  # create month character variable
  dat$monthChar <- month.name[dat$monthOfMaxRisk]
  dat$monthChar <-
    factor(dat$monthChar, levels = month.name[1:12])
  
  # create plot
  p <- ggplot(data = dat, aes(x = x, y = y, fill = monthChar)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = pals::tol(n = 12), drop = FALSE) +
    labs(fill = 'Month of max fire risk') +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15))
  
  ggsave(plot = p,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Plots/monthOfMaxRisk_', vec_islands[[i]], '.png'),
         height = 6, width = 10, dpi = 300)
  
  rm(r, dat, p); gc()
}




##### combine month of max risk and max risk plots #####

# use alpha to plot max risk bin

for(i in 1:length(vec_islands)){
  
  # load island i rasters
  r_risk <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Rasters/',
                          'annualMaxRisk_', vec_islands[[i]], '.tif'))
  r_month <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/',
                           'annual_monthofMax_fire_prob_', vec_islands[[i]], '.tif'))
  
  # combine into data.frame
  dat <- as.data.frame(r_risk, xy = TRUE)
  colnames(dat) <- c('x', 'y', 'risk')
  dat$month <- values(r_month)
  dat <- dat[!is.na(dat$risk),]
  gc()
  
  # format variables
  dat$riskBin <- ifelse(dat$risk == 100, 'Low',
                 ifelse(dat$risk == 101, 'Medium',
                 ifelse(dat$risk == 102, 'High',
                 ifelse(dat$risk == 103, 'Very high', NA))))
  dat$riskBin <- factor(dat$riskBin,
                        levels = c('Low', 'Medium', 'High', 'Very high'))
  dat$monthChar <- month.name[dat$month]
  dat$monthChar <-
    factor(dat$monthChar, levels = month.name[1:12])
  
  # plot
  p <- ggplot(data = dat, aes(x = x, y = y, fill = monthChar, alpha = riskBin)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = pals::tol(n = 12), drop = FALSE) +
    scale_alpha_manual(values = c('Low' = 0.10, 'Medium' = 0.33, 'High' = 0.50, 'Very high' = 1.0)) +
    labs(fill = 'Month of max fire risk', alpha = 'Risk') +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15))
  
  ggsave(plot = p,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/2023 Lahaina and Kula fires/Annual/Plots/combined_maxRisk_monthOfMaxRisk_', vec_islands[[i]], '.png'),
         height = 6, width = 10, dpi = 300)
  
  gc()
}

rm(dat, r_risk, r_month, p); gc()



