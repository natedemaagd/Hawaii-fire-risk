
# this script tabulates hectares by island and risk level for mean annual climate

library(terra); library(ggplot2); library(viridis)




##### load data #####

# list rasters
list_rastNames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season',
             pattern = 'annual', full.names = TRUE)
list_rast <- lapply(list_rastNames, rast)
names(list_rast) <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')
vec_islands <- names(list_rast)

# load risk cutoffs
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
rm(dat_fire, dat_fire2); gc()




##### categorize risk #####

for(i in 1:length(list_rast)){
  
  # convert to data.frame
  dat <- as.data.frame(list_rast[[i]], xy = TRUE)
  colnames(dat) <- c('x', 'y', 'risk')
  
  # use risk cutoffs to assign a risk category
  dat$riskCategory <-
    with(dat, cut(risk,
                  breaks = c(0, highRiskThresholds[highRiskThresholds$island == 'Statewide', 2:4], 1),
                  include.lowest = TRUE))
  fireRiskLevels <- data.frame(riskCategory = levels(dat$riskCategory),
                               label = factor(c('Low', 'Moderate', 'High', 'Very high'),
                                              levels = c('Low', 'Moderate', 'High', 'Very high')))
  dat <- dplyr::left_join(dat, fireRiskLevels, 'riskCategory')
  
  ### create plot and table ###
  
  # plot
  ggplot(data = dat,
         aes(x = x, y = y, fill = label)) +
    geom_tile() +
    coord_equal() +
    scale_fill_manual(values = c('lightgray', viridis(3)), drop = FALSE) +
    theme(panel.background = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), axis.text = element_blank(),
          text = element_text(size = 15)) +
    labs(fill = 'Max future\nfire risk')
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total current area at risk/',
                           'currentMaxRisk ', vec_islands[[i]], 'annual.png'),
         dpi = 300, height = 4.5, width = 7)
  
  # tabulate hectares
  tab_maxRisk <- as.data.frame(table(dat$label))
  colnames(tab_maxRisk) <- c('Risk level', 'pixels')
  tab_maxRisk$hectares <- tab_maxRisk$pixels * 900 / 10000
  paste0('Total hectares more than \'Low\' risk ', vec_islands[[i]], ': ',
         round(sum(tab_maxRisk[tab_maxRisk$Var1 != 'Low', 'hectares'])))
  write.csv(tab_maxRisk,
            file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total current area at risk/',
                          'currentMaxRisk ', vec_islands[[i]], '.csv'),
            row.names = FALSE)
  
  gc()
}




############################ OLD CODE #################################

# ##### load data #####
# 
# # list rasters
# list_rastFiles <-
#   list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_final/18 - rasters binned',
#              pattern = 'annual', full.names = TRUE)
# list_rastFiles <- list_rastFiles[-grep('statewide', list_rastFiles)]
# 
# # load rasters
# list_rast <-
#   lapply(list_rastFiles, rast)
# names(list_rast) <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')
# 
# 
# 
# 
# ##### tabulate risk #####
# 
# for(i in 1:length(list_rast)){
#   
#   # tabulate
#   dat <- as.data.frame(table(values(list_rast[[i]])))
#   
#   # format
#   colnames(dat) <- c('risk', 'num_pixels')
#   dat$risk <- c('Low', 'Moderate', 'High', 'Very high')
#   dat$num_hectares <- dat$num_pixels * 0.09
#   
#   # save data
#   write.csv(file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total current area at risk/',
#                           'currentMaxRisk ', names(list_rast)[[i]], '.csv'),
#             dat, row.names = FALSE)
# }
