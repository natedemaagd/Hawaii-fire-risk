
# this script finds the max fire future risk probability for each pixel and summarizes them

library(raster); library(ggplot2); library(viridis); library(snow)

# list future fire risk rasters
vec_fileNames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future',
                            pattern = '.tif')

# define islands
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')

# define wet and dry seasons
vec_seasonDry <- c('05','06','07','08','09')
vec_seasonWet <- c('10','11','12','01','02','03','04')
vec_seasons <- c('Dry', 'Wet', 'Annual')

# define models
vec_models <- c('sta', 'dyn')
vec_rcps <- c('45', '85')

# define years
vec_years <- c('2070', '2100')

# load fire risk thresholds
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
rm(dat_fire, dat_fire2); gc()




##### max future fire probability #####

# for each season, model, year, and island, get max probability for each pixel
for(i in 1:length(vec_islands)){
  for(s in 1:length(vec_seasons)){
    for(m in 1:length(vec_models)){
      for(r in 1:length(vec_rcps)){
        for(y in 1:length(vec_years)){
          
          
          
          
          ### subset rasters ###
          
          # skip mid-century dynamical
          if(vec_models[[m]] == 'dyn' & vec_years[[y]] == '2070') next
          
          # list all rasters for island i
          vec_fileNames_i <- vec_fileNames[grep(vec_islands[[i]], vec_fileNames)]
          
          # keep only rasters with correct season
          if(s == 1){
            vec_fileNames_i <- vec_fileNames_i[substr(vec_fileNames_i, nchar(vec_fileNames_i)-5, nchar(vec_fileNames_i)-4) %in% vec_seasonDry]
          } else if(s == 2){
            vec_fileNames_i <- vec_fileNames_i[substr(vec_fileNames_i, nchar(vec_fileNames_i)-5, nchar(vec_fileNames_i)-4) %in% vec_seasonWet]
          } else {
            vec_fileNames_i <- vec_fileNames_i  # annual (wet + dry) uses all months
          }
          
          # keep only rasters with correct model
          vec_fileNames_i <- vec_fileNames_i[grep(vec_models[[m]], vec_fileNames_i)]
          vec_fileNames_i <- vec_fileNames_i[grep(vec_rcps[[r]], vec_fileNames_i)]
          
          # keep only rasters with correct year
          vec_fileNames_i <- vec_fileNames_i[grep(vec_years[[y]], vec_fileNames_i)]
          
          
          
          
          ### raster calculations ###
          
          # stack rasters
          list_rasters_i <- lapply(vec_fileNames_i, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                          r))
          })
          rStack <- stack(list_rasters_i)
          rm(list_rasters_i, vec_fileNames_i); gc()
          
          # get max pixel-wise risk probability across all future values
          beginCluster(8)
          r_maxRisk <- clusterR(rStack, calc, args = list(max))
          endCluster()
          writeRaster(r_maxRisk,
                      filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14c median and max future fire risk prob by island/',
                                        'futureMaxRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.tif'),
                      overwrite = TRUE)
          
          # convert to data.frame
          dat_maxRisk <- as.data.frame(r_maxRisk, xy = TRUE)
          colnames(dat_maxRisk) <- c('x', 'y', 'maxFireProb')
          dat_maxRisk <-  dat_maxRisk[complete.cases(dat_maxRisk),]
          gc()
          
          # add fire risk variable
          dat_maxRisk$maxFireRisk <-
            with(dat_maxRisk, cut(maxFireProb,
                                  breaks = c(0, highRiskThresholds[highRiskThresholds$island == vec_islands[[i]], 2:4], 1),
                                  include.lowest = TRUE))
          fireRiskLevels <- data.frame(maxFireRisk = levels(dat_maxRisk$maxFireRisk),
                                       label = factor(c('Low', 'Moderate', 'High', 'Very high'),
                                                      levels = c('Low', 'Moderate', 'High', 'Very high')))
          dat_maxRisk <- dplyr::left_join(dat_maxRisk, fireRiskLevels, 'maxFireRisk')
          rm(fireRiskLevels); gc()
          
          
          
          
          ### create plot and table ###
          
          # plot
          ggplot(data = dat_maxRisk,
                 aes(x = x, y = y, fill = label)) +
            geom_tile() +
            coord_equal() +
            scale_fill_manual(values = c('lightgray', viridis(3)), drop = FALSE) +
            theme(panel.background = element_blank(), axis.title = element_blank(),
                  axis.ticks = element_blank(), axis.text = element_blank(),
                  text = element_text(size = 15)) +
            labs(fill = 'Max future\nfire risk')
          ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total future area at risk/',
                                   'futureMaxRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.png'),
                 dpi = 300, height = 4.5, width = 7)
          
          # tabulate hectares
          tab_maxRisk <- as.data.frame(table(dat_maxRisk$label))
          colnames(tab_maxRisk) <- c('Risk level', 'pixels')
          tab_maxRisk$hectares <- tab_maxRisk$pixels * 900 / 10000
          paste0('Total hectares more than \'Low\' risk ', vec_islands[[i]], ': ',
                 round(sum(tab_maxRisk[tab_maxRisk$Var1 != 'Low', 'hectares'])))
          write.csv(tab_maxRisk,
                    file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total future area at risk/',
                                  'futureMaxRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.csv'),
                    row.names = FALSE)
          
          gc()
          
        }
      }
    }
  }
}




##### median future fire probability #####

# for each season, model, year, and island, get median probability for each pixel
for(i in 1:length(vec_islands)){
  for(s in 1:length(vec_seasons)){
    for(m in 1:length(vec_models)){
      for(r in 1:length(vec_rcps)){
        for(y in 1:length(vec_years)){
          
          
          
          
          ### subset rasters ###
          
          # skip mid-century dynamical
          if(vec_models[[m]] == 'dyn' & vec_years[[y]] == '2070') next
          
          # list all rasters for island i
          vec_fileNames_i <- vec_fileNames[grep(vec_islands[[i]], vec_fileNames)]
          
          # keep only rasters with correct season
          if(s == 1){
            vec_fileNames_i <- vec_fileNames_i[substr(vec_fileNames_i, nchar(vec_fileNames_i)-5, nchar(vec_fileNames_i)-4) %in% vec_seasonDry]
          } else if(s == 2){
            vec_fileNames_i <- vec_fileNames_i[substr(vec_fileNames_i, nchar(vec_fileNames_i)-5, nchar(vec_fileNames_i)-4) %in% vec_seasonWet]
          } else {
            vec_fileNames_i <- vec_fileNames_i  # annual (wet + dry) uses all months
          }
          
          # keep only rasters with correct model
          vec_fileNames_i <- vec_fileNames_i[grep(vec_models[[m]], vec_fileNames_i)]
          vec_fileNames_i <- vec_fileNames_i[grep(vec_rcps[[r]], vec_fileNames_i)]
          
          # keep only rasters with correct year
          vec_fileNames_i <- vec_fileNames_i[grep(vec_years[[y]], vec_fileNames_i)]
          
          
          
          
          ### raster calculations ###
          
          # stack rasters
          list_rasters_i <- lapply(vec_fileNames_i, function(r){
            raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/',
                          r))
          })
          rStack <- stack(list_rasters_i)
          rm(list_rasters_i, vec_fileNames_i); gc()
          
          # get median pixel-wise risk probability across all future values
          beginCluster(8)
          r_medianRisk <- clusterR(rStack, calc, args = list(median))
          endCluster()
          writeRaster(r_medianRisk,
                      filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14c median and max future fire risk prob by island/',
                                        'futureMedianRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.tif'),
                      overwrite = TRUE)
          
          # convert to data.frame
          dat_medianRisk <- as.data.frame(r_medianRisk, xy = TRUE)
          colnames(dat_medianRisk) <- c('x', 'y', 'medianFireProb')
          dat_medianRisk <-  dat_medianRisk[complete.cases(dat_medianRisk),]
          gc()
          
          # add fire risk variable
          dat_medianRisk$medianFireRisk <-
            with(dat_medianRisk, cut(medianFireProb,
                                  breaks = c(0, highRiskThresholds[highRiskThresholds$island == vec_islands[[i]], 2:4], 1),
                                  include.lowest = TRUE))
          fireRiskLevels <- data.frame(medianFireRisk = levels(dat_medianRisk$medianFireRisk),
                                       label = factor(c('Low', 'Moderate', 'High', 'Very high'),
                                                      levels = c('Low', 'Moderate', 'High', 'Very high')))
          dat_medianRisk <- dplyr::left_join(dat_medianRisk, fireRiskLevels, 'medianFireRisk')
          rm(fireRiskLevels); gc()
          
          
          
          
          ### create plot and table ###
          
          # plot
          ggplot(data = dat_medianRisk,
                 aes(x = x, y = y, fill = label)) +
            geom_tile() +
            coord_equal() +
            scale_fill_manual(values = c('lightgray', viridis(3)), drop = FALSE) +
            theme(panel.background = element_blank(), axis.title = element_blank(),
                  axis.ticks = element_blank(), axis.text = element_blank(),
                  text = element_text(size = 15)) +
            labs(fill = 'Median future\nfire risk')
          ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total future area at risk/',
                                   'futureMedianRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.png'),
                 dpi = 300, height = 4.5, width = 7)
          
          # tabulate hectares
          tab_medianRisk <- as.data.frame(table(dat_medianRisk$label))
          colnames(tab_medianRisk) <- c('Risk level', 'pixels')
          tab_medianRisk$hectares <- tab_medianRisk$pixels * 900 / 10000
          paste0('Total hectares more than \'Low\' risk ', vec_islands[[i]], ': ',
                 round(sum(tab_medianRisk[tab_medianRisk$Var1 != 'Low', 'hectares'])))
          write.csv(tab_medianRisk,
                    file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total future area at risk/',
                                  'futureMedianRisk ', vec_islands[[i]], ' ', vec_models[[m]], ' rcp', vec_rcps[[r]], ' ',  vec_years[[y]], ' ', vec_seasons[[s]], '.csv'),
                    row.names = FALSE)
          
          gc()
          
        }
      }
    }
  }
}

