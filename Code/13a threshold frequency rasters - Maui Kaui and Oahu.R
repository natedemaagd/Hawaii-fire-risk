
###########
# IF NEED TO RUN AGAIN, REWRITE TO SAVE INDIVIDUAL RASTERS, NOT JUST STACKS, IN INTERMEDIATE/13a/13b DATA DIRECTORY
##########

# this script creates maps of each island, where the number of times the pixels
# cross the island-specific fire risk thresholds is counted in the historical data

library(raster); library(ggplot2); library(doParallel); library(gtable)
registerDoParallel(cores = 4)

# load thresholds
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata')
rm(dat_fire, dat_fire2)

# define islands
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty')

# for each island, create monthly dummy rasters saying whether the given risk threshold was surpassed
for(i in 1:length(vec_islands)){
  
  # list files for island i
  list_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
                               pattern = vec_islands[[i]])
  
  # load island i rasters
  list_historicalRasters <- lapply(list_filenames, function(r){
    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/', r))
  })
  
  # dummy rasters: did the value in each month m surpass the LOW PROBABILITY threshold?
  list_dummyRasterLowThreshold <- list()
  for(m in 1:length(list_historicalRasters)){
    list_dummyRasterLowThreshold[[m]] <- list_historicalRasters[[m]]  # copy month m raster
    values(list_dummyRasterLowThreshold[[m]]) <- ifelse(values(list_historicalRasters[[m]]) >
                                                        highRiskThresholds[highRiskThresholds$island == 'Statewide',
                                                                           'hiRiskThresh_25pctileBurned'],
                                                1, 0)
  }
  dummyRasterStack_lowFireRisk <- stack(list_dummyRasterLowThreshold)
  saveRDS(dummyRasterStack_lowFireRisk, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/dummyRasterStack_lowFireRisk_',
                                                      vec_islands[[i]], '.rds'))
  rm(list_dummyRasterLowThreshold, dummyRasterStack_lowFireRisk)
  gc()
  
  
  # dummy rasters: did the value in each month m surpass the MODERATE PROBABILITY threshold?
  list_dummyRasterModerateThreshold <- list()
  for(m in 1:length(list_historicalRasters)){
    list_dummyRasterModerateThreshold[[m]] <- list_historicalRasters[[m]]  # copy month m raster
    values(list_dummyRasterModerateThreshold[[m]]) <- ifelse(values(list_historicalRasters[[m]]) >
                                                          highRiskThresholds[highRiskThresholds$island == 'Statewide',
                                                                             'hiRiskThresh_50pctileBurned'],
                                                        1, 0)
  }
  dummyRasterStack_moderateFireRisk <- stack(list_dummyRasterModerateThreshold)
  saveRDS(dummyRasterStack_moderateFireRisk, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/dummyRasterStack_moderateFireRisk_',
                                                      vec_islands[[i]], '.rds'))
  rm(list_dummyRasterModerateThreshold, dummyRasterStack_moderateFireRisk)
  gc()
  
  # dummy rasters: did the value in each month m surpass the HIGH PROBABILITY threshold?
  list_dummyRasterHighThreshold <- list()
  for(m in 1:length(list_historicalRasters)){
    list_dummyRasterHighThreshold[[m]] <- list_historicalRasters[[m]]  # copy month m raster
    values(list_dummyRasterHighThreshold[[m]]) <- ifelse(values(list_historicalRasters[[m]]) >
                                                               highRiskThresholds[highRiskThresholds$island == 'Statewide',
                                                                                  'hiRiskThresh_75pctileBurned'],
                                                             1, 0)
  }
  dummyRasterStack_highFireRisk <- stack(list_dummyRasterHighThreshold)
  saveRDS(dummyRasterStack_highFireRisk, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/dummyRasterStack_highFireRisk_',
                                                           vec_islands[[i]], '.rds'))
  rm(list_dummyRasterHighThreshold, dummyRasterStack_highFireRisk)
  gc()
  
  print(paste0(i, ' - ', Sys.time()))
  
}
