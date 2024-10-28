
# this script tabulates how many hectares surpass each risk threshold each month
# and sumamrizes them

library(raster); library(ggplot2); library(viridis); library(snow)
rasterOptions(chunksize = 1e+09)

# list historical fire risk rasters
vec_fileNames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                            pattern = '.tif')

# define islands
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')

# define wet and dry seasons
vec_seasonWet <- c('10','11','12','01','02','03','04')
vec_seasonDry <- c('05','06','07','08','09')
vec_seasons <- c('Dry', 'Wet')

# load fire risk thresholds
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
rm(dat_fire, dat_fire2); gc()




##### tabulate hectares at risk for each month #####

# shortcut to load completed data below

# list_tabulation <- list()
# for(i in 1:length(vec_islands)){
#   
#   # list all rasters for island i
#   vec_fileNames_i <- vec_fileNames[grep(vec_islands[[i]], vec_fileNames)]
#   
#   # stack rasters
#   list_rasters_i <- parLapply(cl = makeCluster(10),
#                               vec_fileNames_i,
#                               function(r){
#                                 raster::raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
#                                               r))
#   })
#   
#   gc()
#   
#   # count number of moderate risk pixels each month
#   mod_pixels <-
#     sapply(list_rasters_i, function(r){
#       
#       # get raster values
#       r_vals <- values(r)
#       r_vals <- r_vals[!is.na(r_vals)]
#       
#       # calculate number of pixels
#       num_pixels <- length(r_vals[r_vals >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                                                'hiRiskThresh_25pctileBurned'] &
#                                   r_vals < highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                                               'hiRiskThresh_50pctileBurned']])
#       
#       # if result is empty, set num_pixels = 0
#       if(length(num_pixels) == 0){
#         num_pixels = 0
#       }
#       
#       # return number of pixels
#       gc()
#       return(num_pixels)
#       
#       })
#   
#   # count number of high risk pixels each month
#   high_pixels <-
#     sapply(list_rasters_i, function(r){
#       
#       # get raster values
#       r_vals <- values(r)
#       r_vals <- r_vals[!is.na(r_vals)]
#       
#       # calculate number of pixels
#       num_pixels <- length(r_vals[r_vals >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                                                'hiRiskThresh_50pctileBurned'] &
#                                     r_vals < highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                                                 'hiRiskThresh_75pctileBurned']])
#       
#       # if result is empty, set num_pixels = 0
#       if(length(num_pixels) == 0){
#         num_pixels = 0
#       }
#       
#       # return number of pixels
#       gc()
#       return(num_pixels)
#       
#     })
#   
#   # count number of very high risk pixels each month
#   vhigh_pixels <-
#     sapply(list_rasters_i, function(r){
#       
#       # get raster values
#       r_vals <- values(r)
#       r_vals <- r_vals[!is.na(r_vals)]
#       
#       # calculate number of pixels
#       num_pixels <- length(r_vals[r_vals >= highRiskThresholds[highRiskThresholds$island == vec_islands[[i]],
#                                                                'hiRiskThresh_75pctileBurned']])
#       
#       # if result is empty, set num_pixels = 0
#       if(length(num_pixels) == 0){
#         num_pixels = 0
#       }
#       
#       # return number of pixels
#       gc()
#       return(num_pixels)
#       
#     })
#   
#   # compile data
#   dat_pixelTabulation <- data.frame(island = vec_islands[[i]],
#                                     yearmo = sapply(vec_fileNames_i, function(s){
#                                       substr(s, nchar(s) - 9, nchar(s) - 4)
#                                     }),
#                                     modRisk_pixels = mod_pixels,
#                                     highRisk_pixels = high_pixels,
#                                     veryHighRisk_pixels = vhigh_pixels)
#   
#   list_tabulation[[i]] <- dat_pixelTabulation
#   
#   gc()
# }
# 
# # combine into single data.frame
# dat_tabulation <- do.call(rbind, list_tabulation)
# rm(list_rasters_i, list_tabulation, i,
#    mod_pixels, high_pixels, vhigh_pixels,
#    dat_pixelTabulation)
# 
# # add variables
# dat_tabulation$modRisk_hectares <- dat_tabulation$modRisk_pixels * 0.09
# dat_tabulation$highRisk_hectares <- dat_tabulation$highRisk_pixels * 0.09
# dat_tabulation$veryHighRisk_hectares <- dat_tabulation$veryHighRisk_pixels * 0.09
# dat_tabulation$year <- as.numeric(substr(dat_tabulation$yearmo, 1, 4))
# dat_tabulation$month <- as.numeric(substr(dat_tabulation$yearmo, 5, 6))
# dat_tabulation$season <- ifelse(dat_tabulation$month %in% as.numeric(vec_seasonDry),
#                                 'Dry', 'Wet')
# rownames(dat_tabulation) <- 1:nrow(dat_tabulation)
# 
# saveRDS(dat_tabulation,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
#                       '15 monthly hectares at risk.rds'))
dat_tabulation <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                                 '15 monthly hectares at risk.rds'))




##### plots #####

# create plot-friendly data
plotdat <-
  data.frame(hectares =
               with(dat_tabulation,
                    c(modRisk_hectares, highRisk_hectares, veryHighRisk_hectares)),
             risk = rep(c('Moderate', 'High', 'Very high'),
                        each = nrow(dat_tabulation)),
             island = dat_tabulation$island,
             year = dat_tabulation$year,
             month = dat_tabulation$month,
             season = dat_tabulation$season)
plotdat$risk <- factor(plotdat$risk, levels = c('Moderate', 'High', 'Very high'))

# add "% of hectares at risk" variable to plotdat
dat_hectares <-
  data.frame(island = vec_islands,
             islandSize_hectares = c(154544, 145635, 300957, 1043200))
plotdat <- merge(plotdat, dat_hectares, by = 'island')
plotdat$pctLandArea <- plotdat$hectares / plotdat$islandSize_hectares * 100

plotdat$island[plotdat$island == 'MauiCounty'] <- 'Maui County'

# categorize by year
ggplot(data = plotdat,
       aes(pctLandArea, fill = as.character(year))) +
  geom_histogram() +
  scale_fill_viridis_d() +
  facet_grid(island ~ risk, scales = 'free') +
  labs(x = '% land area', y = 'Number of months', fill = 'Year') +
  theme(text = element_text(size = 20))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '15 distribution of land area at risk each month.png'),
       dpi = 300, height = 7, width = 8)
