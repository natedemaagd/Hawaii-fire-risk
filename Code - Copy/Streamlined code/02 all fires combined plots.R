
# This script creates the historical vs. month-of fire analysis, but for all
# fires combined.

library(sf)
library(raster)
library(ggplot2)
library(tidyverse)
library(viridis)




##### load data #####

# fire perimeters
sf_fireDat <-
  read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")




##### data formatting #####

# add island name to fire data
kmlnames <- strsplit(unique(sf_fireDat$kmlname), '_')  # get list of kmlnames, which contain county names
kmlnames <- sapply(kmlnames, function(x) x[[4]])  # keep only county identifier and collapse from list to vector
sf_fireDat$islandID <- kmlnames # use vector to create temporary island ID variable in original sf_fireDat
kmlnames <- unique(kmlnames)  # keep only unique county values
dat_island <- data.frame(islandID = kmlnames, island = c('Hawaii', 'Oahu', 'MauiCounty', 'Kauai', 'Hawaii', 'Oahu'))  # create data.frame to match islandID with standardized island names in other files
sf_fireDat <- left_join(sf_fireDat, dat_island, 'islandID')  # merge island variable to fire data
sf_fireDat$islandID <- NULL  # remove temporary islandID matching variable
sf_fireDat <- sf_fireDat %>% relocate(island, .after = Inc_name)  # move island column to be with rest of the data (instead of after the geometry column)
rm(dat_island, kmlnames)

# split by island and order according to fire size (largest to smallest)
sf_fireDat <- split(sf_fireDat, sf_fireDat$island)
sf_fireDat <-
  lapply(sf_fireDat, function(x){
    x[order(x$Sat_sz_ac, decreasing = TRUE),]
  })




##### for each island, get fire probabilities of each fire #####

# list islands
vec_islands <- c('Hawaii', 'Kauai', 'MauiCounty', 'Oahu')

# # get fire probabilities
# filenames_historicalProbs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
#                                         pattern = '.tif',  full.names = TRUE)
# dat_historicalFireProbs <- list()  # initiate list to store fire probs for each month of historical data
# dat_sameMonthFireProbs <- list()   # initiate list to store fire probs for same month of fire
# for(i in 1:length(vec_islands)){
#   dat_historicalFireProbs[[i]] <- list()
#   dat_sameMonthFireProbs[[i]] <- list()
#   for(f in 1:nrow(sf_fireDat[[i]])){
#     
#     # get fire f for island i
#     sf_fire <- sf_fireDat[[i]][f,]
#     
#     # get month and year-month of fire
#     yearmo <- substr(sf_fire$YYYYMMDD, 1, 6)
#     mo <- substr(sf_fire$YYYYMMDD, 5, 6)
#     
#     # create search string to load appropriate raster
#     val_searchString_monthOfFire <- paste0(vec_islands[[i]], '_', yearmo)
#     
#     
#     
#     ### get month-of-fire probabilities ###
#     
#     # load the fire prob raster for month of fire (skip if fire was after 2016)
#     if(as.numeric(substr(val_searchString_monthOfFire,
#                          nchar(val_searchString_monthOfFire)-5,
#                          nchar(val_searchString_monthOfFire)-2))
#        <= 2016){
#       r <- raster(filenames_historicalProbs[[grep(val_searchString_monthOfFire,
#                                                   filenames_historicalProbs)]])
#     } else {
#       next
#     }
#     
#     
#     # get probability values for pixels within the fire perimeter
#     prob_vals <- mask(r, sf_fire)
#     prob_vals <- prob_vals[!is.na(prob_vals)]
#     
#     # save same-month values to list
#     dat_sameMonthFireProbs[[i]][[f]] <- prob_vals
#     
#     rm(prob_vals, r, yearmo, val_searchString_monthOfFire)
#     gc()
#     
#     print(paste0('i = ', i, ', f = ', f, ' of ', nrow(sf_fireDat[[i]]), ' - COMPLETE'))
#     
#     
#     
#     ### get all historical fire probabilities within same boundary ###
#     
#     # list all rasters for the given month
#     r_list <- grep(paste0(mo, '.tif'), filenames_historicalProbs)
#     
#     # for each fire, get ALL probabilities in perimeter in all months
#     list_vals <- list()
#     for(r in 1:length(r_list)){
#       ras <- raster(filenames_historicalProbs[[r_list[[r]]]])
#       prob_vals <- mask(ras, sf_fire)
#       prob_vals <- prob_vals[!is.na(prob_vals)]
#       list_vals[[r]] <- prob_vals
#       rm(ras, prob_vals); gc()
#       print(r)
#     }
#     
#     # save list_vals as one vector of historical probabilities
#     dat_historicalFireProbs[[i]][[f]] <- do.call(c, list_vals)
#     
#     rm(r_list, list_vals); gc()
#   }
# }
# rm(i,f)
# 
# # remove NULL elements (fires that occurred after 2016)
# dat_historicalFireProbs <-
#   lapply(dat_historicalFireProbs,
#          function(l){ l[-which(sapply(l, is.null))] })
# dat_sameMonthFireProbs <-
#   lapply(dat_sameMonthFireProbs,
#          function(l){ l[-which(sapply(l, is.null))] })
#   
# 
# saveRDS(dat_historicalFireProbs, file = 'H:\\My Drive\\Projects\\PICASC Land-to-sea\\Data\\Intermediate\\Fire\\Streamlined code\\historical.rds')
# saveRDS(dat_sameMonthFireProbs, file = 'H:\\My Drive\\Projects\\PICASC Land-to-sea\\Data\\Intermediate\\Fire\\Streamlined code\\sameMonth.rds')

dat_historicalFireProbs <- readRDS('H:\\My Drive\\Projects\\PICASC Land-to-sea\\Data\\Intermediate\\Fire\\Streamlined code\\historical.rds')
dat_sameMonthFireProbs <- readRDS('H:\\My Drive\\Projects\\PICASC Land-to-sea\\Data\\Intermediate\\Fire\\Streamlined code\\sameMonth.rds')




##### format probabilities data #####

# convert from probability to risk bin
dat_fireRiskThresholds <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds.rds'))
dat_historicalFireProbs_binned <- list()
dat_sameMonthFireProbs_binned <- list()
for(i in 1:length(vec_islands)){
  
  # initiate list element to store values
  dat_historicalFireProbs_binned[[i]] <- list()
  dat_sameMonthFireProbs_binned[[i]] <- list()
  
  for(f in 1:length(dat_historicalFireProbs[[i]])){
    
    # sort probabilities according to risk bin
    dat_historicalFireProbs_binned[[i]][[f]] <-
      cut(dat_historicalFireProbs[[i]][[f]],
          include.lowest = TRUE,
          breaks = c(0, do.call(c, dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], 2:4]), 1),
          labels = c('Low', 'Moderate', 'High', 'Very high')
          )
    dat_sameMonthFireProbs_binned[[i]][[f]] <-
      cut(dat_sameMonthFireProbs[[i]][[f]],
          include.lowest = TRUE,
          breaks = c(0, do.call(c, dat_fireRiskThresholds[dat_fireRiskThresholds$island == vec_islands[[i]], 2:4]), 1),
          labels = c('Low', 'Moderate', 'High', 'Very high')
      )
  }
  gc()
}
rm(f, i)




##### plot - by state #####

# combine list data into data.frames: one for each island
dat_historicalFireProbs_binned <-
  lapply(dat_historicalFireProbs_binned,
         function(l){data.frame('risk' = do.call(c, l))})
dat_sameMonthFireProbs_binned <-
  lapply(dat_sameMonthFireProbs_binned,
         function(l){data.frame('risk' = do.call(c, l))})
for(i in 1:length(vec_islands)){
  dat_historicalFireProbs_binned[[i]]$island = vec_islands[[i]]
  dat_sameMonthFireProbs_binned[[i]]$island = vec_islands[[i]]
}
rm(i); gc()

# combine island-level data.frames into one data.frame
dat_historicalFireProbs_binned <- do.call(rbind, dat_historicalFireProbs_binned)
dat_sameMonthFireProbs_binned <- do.call(rbind, dat_sameMonthFireProbs_binned)
dat_historicalFireProbs_binned$data = 'Historical'
dat_sameMonthFireProbs_binned$data = 'Month of fire'
dat_binned <- rbind(dat_historicalFireProbs_binned, dat_sameMonthFireProbs_binned)
dat_binned$island[dat_binned$island == 'MauiCounty'] <- 'Maui County'

# plot 4-panel (one panel for each island) figure
ggplot(data = dat_binned) +
  geom_bar(aes(x = risk, fill = data, color = data),
           position = 'dodge', alpha = 0.5) +
  facet_wrap(facets = vars(island), nrow = 2, scales = 'free_y') +
  scale_fill_manual(values = inferno(20)[c(1,13)]) +
  scale_color_manual(values = inferno(20)[c(1,13)]) +
  labs(x = 'Fire risk', y = 'Pixels', fill = 'Data', color = 'Data') +
  theme(text = element_text(size = 15))

