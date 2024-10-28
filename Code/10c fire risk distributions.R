
# This script plots the distributions of fire risks within historical fire
# perimeters.

library(ggplot2); library(sf); library(raster); library(plyr); library(dplyr); library(lubridate)
library(doParallel); library(patchwork)
registerDoParallel(cores = 4)

Sys.setenv(TZ = 'HST')

# load fire data
sf_fireDat <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")




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




##### get historical fire probabilities for the fire perimeters #####

list_fire_historicalFireProbs <- list()  # initiate list to store fire probs for each month of historical data

for(f in 1:nrow(sf_fireDat)){
  
  # get fire f and initiate vector in list to save historical fire probs
  sf_fire <- sf_fireDat[f,]
  list_fire_historicalFireProbs[[f]] <- list()
  filenames_historicalProbs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical', pattern = sf_fire$island)  # list of fire prob raster names for the island where the fire occurred
  
  for(m in 1:length(filenames_historicalProbs)){
    
    # load the fire prob raster for month m
    r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                       filenames_historicalProbs[[m]]))
    
    # get probability values for pixels within the fire perimeter
    prob_vals <- mask(r, sf_fire)
    prob_vals <- prob_vals[!is.na(prob_vals)]
    
    # save values to list
    list_fire_historicalFireProbs[[f]][[m]] <- prob_vals
    gc()
    
    print(paste(f, '-', m))
  }
  
  rm(m, prob_vals, r, sf_fire, filenames_historicalProbs)
  gc()
  removeTmpFiles(h=1)
}

rm(f)

# combine list of historical probabilities into one vector
dat_fireExample_historicalFireProbs <- unlist(dat_fireExample_historicalFireProbs)



##### get fire probabilities in the year-month of the fires #####

list_fire_sameMonthFireProbs <- list()
list_fire_island <- list()
for(f in 1:nrow(sf_fireDat)){
  
  # get fire f
  sf_fire <- sf_fireDat[f,]
  
  # get year and month of the fire
  fireYear  <- substr(sf_fire$YYYYMMDD, 1, 4)
  fireMonth <- substr(sf_fire$YYYYMMDD, 5, 6)
  
  if(fireYear > 2016){
    next  # no data for fires after 2016
  }
  
  # load probability raster for that year-month
  dat_fire_sameYearMonthFireProbs <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                                          'monthly_mean_fire_prob_', sf_fire$island, '_', fireYear, fireMonth, '.tif'))
  
  # get probability values for pixels within the fire perimeter
  dat_fire_sameYearMonthFireProbs <- mask(dat_fire_sameYearMonthFireProbs, sf_fire)
  list_fire_sameMonthFireProbs[[f]] <- dat_fire_sameYearMonthFireProbs[!is.na(dat_fire_sameYearMonthFireProbs)]
  list_fire_island[[f]] <- sf_fire$island
  
  rm(sf_fire, fireYear, fireMonth, dat_fire_sameYearMonthFireProbs)
  gc()
  
  print(paste(f, 'of', nrow(sf_fireDat)))
  
}

rm(f)




##### plot fire risk distributions for each island #####

# plots of same-time fire risk within fire perimeters

# melt data
list_dat_sameMonthFireProbs <- list()
for(f in 1:length(list_fire_sameMonthFireProbs)){
  if(is.null(list_fire_sameMonthFireProbs[[f]])){
    next
  }
  list_dat_sameMonthFireProbs[[f]] <-
    data.frame(risk = list_fire_sameMonthFireProbs[[f]],
               island = list_fire_island[[f]])
}

dat_sameMonthFireProbs <- do.call(rbind, list_dat_sameMonthFireProbs)
rm(list_fire_sameMonthFireProbs, list_fire_island, f)

# load thresholds
dat_thresholds <-
  read.csv("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/10a fire risk thresholds.csv")

# plot
p1 <- ggplot() +
  geom_density(data = dat_sameMonthFireProbs[dat_sameMonthFireProbs$island == 'Hawaii',],
               aes(x = risk), color = 'black', fill = 'red', alpha = 0.5) +
  geom_vline(xintercept = unlist(c(dat_thresholds[dat_thresholds$island == 'Hawaii', 3:5])),
             linetype = 'dashed', size = 0.5) +
  labs(x = NULL, y = 'Density', title = 'Hawai\'i') +
  theme(text = element_text(size = 14))
p2 <- ggplot() +
  geom_density(data = dat_sameMonthFireProbs[dat_sameMonthFireProbs$island == 'Kauai',],
               aes(x = risk), color = 'black', fill = 'red', alpha = 0.5) +
  geom_vline(xintercept = unlist(c(dat_thresholds[dat_thresholds$island == 'Kauai', 3:5])),
             linetype = 'dashed', size = 0.5) +
  labs(x = NULL, y = NULL, title = 'Kaua\'i') +
  theme(text = element_text(size = 14))
p3 <- ggplot() +
  geom_density(data = dat_sameMonthFireProbs[dat_sameMonthFireProbs$island == 'Oahu',],
               aes(x = risk), color = 'black', fill = 'red', alpha = 0.5) +
  geom_vline(xintercept = unlist(c(dat_thresholds[dat_thresholds$island == 'Oahu', 3:5])),
             linetype = 'dashed', size = 0.5) +
  labs(x = 'Risk', y = 'Density', title = 'O\'ahu') +
  theme(text = element_text(size = 14))
p4 <- ggplot() +
  geom_density(data = dat_sameMonthFireProbs[dat_sameMonthFireProbs$island == 'MauiCounty',],
               aes(x = risk), color = 'black', fill = 'red', alpha = 0.5) +
  geom_vline(xintercept = unlist(c(dat_thresholds[dat_thresholds$island == 'MauiCounty', 3:5])),
             linetype = 'dashed', size = 0.5) +
  labs(x = 'Risk', y = NULL, title = 'Maui County') +
  theme(text = element_text(size = 14))

p <- p1 + p2 + p3 + p4

ggsave(plot = p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '10c same-month fire risk within fire perimeters with thresholds.png'),
       dpi = 300)
