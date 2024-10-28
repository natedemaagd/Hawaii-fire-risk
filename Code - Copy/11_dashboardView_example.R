
library(ggplot2); library(sf); library(raster); library(plyr); library(dplyr); library(lubridate)

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

# choose biggest fire as an example
sf_fireExample <- sf_fireDat[which.max(sf_fireDat$Sat_sz_ac),]




##### get historical fire probabilities for the example fire region #####

# Load historical fire probabilities for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalProbs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical', pattern = sf_fireExample$island)  # list of fire prob raster names for the island where the fire occurred
dat_fireExample_historicalFireProbs <- list()  # initiate list to store fire probs for each month of historical data
for(m in 1:length(filenames_historicalProbs)){
  
  # load the fire prob raster for month m
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                     filenames_historicalProbs[[m]]))
  
  # get probability values for pixels within the fire perimeter
  prob_vals <- mask(r, sf_fireExample)
  prob_vals <- prob_vals[!is.na(prob_vals)]
  
  # save values to list
  dat_fireExample_historicalFireProbs[[m]] <- prob_vals
  gc()
}
rm(m, prob_vals, r); gc()

# combine list of historical probabilities into one vector
dat_fireExample_historicalFireProbs <- unlist(dat_fireExample_historicalFireProbs)



##### get fire probabilities in the year-month of the example fire #####

# get year and month of the fire
fireYear  <- substr(sf_fireExample$YYYYMMDD, 1, 4)
fireMonth <- substr(sf_fireExample$YYYYMMDD, 5, 6)

# load probability raster for that year-month
dat_fireExample_sameYearMonthFireProbs <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                                        'monthly_mean_fire_prob_', sf_fireExample$island, '_', fireYear, fireMonth, '.tif'))

# get probability values for pixels within the fire perimeter
dat_fireExample_sameYearMonthFireProbs <- mask(dat_fireExample_sameYearMonthFireProbs, sf_fireExample)
dat_fireExample_sameYearMonthFireProbs <- dat_fireExample_sameYearMonthFireProbs[!is.na(dat_fireExample_sameYearMonthFireProbs)]




##### plot - distribution of example fire probabilities compared to overall historical fire probabilities in that same region #####

# melt data
plotdat <- data.frame(value = c(dat_fireExample_historicalFireProbs, dat_fireExample_sameYearMonthFireProbs),
                      Probability = c(rep('Historical', times = length(dat_fireExample_historicalFireProbs)),
                                      rep('Same-month', times = length(dat_fireExample_sameYearMonthFireProbs))))

# plot
ggplot(data = plotdat, aes(value, color = Probability, fill = Probability)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Pixel-level fire probability', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - fire probability comparison - Big Island 10-3-2005.png',
       dpi = 300, height = 4, width = 6)

rm(dat_fireExample_historicalFireProbs, dat_fireExample_sameYearMonthFireProbs, plotdat)
gc()




##### plot - fire location on current-month fire probs #####

# load probability raster for that year-month
dat_fireExample_sameYearMonthFireProbs <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                                        'monthly_mean_fire_prob_', sf_fireExample$island, '_', fireYear, fireMonth, '.tif'))

# convert raster to data.frame
dat_fireExample_sameYearMonthFireProbs <- as.data.frame(dat_fireExample_sameYearMonthFireProbs, xy = TRUE)
colnames(dat_fireExample_sameYearMonthFireProbs) <- c('x', 'y', 'value')
dat_fireExample_sameYearMonthFireProbs <- dat_fireExample_sameYearMonthFireProbs[!is.na(dat_fireExample_sameYearMonthFireProbs$value),]

# plot
ggplot() +
  geom_raster(data = dat_fireExample_sameYearMonthFireProbs, aes(x = x, y = y, fill = value)) +
  geom_sf(data = sf_fireExample, color = 'red', fill = 'transparent') +
  coord_sf() +
  theme(axis.line = element_blank(),  axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(), text = element_text(size = 12)) +
  scale_fill_viridis_c() +
  labs(fill = 'Fire probability', caption = paste0('Fire occurred ', fireMonth, '-', fireYear))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - fire location - Big Island 10-3-2005.png',
       dpi = 300, height = 4, width = 6)

rm(filenames_historicalProbs, dat_fireExample_sameYearMonthFireProbs)
gc()




##### get historical rain for the example fire region #####

# Load historical rainfall for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalRain <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly', pattern = sf_fireExample$island)  # list of historical rainfall for the island where the fire occurred
dat_fireExample_historicalRain <- list()  # initiate list to store rainfall values for each month of historical data
for(m in 1:length(filenames_historicalRain)){
  
  # load the rain raster for month m
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/',
                     filenames_historicalRain[[m]]))
  
  # get probability values for pixels within the fire perimeter
  rain_vals <- mask(r, sf_fireExample)
  rain_vals <- rain_vals[!is.na(rain_vals)]
  
  # save values to list
  dat_fireExample_historicalRain[[m]] <- rain_vals
  gc()
}
rm(m, rain_vals, r); gc()

# create reference data.frame for year and month of all historical rain data
dat_historicalRain_dateReference <- data.frame(year = substr(filenames_historicalRain, 22, 25),
                                               month = 1:12)
dat_historicalRain_dateReference$yearmonth <- as.Date(paste(dat_historicalRain_dateReference$year,
                                                            dat_historicalRain_dateReference$month,
                                                            '1', sep = '-'))

# prior 12 month historical average is just the overall distribution
dat_Rain_prior12months_historical <- unlist(dat_fireExample_historicalRain)

# prior three months average is distribution for (fireMonth of year - 3) to (fireMonth of year - 1)
prior3months_historical <- ((as.numeric(fireMonth))-3):((as.numeric(fireMonth))-1)  # get all three months prior to month of year the fire occurred
prior3months_historical <- ifelse(prior3months_historical <= 0, prior3months_historical + 12, prior3months_historical)  # if prior months is non-positive, add 12 (recycles years of month)
dat_Rain_prior3months_historical <- dat_fireExample_historicalRain[dat_historicalRain_dateReference$month %in% prior3months_historical]  # get all historical rain values for prior 3 months of year
dat_Rain_prior3months_historical <- unlist(dat_Rain_prior3months_historical)  # collapse into vector
rm(prior3months_historical)

# prior month average is distribution of (fireMonth of year - 1)
prior1month_historical <- as.numeric(fireMonth)  # get prior month of year
prior1month_historical <- ifelse(prior1month_historical <= 0, prior1month_historical + 12, prior1month_historical)  #if prior month is non-positive, add 12 (recycles years of month)
dat_Rain_prior1month_historical <- dat_fireExample_historicalRain[dat_historicalRain_dateReference$month %in% prior1month_historical]  # get all historical rain values for prior 3 months of year
dat_Rain_prior1month_historical <- unlist(dat_Rain_prior1month_historical)  # collapse into vector
rm(prior1month_historical)




##### get actual prior 1, 3, and 12 month rain for given example rain year-month #####

# prior 12 months
prior12months <- seq.Date(from = as.Date(paste0(fireYear, '-', fireMonth, '-01')) %m-% months(12),
                          to   = as.Date(paste0(fireYear, '-', fireMonth, '-01')) %m-% months(1),
                          by = 'month')  # sequence of months 12 months prior to fire
dat_Rain_prior12months <- dat_fireExample_historicalRain[dat_historicalRain_dateReference$yearmonth %in% prior12months]
dat_Rain_prior12months <- unlist(dat_Rain_prior12months)
rm(prior12months)

# prior 3 months
prior3months <- seq.Date(from = as.Date(paste0(fireYear, '-', fireMonth, '-01')) %m-% months(3),
                          to   = as.Date(paste0(fireYear, '-', fireMonth, '-01')) %m-% months(1),
                          by = 'month')  # sequence of months 12 months prior to fire
dat_Rain_prior3months <- dat_fireExample_historicalRain[dat_historicalRain_dateReference$yearmonth %in% prior3months]
dat_Rain_prior3months <- unlist(dat_Rain_prior3months)
rm(prior3months)

# prior month
prior1month <- as.Date(paste0(fireYear, '-', fireMonth, '-01')) %m-% months(1)
dat_Rain_prior1month <- dat_fireExample_historicalRain[dat_historicalRain_dateReference$yearmonth %in% prior1month]
dat_Rain_prior1month <- unlist(dat_Rain_prior1month)
rm(prior1month)




##### plot historical prior 12 month rain with fire prior 12 month rain #####

# melt data
plotdat <- data.frame(value    = c(dat_Rain_prior12months,
                                   dat_Rain_prior12months_historical),
                      Rainfall = c(rep('Prior 12 months', times = length(dat_Rain_prior12months)),
                                   rep('Historical', times = length(dat_Rain_prior12months_historical))))

# plot
ggplot(data = plotdat, aes(value, color = Rainfall, fill = Rainfall)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 12-month rainfall comparison - Big Island 10-3-2005.png',
       dpi = 300, height = 4, width = 6)

rm(dat_Rain_prior12months, dat_Rain_prior12months_historical, plotdat)
gc()




##### plot historical prior 3 month rain with fire prior 3 month rain #####

# melt data
plotdat <- data.frame(value    = c(dat_Rain_prior3months,
                                   dat_Rain_prior3months_historical),
                      Rainfall = c(rep('Prior 3 months', times = length(dat_Rain_prior3months)),
                                   rep('Historical 3 months prior', times = length(dat_Rain_prior3months_historical))))

# plot
ggplot(data = plotdat, aes(value, color = Rainfall, fill = Rainfall)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 3-month rainfall comparison - Big Island 10-3-2005.png',
       dpi = 300, height = 4, width = 6)

rm(dat_Rain_prior3months, dat_Rain_prior3months_historical, plotdat)
gc()




##### plot historical prior 1 month rain with fire prior 1 month rain #####

# melt data
plotdat <- data.frame(value    = c(dat_Rain_prior1month,
                                   dat_Rain_prior1month_historical),
                      Rainfall = c(rep('Prior 1 month', times = length(dat_Rain_prior1month)),
                                   rep('Historical 1 month prior', times = length(dat_Rain_prior1month_historical))))

# plot
ggplot(data = plotdat, aes(value, color = Rainfall, fill = Rainfall)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 1-month rainfall comparison - Big Island 10-3-2005.png',
       dpi = 300, height = 4, width = 6)

rm(dat_Rain_prior1month, dat_Rain_prior1month_historical, plotdat)
gc()


