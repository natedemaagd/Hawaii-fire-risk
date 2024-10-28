
library(ggplot2); library(sf); library(plyr); library(dplyr); library(lubridate)
library(terra); library(ggtext); library(doParallel)
registerDoParallel(cores = 6)

Sys.setenv(TZ = 'HST')

# load fire data
sf_fireDat <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/2019_1999_Hawaii_Fire_Perimeters/2022_1999_Hawaii_Large_Fire_Perimeters_UH_NREM")

# choose biggest fire as an example
sf_fireExample <- sf_fireDat[which.max(sf_fireDat$Sat_sz_ac),]

# name the fire
label_fireName <- 'Big Island 10-3-2005'




##### get historical fire probabilities for the example fire region #####

# Load historical fire probabilities for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalProbs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/prediction_rasters_MonthlyHistorical', pattern = sf_fireExample$Island)  # list of fire prob raster names for the island where the fire occurred
dat_fireExample_historicalFireProbs <- list()  # initiate list to store fire probs for each month of historical data
# dat_wholeIsland_historicalFireProbs <- list()  # SINGLE CORE
# for(m in 1:length(filenames_historicalProbs)){
#   
#   # load the fire prob raster for month m
#   r <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/prediction_rasters_MonthlyHistorical/',
#                    filenames_historicalProbs[[m]]))
#   dat_wholeIsland_historicalFireProbs[[m]] <- r
#   
#   # get probability values for pixels within the fire perimeter
#   prob_vals <- mask(r, sf_fireExample)
#   prob_vals <- prob_vals[!is.na(prob_vals)]
#   
#   # save values to list
#   dat_fireExample_historicalFireProbs[[m]] <- prob_vals
#   
#   print(paste0(m, ' of ', length(filenames_historicalProbs)))
#   gc()
# }
# rm(m, prob_vals, r); gc()
dat_historicalFireProbs <-  # MULTI-CORE
  foreach(m = 1:length(filenames_historicalProbs),
          .packages = 'terra') %dopar% {
  
  # load the fire prob raster for month m
  r <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/prediction_rasters_MonthlyHistorical/',
                   filenames_historicalProbs[[m]]))
  
  # get probability values for pixels within the fire perimeter
  prob_vals <- values(r)
  prob_vals <- prob_vals[!is.na(prob_vals)]
  prob_vals_burnPerimeter <- mask(r, sf_fireExample)
  prob_vals_burnPerimeter <- prob_vals_burnPerimeter[!is.na(prob_vals_burnPerimeter)]
  
  # save values to list
  list_probVals <- list(prob_vals, prob_vals_burnPerimeter)
  names(list_probVals) <- c('wholeIsland', 'burnPerimeter')
  gc()
  return(list_probVals)
  
}

# combine list of historical probabilities into individual vectors
dat_wholeIsland_historicalFireProbs <-
  sapply(dat_historicalFireProbs, function(x){
    x$wholeIsland
  })
dat_wholeIsland_historicalFireProbs <- c(dat_wholeIsland_historicalFireProbs); gc()
dat_fireExample_historicalFireProbs_separated <-
  sapply(dat_historicalFireProbs, function(x){
    x$burnPerimeter
  }); gc()
dat_fireExample_historicalFireProbs <- c(dat_fireExample_historicalFireProbs_separated)
dat_burnRiskQuartiles <- quantile(dat_fireExample_historicalFireProbs)

# get ALL burn area thresholds
dat_thresholds_allburns <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/10a fire risk thresholds.rds")

# assemble fire risk threshold data.frame
vec_allprobs <- dat_wholeIsland_historicalFireProbs
vec_thresholds_all <- quantile(vec_allprobs, probs = c(0.60, 0.80, 0.90))
vec_thresholds_all2 <- quantile(vec_allprobs, probs = c(0.60, 0.80, 0.90, 0.97))
vec_thresholds_fire <- unlist(dat_thresholds_allburns[dat_thresholds_allburns$island == 'Statewide', 2:4])
dat_thresholds <-
  data.frame(percentile = c(0.60, 0.80, 0.90,
                            0.25, 0.50, 0.75),
             value = c(vec_thresholds_all, vec_thresholds_fire),
             type = c('Fire weather', 'Fire weather', 'Fire weather',
                      'Burn area', 'Burn area', 'Burn area'))
rm(vec_allprobs, vec_thresholds_all, vec_thresholds_fire, dat_thresholds_allburns)
gc()



##### get fire probabilities in the year-month of the example fire #####

# get year and month of the fire
fireYear  <- substr(sf_fireExample$YYYYMMDD, 1, 4)
fireMonth <- substr(sf_fireExample$YYYYMMDD, 5, 6)

# load probability raster for that year-month
dat_fireExample_sameYearMonthFireProbs <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/prediction_rasters_MonthlyHistorical/',
                                                      'monthly_mean_fire_prob_', sf_fireExample$Island, '_', fireYear, fireMonth, '.tif'))

# get probability values for pixels within the fire perimeter
dat_fireExample_sameYearMonthFireProbs <- mask(dat_fireExample_sameYearMonthFireProbs, sf_fireExample)
dat_fireExample_sameYearMonthFireProbs <- dat_fireExample_sameYearMonthFireProbs[!is.na(dat_fireExample_sameYearMonthFireProbs)]




##### plot - distribution of example fire probabilities compared to overall historical fire probabilities in that same region #####

# load thresholds
dat_thresholds_burn <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/10a fire risk thresholds.rds")
dat_thresholds_jolly <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/10a fire risk thresholds - Jolly et al percentiles.rds")

# melt probability data
plotdat <- data.frame(value = c(dat_fireExample_historicalFireProbs, dat_fireExample_sameYearMonthFireProbs),
                      Probability = c(rep('Historical', times = length(dat_fireExample_historicalFireProbs)),
                                      rep('Same-month', times = length(dat_fireExample_sameYearMonthFireProbs))))

# melt threshold data
plotdat_thresholds <-
  data.frame(value = unlist(c(dat_thresholds_burn[dat_thresholds_burn$island == 'Statewide', 2:4],
                              dat_thresholds_jolly$vec_percentiles)),
             source = c(rep('Within-fire quartiles', times = 3),
                        rep('Jolly et al.', times = 6)))

# plot
ggplot() +
  geom_density(data = plotdat, aes(value, color = Probability, fill = Probability), alpha = 0.5) +
  geom_vline(data = plotdat_thresholds, aes(xintercept = value, linetype = source)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  # scale_x_continuous(limits = c(0, 0.25/4)) +
  labs(x = 'Pixel-level fire probability', y = 'Pixel density') +
  guides(linetype = guide_legend('Risk threshold\nsource')) +
  theme(text = element_text(size = 20))
ggsave(file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - fire probability comparison - ', label_fireName, '.png'),
       dpi = 300, height = 4, width = 6)

rm(dat_fireExample_historicalFireProbs, dat_fireExample_sameYearMonthFireProbs, plotdat, dat_thresholds_burn, dat_thresholds_jolly)
gc()




##### plot - fire location on current-month fire probs #####

# load probability raster for that year-month
dat_fireExample_sameYearMonthFireProbs <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/prediction_rasters_MonthlyHistorical/',
                                                      'monthly_mean_fire_prob_', sf_fireExample$Island, '_', fireYear, fireMonth, '.tif'))

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
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - fire location - ', label_fireName, '.png',
       dpi = 300, height = 4, width = 6)

# plot with categorical risk instead of actual risk values
dat_fireExample_sameYearMonthFireProbs$value_thresholdBurnArea <-
  cut(x = dat_fireExample_sameYearMonthFireProbs$value,
      breaks = c(0, dat_thresholds$value[dat_thresholds$type == 'Burn area'], 1),
      labels = c('[0%, 25%]', '(25%, 50%]', '(50%, 75%]', '(75%, 100%]'),
      include.lowest = TRUE)
dat_fireExample_sameYearMonthFireProbs$value_thresholdFireWeather <-
  cut(x = dat_fireExample_sameYearMonthFireProbs$value,
      breaks = c(0, vec_thresholds_all2, 1),
      labels = c('[0%, 60%]', '(60%, 80%]', '(80%, 90%]', '(90%, 97%]', '(97%, 100%]'))

ggplot(data = dat_fireExample_sameYearMonthFireProbs,
       aes(x = x, y = y, fill = value_thresholdBurnArea)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d() +
  labs(fill = 'Threshold percentile') +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 12))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '11_fire example - same month fire risk - burn risk bins - ', label_fireName, '.png'))

ggplot(data = dat_fireExample_sameYearMonthFireProbs,
       aes(x = x, y = y, fill = value_thresholdFireWeather)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d() +
  labs(fill = 'Threshold percentile') +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 12))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '11_fire example - same month fire risk - fire weather bins', label_fireName, '.png'))

# crop to burn perimeter
dat_fireExample_sameYearMonthFireProbs_cropped <- c()

rm(filenames_historicalProbs, dat_fireExample_sameYearMonthFireProbs)
gc()




##### max risk from mean climate #####

# load max risk raster for appropriate island
ras_meanClimate <-
  rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/14c median and max historical fire risk prob by island/',
              'max fire risk ', sf_fireExample$Island, '.tif'))

# convert to data.frame and categorize risk
dat_meanClimate <- as.data.frame(ras_meanClimate, na.rm = TRUE, xy = TRUE)
colnames(dat_meanClimate) <- c('x', 'y', 'maxRisk')
dat_meanClimate$maxRisk_burnAreaCuts <-
  cut(x = dat_meanClimate$maxRisk,
      breaks = c(0, dat_thresholds$value[dat_thresholds$type == 'Burn area'], 1),
      labels = c('[0%, 25%]', '(25%, 50%]', '(50%, 75%]', '(75%, 100%]'),
      include.lowest = TRUE)
dat_meanClimate$maxRisk_fireWeatherCuts <-
  cut(x = dat_meanClimate$maxRisk,
      breaks = c(0, vec_thresholds_all2, 1),
      labels = c('[0%, 60%]', '(60%, 80%]', '(80%, 90%]', '(90%, 97%]', '(97%, 100%]'))

# plot raw values
ggplot(data = dat_meanClimate,
       aes(x = x, y = y, fill = maxRisk)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(limits = c(quantile(dat_meanClimate$maxRisk)[[1]],
                                  quantile(dat_meanClimate$maxRisk)[[5]]),
                       breaks = seq(quantile(dat_meanClimate$maxRisk)[[1]],
                                    quantile(dat_meanClimate$maxRisk)[[5]],
                                    length.out = 5),
                       labels = round(seq(quantile(dat_meanClimate$maxRisk)[[1]],
                                          quantile(dat_meanClimate$maxRisk)[[5]],
                                          length.out = 5),
                                      3)) +
  labs(fill = 'Max risk under\nmean climate') +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 12))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '11_fire example - max risk under mean climate - ', label_fireName, '.png'))

# plot risk binned by fire weather
ggplot(data = dat_meanClimate,
       aes(x = x, y = y, fill = maxRisk_fireWeatherCuts)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d() +
  labs(fill = 'Max risk percentile\nunder mean climate') +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 12))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '11_fire example - max risk under mean climate - fire weather cuts - ', label_fireName, '.png'))

# plot risk binned by burn area cuts
ggplot(data = dat_meanClimate,
       aes(x = x, y = y, fill = maxRisk_burnAreaCuts)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d() +
  labs(fill = 'Max risk percentile\nunder mean climate') +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 12))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '11_fire example - max risk under mean climate - burn area cuts - ', label_fireName, '.png'))





##### get historical rain for the example fire region #####

# Load historical rainfall for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalRain <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/raster_stacks/rf_monthly', pattern = sf_fireExample$Island)  # list of historical rainfall for the island where the fire occurred
dat_fireExample_historicalRain <- list()  # initiate list to store rainfall values for each month of historical data
for(m in 1:length(filenames_historicalRain)){
  
  # load the rain raster for month m
  r <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/raster_stacks/rf_monthly/',
                   filenames_historicalRain[[m]]))
  
  # get probability values for pixels within the fire perimeter
  rain_vals <- terra::mask(r, sf_fireExample)
  rain_vals <- rain_vals[!is.na(rain_vals)]
  
  # save values to list
  dat_fireExample_historicalRain[[m]] <- rain_vals
  print(m)
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
plotdat_medians <- aggregate(plotdat$value, list(plotdat$Rainfall), median, na.rm = TRUE)
colnames(plotdat_medians) <- c('Rainfall', 'value')
ggplot() +
  geom_density(data = plotdat, aes(value, color = Rainfall, fill = Rainfall),
               alpha = 0.5) +
  geom_vline(data = plotdat_medians, aes(xintercept = value, color = Rainfall),
             size = 1.0, linetype = 'dashed') +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 12-month rainfall comparison - ', label_fireName, '.png',
       dpi = 300, height = 4, width = 6)
rm(plotdat_medians)

rm(dat_Rain_prior12months, dat_Rain_prior12months_historical, plotdat)
gc()




##### plot historical prior 3 month rain with fire prior 3 month rain #####

# melt data
plotdat <- data.frame(value    = c(dat_Rain_prior3months,
                                   dat_Rain_prior3months_historical),
                      Rainfall = c(rep('Prior 3 months', times = length(dat_Rain_prior3months)),
                                   rep('Historical 3 months prior', times = length(dat_Rain_prior3months_historical))))

# plot
plotdat_medians <- aggregate(plotdat$value, list(plotdat$Rainfall), median, na.rm = TRUE)
colnames(plotdat_medians) <- c('Rainfall', 'value')
ggplot() +
  geom_density(data = plotdat, aes(value, color = Rainfall, fill = Rainfall),
               alpha = 0.5) +
  geom_vline(data = plotdat_medians, aes(xintercept = value, color = Rainfall),
             size = 1.0, linetype = 'dashed') +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 3-month rainfall comparison - ', label_fireName, '.png',
       dpi = 300, height = 4, width = 6)

rm(dat_Rain_prior3months, dat_Rain_prior3months_historical, plotdat, plotdat_medians)
gc()




##### plot historical prior 1 month rain with fire prior 1 month rain #####

# melt data
plotdat <- data.frame(value    = c(dat_Rain_prior1month,
                                   dat_Rain_prior1month_historical),
                      Rainfall = c(rep('Prior 1 month', times = length(dat_Rain_prior1month)),
                                   rep('Historical 1 month prior', times = length(dat_Rain_prior1month_historical))))

# plot
plotdat_medians <- aggregate(plotdat$value, list(plotdat$Rainfall), median, na.rm = TRUE)
colnames(plotdat_medians) <- c('Rainfall', 'value')
ggplot() +
  geom_density(data = plotdat, aes(value, color = Rainfall, fill = Rainfall),
               alpha = 0.5) +
  geom_vline(data = plotdat_medians, aes(xintercept = value, color = Rainfall),
             size = 1.0, linetype = 'dashed') +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = 'Pixel-level rainfall (mm)', y = 'Pixel density') +
  theme(text = element_text(size = 12))
ggsave(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - 1-month rainfall comparison - ', label_fireName, '.png',
       dpi = 300, height = 4, width = 6)

rm(dat_Rain_prior1month, dat_Rain_prior1month_historical, plotdat, plotdat_medians)
gc()




##### fire probability ECDFs #####

# generate plot data
plotdat <-
  data.frame(value = c(dat_fireExample_historicalFireProbs,
                       dat_fireExample_sameYearMonthFireProbs),
             type = c(rep('Historical', times = length(dat_fireExample_historicalFireProbs)),
                      rep('Month of fire', times = length(dat_fireExample_sameYearMonthFireProbs))))

# generate line segment data
plotdat_xsegments <-
  data.frame(x = c(0, 0, 0, 0,
                   0, 0, 0),
             xend = c(quantile(dat_fireExample_historicalFireProbs,
                               probs = c(0.60, 0.80, 0.90, 0.97)),
                      quantile(dat_fireExample_sameYearMonthFireProbs,
                               probs = c(0.25, 0.50, 0.75))),
             y = c(0.60, 0.80, 0.90, 0.97,
                   0.25, 0.50, 0.75),
             yend = c(0.60, 0.80, 0.90, 0.97,
                      0.25, 0.50, 0.75),
             type = c('Historical', 'Historical', 'Historical', 'Historical',
                      'Month of fire', 'Month of fire', 'Month of fire'))
plotdat_ysegments <-
  data.frame(x = c(quantile(dat_fireExample_historicalFireProbs,
                            probs = c(0.60, 0.80, 0.90, 0.97)),
                   quantile(dat_fireExample_sameYearMonthFireProbs,
                            probs = c(0.25, 0.50, 0.75))),
             xend = c(quantile(dat_fireExample_historicalFireProbs,
                               probs = c(0.60, 0.80, 0.90, 0.97)),
                      quantile(dat_fireExample_sameYearMonthFireProbs,
                               probs = c(0.25, 0.50, 0.75))),
             y = c(0, 0, 0, 0,
                   0, 0, 0),
             yend = c(0.60, 0.80, 0.90, 0.97,
                      0.25, 0.50, 0.75),
             type = c('Historical', 'Historical', 'Historical', 'Historical',
                      'Month of fire', 'Month of fire', 'Month of fire'))
plotdat_xtext <-
  data.frame(x = c(quantile(dat_fireExample_historicalFireProbs,
                            probs = c(0.60, 0.80, 0.90, 0.97)),
                   quantile(dat_fireExample_sameYearMonthFireProbs,
                            probs = c(0.25, 0.50, 0.75))),
             y = c(0.0625, 0.0625, 0.0625, 0.0625,
                   0.0625, 0.0625, 0.0625),
             text = c(as.character(sprintf("%.3f",
                                           round(c(quantile(dat_fireExample_historicalFireProbs,
                                                            probs = c(0.60, 0.80, 0.90, 0.97)),
                                                   quantile(dat_fireExample_sameYearMonthFireProbs,
                                                            probs = c(0.25, 0.50, 0.75))),
                                                 digits = 3)))),
             type = c('Historical', 'Historical', 'Historical', 'Historical',
                      'Month of fire', 'Month of fire', 'Month of fire'))

plotdat_ytext <-
  data.frame(x = c(0.025, 0.025, 0.025, 0.025,
                   0.025, 0.025, 0.025),
             y = c(0.60, 0.80, 0.90, 0.97,
                   0.25, 0.50, 0.75),
             text = c('0.60', '0.80', '0.90', '0.97',
                      '0.25', '0.50', '0.75'),
             type = c('Historical', 'Historical', 'Historical', 'Historical',
                      'Month of fire', 'Month of fire', 'Month of fire'))

# plot
ggplot() +
  stat_ecdf(data = plotdat, aes(value), size = 1) +
  geom_segment(data = plotdat_xsegments,
               aes(x = x, y = y, yend = yend, xend = xend),
               linetype = 'dashed',
               inherit.aes = FALSE) +
  geom_segment(data = plotdat_ysegments,
               aes(x = x, y = y, yend = yend, xend = xend),
               linetype = 'dashed',
               inherit.aes = FALSE) +
  geom_richtext(data = plotdat_xtext,
                aes(x = x, y = y, label = text), angle = 90) +
  geom_richtext(data = plotdat_ytext,
                aes(x = x, y = y, label = text)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, NA)) +
  facet_grid(cols = vars(type)) +
  labs(x = 'Fire probability', y = 'Percentile') +
  theme(text = element_text(size = 12))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/11_fire example - probability ECDFs - ', label_fireName, '.png',
       dpi = 300, height = 5, width = 12)

rm(plotdat, plotdat_xsegments, plotdat_ysegments, plotdat_xtext, plotdat_ytext)
gc()




##### historical frequency maps #####

# count number of times each pixel crosses each threshold
list_threshold_frequency <- list()
for(t in 1:length(dat_thresholds$percentile)){
  
  # check each raster's pixels to see if they pass the threshold
  list_threshold_frequency_t <- list()
  for(m in 1:length(dat_wholeIsland_historicalFireProbs)){
    
    # get raster for month m
    r <- dat_wholeIsland_historicalFireProbs[[m]]
    
    # replace values to indicate if they pass threshold t
    list_threshold_frequency_t[[m]] <-
      classify(r, rcl = matrix(c(0, dat_thresholds$value[[t]], 0,
                                 dat_thresholds$value[[t]], 1, 1),
                               byrow = TRUE, ncol = 3),
               include.lowest = TRUE)
    print(paste0('-----', m, ' of ', length(dat_wholeIsland_historicalFireProbs), ' at ', Sys.time()))
  }
  gc()
  
  # sum rasters
  ras_threshold_frequency <- rast(list_threshold_frequency_t)
  rm(list_threshold_frequency_t); gc()
  ras_threshold_frequency_sum <- sum(ras_threshold_frequency)
  rm(ras_threshold_frequency); gc()
  
  # find percent of time each pixel passes the threshold
  list_threshold_frequency[[t]] <- ras_threshold_frequency_sum / length(dat_wholeIsland_historicalFireProbs)
  gc()
  
  print(paste0(t, ' of ', length(dat_thresholds$percentile), ' at ', Sys.time()))
  
}
rm(r, t, m); gc()

# plot maps
for(i in 1:length(list_threshold_frequency)){
  
  # convert raster to data.frame
  dat <- as.data.frame(list_threshold_frequency[[i]], xy = TRUE)
  colnames(dat) <- c('x', 'y', 'val')
  
  # plot
  ggplot(data = dat,
         aes(x = x, y = y, fill = val*100)) +
    scale_fill_viridis_c(limits = c(0, 100), breaks = seq(0, 100, 25), labels = seq(0, 100, 25)) +
    geom_raster() +
    coord_equal() +
    labs(fill = '% months') +
    theme(panel.background = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), axis.text = element_blank(),
          text = element_text(size = 12))
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                           '11_fire example - historical frequency - ',
                           dat_thresholds$type[[i]], ' ', dat_thresholds$percentile[[i]], ' - ', label_fireName, '.png'),
         dpi = 300)
  
}
