
# This code plots summary stats given a fire perimeter. Lines 46 and 49 define,
# respectively, the island and fire (according to size) to plot.

# This script reproduces (and builds on) 11_dashboardView_example.R.

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



##### choose a fire to summarize #####

# define an island: one of "Hawaii", "Kauai", "MauiCounty", "Oahu"
island <- 'Oahu'

# define which fire to plot (1 = largest fire on chosen island)
fire <- 1

# return error if island isn't set correctly
if(!island %in% c("Hawaii", "Kauai", "MauiCounty", "Oahu")){
  stop("Value for \"island\" is not valid.")
}

# return error if chosen fire index is larger than the total number of fires on the island
if(fire > nrow(sf_fireDat[[island]])){
  stop(paste0("Value for \"fire\" is greater than total number of fires on ", island))
}

# get requested fire from data
sf_fire <- sf_fireDat[[island]][fire,]









###################################################

########## raw fire probability analysis ##########

###################################################




##### get historical fire probabilities for the fire region #####

# Load historical fire probabilities for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalProbs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical', pattern = island)
dat_historicalFireProbs <- list()  # initiate list to store fire probs for each month of historical data
for(m in 190:length(filenames_historicalProbs)){
  
  # load the fire prob raster for month m
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                     filenames_historicalProbs[[m]]))
  
  # get probability values for pixels within the fire perimeter
  prob_vals <- mask(r, sf_fire)
  prob_vals <- prob_vals[!is.na(prob_vals)]
  
  # save values to list
  dat_historicalFireProbs[[m]] <- prob_vals
  gc()
}
rm(m, prob_vals, r); gc()

# combine list of historical probabilities into one vector
dat_historicalFireProbs <- unlist(dat_historicalFireProbs)
gc()




##### get fire probabilities in the year-month of the example fire #####

# get year and month of the fire
fireYear  <- substr(sf_fire$YYYYMMDD, 1, 4)
fireMonth <- substr(sf_fire$YYYYMMDD, 5, 6)

# load probability raster for that year-month
dat_sameYearMonthFireProbs <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                                        'monthly_mean_fire_prob_', island, '_', fireYear, fireMonth, '.tif'))

# get probability values for pixels within the fire perimeter
dat_sameYearMonthFireProbs <- mask(dat_sameYearMonthFireProbs, sf_fire)
dat_sameYearMonthFireProbs <- dat_sameYearMonthFireProbs[!is.na(dat_sameYearMonthFireProbs)]




##### plot - distribution of chosen fire probabilities compared to overall historical fire probabilities in that same region #####

# melt data
plotdat <-
  data.frame(value = c(dat_historicalFireProbs,
                       dat_sameYearMonthFireProbs),
             Probability = c(rep('Historical',
                                 times = length(dat_historicalFireProbs)),
                             rep('Month of fire',
                                 times = length(dat_sameYearMonthFireProbs))))

# annotation (text) data
plotdat_annotate <-
  data.frame(
    xpos = Inf, ypos =  Inf,
    annotateText = c(paste0('Historical median = ', format(round(median(dat_historicalFireProbs), 2), nsmall = 3), '\n',
                            'Month of fire median = ', format(round(median(dat_sameYearMonthFireProbs), 2), nsmall = 3))),
    hjustvar = 1,
    vjustvar = 1
  )

# plot
ggplot() +
  geom_density(data = plotdat, aes(value, color = Probability, fill = Probability),
               alpha = 0.5) +
  scale_fill_manual(values = inferno(20)[c(1,13)]) +
  scale_color_manual(values = inferno(20)[c(1,13)]) +
  labs(x = 'Pixel-level fire probability', y = 'Pixel density') +
  geom_vline(xintercept = c(median(dat_historicalFireProbs), median(dat_sameYearMonthFireProbs)),
             linetype = 'longdash', color = inferno(20)[c(1,13)]) +
  geom_text(data = plotdat_annotate,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText)) +
  theme(text = element_text(size = 14))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Dashboard figures/',
                         'fire probability comparison - ', island, ' ', str_pad(fire, 3, pad = "0"), ' ', fireYear, '-', fireMonth, '.png'),
       height = 4, width = 7, dpi = 300)

rm(plotdat, plotdat_annotate)
gc()









###################################################

################ risk bin analysis ################

###################################################




# load risk bin cutoffs
dat_fireRiskThresholds <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds.rds'))

# create bin variables from raw probability variables
dat_historicalFireProbs_binned <-
  ifelse(dat_historicalFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_25pctileBurned"],  'Low',
  ifelse(dat_historicalFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_25pctileBurned"] &
         dat_historicalFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_50pctileBurned"],  'Moderate',
  ifelse(dat_historicalFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_50pctileBurned"] &
         dat_historicalFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_75pctileBurned"],  'High',
  ifelse(dat_historicalFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_75pctileBurned"],  'Very high', NA))))

dat_sameYearMonthFireProbs_binned <-
  ifelse(dat_sameYearMonthFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_25pctileBurned"],  'Low',
  ifelse(dat_sameYearMonthFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_25pctileBurned"] &
         dat_sameYearMonthFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_50pctileBurned"],  'Moderate',
  ifelse(dat_sameYearMonthFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_50pctileBurned"] &
         dat_sameYearMonthFireProbs  < dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_75pctileBurned"],  'High',
  ifelse(dat_sameYearMonthFireProbs >= dat_fireRiskThresholds[dat_fireRiskThresholds$island == island, "hiRiskThresh_75pctileBurned"],  'Very high', NA))))

# create data.frame for plotting
plotdat <-
  data.frame(value = c(dat_historicalFireProbs_binned,
                       dat_sameYearMonthFireProbs_binned),
             label = c(rep('Historical', times = length(dat_historicalFireProbs_binned)),
                       rep('Month of fire', times = length(dat_sameYearMonthFireProbs_binned))))
plotdat <- as.data.frame(table(plotdat))
plotdat$value <-
  factor(plotdat$value, levels = c('Low', 'Moderate', 'High', 'Very high'))

# convert pixel counts to percent total historical/same-month
plotdat <- split(plotdat, plotdat$label)
plotdat <- lapply(plotdat, function(x){
  x$pctTotalLand <- x$Freq / sum(x$Freq) * 100
  x
})
plotdat <- do.call(rbind, plotdat)

# create annotation data
plotdat_annotate <-
  data.frame(
    xpos = Inf, ypos =  Inf,
    annotateText = paste0('Total fire size: ', round( sf_fire$Sat_sz_ac), ' acres'),
    hjustvar = 1,
    vjustvar = 1
  )

# create plot
ggplot() +
  geom_col(data = plotdat,
           aes(x = value, y = pctTotalLand, fill = label, color = label),
           position = 'dodge', alpha = 0.5) +
  scale_fill_manual(values = inferno(20)[c(1,13)]) +
  scale_color_manual(values = inferno(20)[c(1,13)]) +
  scale_y_continuous(name = 'Percent of burn area',
                     sec.axis = sec_axis(~ . * sf_fire$Sat_sz_ac / 100,
                                         name = 'Acres')) +
  geom_label(data = plotdat_annotate,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText),
            label.size = NA, label.r = unit(0, 'lines'), alpha = 0.6) +
  labs(x = 'Fire risk category',  fill = NULL, color = NULL) +
  theme(text = element_text(size = 14))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Dashboard figures/',
                         'fire probability comparison (binned) - ', island, ' ', str_pad(fire, 3, pad = "0"), ' ', fireYear, '-', fireMonth, '.png'),
       height = 4, width = 7, dpi = 300)









###################################################

################ rainfall analysis ################

###################################################




##### get historical rain for the fire region #####

# Load historical rainfall for the island where the fire occurred, and save probability values only for the fire region.
filenames_historicalRain <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly', pattern = sf$island)  # list of historical rainfall for the island where the fire occurred
dat_historicalRain <- list()  # initiate list to store rainfall values for each month of historical data
for(m in 1:length(filenames_historicalRain)){
  
  # load the rain raster for month m
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/',
                     filenames_historicalRain[[m]]))
  
  # get probability values for pixels within the fire perimeter
  rain_vals <- mask(r, sf)
  rain_vals <- rain_vals[!is.na(rain_vals)]
  
  # save values to list
  dat_historicalRain[[m]] <- rain_vals
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
dat_Rain_prior12months_historical <- unlist(dat_historicalRain)

# prior three months average is distribution for (fireMonth of year - 3) to (fireMonth of year - 1)
prior3months_historical <- ((as.numeric(fireMonth))-3):((as.numeric(fireMonth))-1)  # get all three months prior to month of year the fire occurred
prior3months_historical <- ifelse(prior3months_historical <= 0, prior3months_historical + 12, prior3months_historical)  # if prior months is non-positive, add 12 (recycles years of month)
dat_Rain_prior3months_historical <- dat_historicalRain[dat_historicalRain_dateReference$month %in% prior3months_historical]  # get all historical rain values for prior 3 months of year
dat_Rain_prior3months_historical <- unlist(dat_Rain_prior3months_historical)  # collapse into vector
rm(prior3months_historical)

# prior month average is distribution of (fireMonth of year - 1)
prior1month_historical <- as.numeric(fireMonth)  # get prior month of year
prior1month_historical <- ifelse(prior1month_historical <= 0, prior1month_historical + 12, prior1month_historical)  #if prior month is non-positive, add 12 (recycles years of month)
dat_Rain_prior1month_historical <- dat_historicalRain[dat_historicalRain_dateReference$month %in% prior1month_historical]  # get all historical rain values for prior 3 months of year
dat_Rain_prior1month_historical <- unlist(dat_Rain_prior1month_historical)  # collapse into vector
rm(prior1month_historical)


