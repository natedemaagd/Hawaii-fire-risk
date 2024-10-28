library(rgdal); library(raster); library(stargazer); library(ggplot2)

# load ag land shapefiles
shp_ag1980 <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/ag land shapefiles/1980Ag', layer = 'alum')
shp_ag2015 <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/ag land shapefiles/2015AgBaseline', layer = '2015AgBaseline')

# load fires shapefile
shp_fires <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters', layer = '2019_1999_Hawaii_Fire_Perimeters')

# load woodcover rasters
ras_woodcov <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split', pattern = 'wood')
ras_names <- substr(ras_woodcov, 11, 19)
ras_woodcov <- lapply(ras_woodcov, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/', r)))
names(ras_woodcov) <- ras_names; rm(ras_names)

# re-project shapefiles
shp_ag1980 <- spTransform(shp_ag1980, crs(ras_woodcov$wood_1999))
shp_ag2015 <- spTransform(shp_ag2015, crs(ras_woodcov$wood_1999))
shp_fires  <- spTransform(shp_fires,  crs(ras_woodcov$wood_1999))

# rasterize ag land and fire shapefiles
ras_ag1980 <- rasterize(shp_ag1980, ras_woodcov[[1]])
ras_ag2015 <- rasterize(shp_ag2015, ras_woodcov[[1]])
ras_fires  <- rasterize(shp_fires,  ras_woodcov[[1]])

# replace raster values so they're dummies
ras_ag1980[!is.na(ras_ag1980)] <- 1 
ras_ag2015[!is.na(ras_ag2015)] <- 1
ras_fires[!is.na(ras_fires)] <- 1

ras_ag1980[is.na(ras_ag1980)] <- 0
ras_ag1980[is.na(ras_woodcov[[1]])] <- NA
ras_ag2015[is.na(ras_ag2015)] <- 0
ras_ag2015[is.na(ras_woodcov[[1]])] <- NA
ras_fires[is.na(ras_fires)] <- 0
ras_fires[is.na(ras_woodcov[[1]])] <- NA

# find pixels that were ag land in 1980 but not in 2015
ras_agChange <- ras_ag2015 - ras_ag1980   # -1 = ag land was lost, 0 = no change, 1 = ag land gained

# # create new raster that categorizes all combinations of ag and fire
# ras_categories <- ras_ag1980
# ras_categories[!is.na(ras_categories)]                             <- 0  # Other
# ras_categories[ras_fires == 0 & ras_ag2015   ==  1]                <- 1  # No fire, current ag land
# ras_categories[ras_fires == 0 & ras_agChange == -1]                <- 2  # No fire, abandoned ag land
# ras_categories[ras_fires == 0 & ras_ag1980 == 0 & ras_ag2015 == 0] <- 3  # No fire, never ag
# ras_categories[ras_fires == 1 & ras_ag2015   ==  1]                <- 4  # Fire,    current ag land
# ras_categories[ras_fires == 1 & ras_agChange == -1]                <- 5  # Fire,    abandoned ag land
# ras_categories[ras_fires == 1 & ras_ag1980 == 0 & ras_ag2015 == 0] <- 6  # Fire,    never ag
# writeRaster(ras_categories, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_ag_and_fire/ag_and_fire_categories.tif', overwrite = TRUE)
# table(values(ras_categories))
ras_categories <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/rasters_ag_and_fire/ag_and_fire_categories.tif')

# find change in woodcover from 1999 to 2016
ras_woodChange <- ras_woodcov[[length(ras_woodcov)]] - ras_woodcov[[1]]




##### any relationship b/w reforestation and rainfall? #####

# load rainfall raster and resmaple to match ag raster
ras_rainfallIn <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")
ras_rainfallIn <- resample(ras_rainfallIn, ras_woodChange)
gc()

# # create dataframe of values
# dat_rainfallReforestation <- data.frame(rain_annIn = values(ras_rainfallIn)[!is.na(values(ras_rainfallIn))],
#                                         woodChange = values(ras_woodChange)[!is.na(values(ras_rainfallIn))], # woodcov has missing values in mountains, so use rainfall to remove water pixels coded as NA
#                                         agCategory = values(ras_categories)[!is.na(values(ras_rainfallIn))])
# gc()
# 
# # regress woodChange onto rainfall
# lm1                  <- lm(data = dat_rainfallReforestation,                                             formula = woodChange*100 ~ log(rain_annIn))
# lm1_noFire_currentAg <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 1,], formula = woodChange*100 ~ log(rain_annIn))
# lm1_noFire_abandonAg <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 2,], formula = woodChange*100 ~ log(rain_annIn))
# lm1_noFire_neverAg   <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 3,], formula = woodChange*100 ~ log(rain_annIn))
# lm1_fire_currentAg   <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 4,], formula = woodChange*100 ~ log(rain_annIn))
# lm1_fire_abandonAg   <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 5,], formula = woodChange*100 ~ log(rain_annIn))
# lm1_fire_neverAg     <- lm(data = dat_rainfallReforestation[dat_rainfallReforestation$agCategory == 6,], formula = woodChange*100 ~ log(rain_annIn))
# gc()
# 
# stargazer(lm1,lm1_noFire_currentAg, lm1_noFire_abandonAg, lm1_noFire_neverAg, lm1_fire_currentAg, lm1_fire_abandonAg, lm1_fire_neverAg,
#           covariate.labels = c('Log mean ann. rain', 'Constant'),
#           dep.var.labels = 'Change in woody cover (\\%)',
#           add.lines = list(c('Land use',     'All data', 'Current ag', 'Abandoned ag', 'Never ag', 'Current ag',    'Abandoned ag',  'Never ag'),
#                            c('Fire history', 'All data', 'No',         'No',           'No',       '\\textbf{Yes}', '\\textbf{Yes}', '\\textbf{Yes}')))




##### dominant grassland trajectory #####

# load herb cover rasters
ras_herbcov <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split', pattern = 'herb')
ras_names <- substr(ras_herbcov, 11, 19)
ras_herbcov <- lapply(ras_herbcov, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/', r)))
names(ras_herbcov) <- ras_names; rm(ras_names)

# for pixels that are abandoned ag, get woodcover trajectory for pixels that are grass-dominant (>= 50%) in first year - exclude fire pixels
dat_grassDomAbanAg <- values(ras_woodChange)[values(ras_categories) == 2 & values(ras_herbcov$herb_1999) >= 0.50 & !is.na(values(ras_woodChange))]

# do the same as above, except for land that was NEVER ag
dat_grassDomNeverAg <- values(ras_woodChange)[values(ras_categories) == 3 & values(ras_herbcov$herb_1999) >= 0.50 & !is.na(values(ras_woodChange))]

# melt data then plot
dat_grassDom <- data.frame(group = c(rep('Abandoned ag', times = length(dat_grassDomAbanAg)), rep('Never ag', times = length(dat_grassDomNeverAg))),
                           value = c(dat_grassDomAbanAg, dat_grassDomNeverAg))

ggplot(data = dat_grassDom) + geom_histogram(aes(value, color = group, fill = group), alpha = 0.3) +
  labs(title = 'Change in wood cover for grass-dominated (>50%) pixels from 1999 to 2015',
       x = 'Change in wood cover (proportion)', y = 'Number of pixels') +
  scale_fill_discrete(name = NULL) + scale_color_discrete(name = NULL)
ggplot(data = dat_grassDom) + geom_density(aes(value, color = group, fill = group), alpha = 0.3) +
  labs(title = 'Change in wood cover for grass-dominated (>50%) pixels from 1999 to 2015',
       x = 'Change in wood cover (proportion)', y = 'Density') +
  scale_fill_discrete(name = NULL) + scale_color_discrete(name = NULL)
aggregate(dat_grassDom$value, list(dat_grassDom$group), summary)
