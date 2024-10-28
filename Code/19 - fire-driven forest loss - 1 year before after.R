
# This script calculates the area of forest lost due to fire

library(terra); library(sf); library(ggplot2); library(tidyverse)




##### load data #####

# load fire perimeters
sf_fires <-
  read_sf(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/",
                 "2019_1999_Hawaii_Fire_Perimeters.shp"))

# get vector of counties where each fire occurred
vec_fireCounty <- sf_fires$kmlname
vec_fireCounty <- strsplit(vec_fireCounty, split = '_')
vec_fireCounty <- sapply(vec_fireCounty, function(x) x[[4]])

# load island outlines
sf_islands <-
  read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii coastlines/",
                 "coast_n83.shp"))

# load fractional land cover rasters (full stack)
rast_fullStack <- rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/Raw from Clay/",
                              "HI_FracLC_statewide_fullstack.tif"))
names(rast_fullStack)[seq(1, 65, 4)] <- c(paste0(rep("herb", 17),"_", c(seq(1999,2013,1), 2015, 2016)))
names(rast_fullStack)[seq(2, 66, 4)] <- c(paste0(rep("wood", 17),"_", c(seq(1999,2013,1), 2015, 2016)))
names(rast_fullStack)[seq(3, 67, 4)] <- c(paste0(rep("bare", 17),"_", c(seq(1999,2013,1), 2015, 2016)))
names(rast_fullStack)[seq(4, 68, 4)] <- c(paste0(rep("RMSE", 17),"_", c(seq(1999,2013,1), 2015, 2016)))

# keep only woody cover
rast_woody <- rast_fullStack[[grep("wood", names(rast_fullStack))]]
rm(rast_fullStack); gc()

# project shapefiles to match rasters
sf_islands <- st_transform(sf_islands, st_crs(sf_fires))

# monthly fire risk rasters
list_fireRisk <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
             pattern = '.tif', full.names = TRUE)




##### format island data #####

# create island ID variable in sf_islands
sf_islands$island <- factor(1:nrow(sf_islands))
sf_islands$island <-
  c('Kauai', 'Minor island 1', 'Niihau', 'Oahu', 'Minor island 2',
    'Minor island 3', 'Minor island 4', 'Molokai', 'Maui', 'Lanai',
    'Minor island 5', 'Kahoolawe', 'Big Island')

# remove minor islands
sf_islands <-
  sf_islands[sf_islands$island %in%
               c('Kauai', 'Oahu', 'Molokai', 'Maui', 'Lanai', 'Big Island'),]




##### find forest pixels lost to fire and summarize associated fire risk: loss of at least 40% woody cover #####

# initiate data.frame
df_forestLoss <- data.frame(county = vec_fireCounty,
                            pixelsLost = NA,
                            risk_25thPct = NA,
                            risk_50thPct = NA,
                            risk_75thPct = NA)
rm(vec_fireCounty)

# rename counties
df_forestLoss$county <- gsub('County', '', df_forestLoss$county)
df_forestLoss$county <- gsub('Country', '', df_forestLoss$county)
df_forestLoss$county <- gsub('Maui', 'MauiCounty', df_forestLoss$county)
df_forestLoss$county <- gsub('Honolulu', 'Oahu', df_forestLoss$county)

for(i in 1:nrow(sf_fires)){
  
  
  ### get forest pixels lost ###
  
  # get fire i
  fire_i <- sf_fires[i,]
  
  # get year of fire
  year_i <- fire_i$Year
  
  # skip if fire year is 1999 or 2016 (no prior or next year data to calculate effect)
  if(year_i <= 1999 | year_i >= 2016 | year_i %in% 2013:2015) next
  
  # get rasters year before and after fire
  ras_yearBefore <- rast_woody[paste0('wood_', year_i - 1)]
  ras_yearAfter  <- rast_woody[paste0('wood_', year_i + 1)]
  
  # get difference raster and create dummy where forest loss > 40%
  ras_diff <- ras_yearAfter - ras_yearBefore
  ras_diffDummy <- ras_diff
  ras_diffDummy[ras_diff <  -0.40 & !is.na(ras_diff)] <- 1
  ras_diffDummy[ras_diff >= -0.40 & !is.na(ras_diff)] <- 0
  gc()
  
  # mask difference dummy raster with fire shapefile
  ras_diffDummy_masked <- mask(ras_diffDummy, fire_i)
  
  # save number of forest pixels lost
  df_forestLoss$pixelsLost[[i]] <- sum(values(ras_diffDummy_masked), na.rm = TRUE)
  gc()
  
  
  ### get median fire risk of forest pixels lost ###
  
  # get correct raster for fire i and load it
  ras_fire <- list_fireRisk[grepl(df_forestLoss$county[[i]], list_fireRisk, fixed = TRUE)]
  ras_fire <- ras_fire[grepl(substr(sf_fires$YYYYMMDD[[i]], 1, 6), ras_fire, fixed = TRUE)]
  ras_fire <- rast(ras_fire)
  gc()
  
  # mask fire raster with the dummy raster (all pixels with risk) and summarize risk vals
  ras_fire <- resample(ras_fire, ras_diffDummy_masked, threads = TRUE)
  ras_fire_all <- mask(ras_fire, ras_diffDummy_masked)
  df_riskAll <- as.data.frame(ras_fire_all)
  colnames(df_riskAll) <- 'risk'
  df_riskAll <- df_riskAll[!is.na(df_riskAll$risk),]
  summary_fireRisk <- summary(df_riskAll)
  df_forestLoss[i,3:5] <- summary_fireRisk[c(2,3,5)]
  
  rm(fire_i, ras_diff, ras_diffDummy, ras_diffDummy_masked, ras_fire, ras_fire_all,
     ras_yearAfter, ras_yearBefore, df_riskAll, summary_fireRisk, year_i)
  gc()
  
  # saveRDS(df_forestLoss,
  #         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/19 loop/',
  #                       '19 - df_forestLoss.rds'))
  # write.csv(data.frame(i = i),
  #           file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/19 loop/',
  #                         '19 - df_forestLoss i tracker.csv'), row.names = FALSE)
  
  print(paste0(i, ' - ', Sys.time()))
}

rm(i)

# save progress
saveRDS(df_forestLoss,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                      '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))
df_forestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                 '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))




##### summarize fire risk of ONLY woody pixels lost to fire (i.e., not entire fire perimeter) #####

# initialize columns
df_forestLoss$risk_25thPct_forestOnly <- NA
df_forestLoss$risk_50thPct_forestOnly <- NA
df_forestLoss$risk_75thPct_forestOnly <- NA

# loop through fires and summarize fire risk only in forest pixels lost
for(i in 1:nrow(sf_fires)){
  
  # if no forest pixels were lost, skip the fire
  if(df_forestLoss$pixelsLost[[i]] == 0 | is.na(df_forestLoss$pixelsLost[[i]])) next
  
  # get fire i
  fire_i <- sf_fires[i,]
  
  # get year of fire
  year_i <- fire_i$Year
  
  # skip if fire year is 1999 or 2016 (no prior or next year data to calculate effect)
  if(year_i <= 1999 | year_i >= 2016 | year_i %in% 2013:2015) next
  
  # get rasters year before and after fire
  ras_yearBefore <- rast_woody[paste0('wood_', year_i - 1)]
  ras_yearAfter  <- rast_woody[paste0('wood_', year_i + 1)]
  
  # get difference raster and create dummy where forest loss > 40%
  ras_diff <- ras_yearAfter - ras_yearBefore
  ras_diffDummy <- ras_diff
  ras_diffDummy[ras_diff <  -0.40 & !is.na(ras_diff)] <- 1
  ras_diffDummy[ras_diff >= -0.40 & !is.na(ras_diff)] <- NA  # unlike last loop, we completely remove pixels where forest wasn't lost
  gc()
  
  # mask difference dummy raster with fire shapefile
  ras_diffDummy_masked <- mask(ras_diffDummy, fire_i)
  
  # get correct raster for fire i and load it
  ras_fire <- list_fireRisk[grepl(df_forestLoss$county[[i]], list_fireRisk, fixed = TRUE)]
  ras_fire <- ras_fire[grepl(substr(sf_fires$YYYYMMDD[[i]], 1, 6), ras_fire, fixed = TRUE)]
  ras_fire <- rast(ras_fire)
  gc()
  
  # mask fire raster with the dummy raster (all pixels with risk) and summarize risk vals
  ras_fire <- resample(ras_fire, ras_diffDummy_masked, threads = TRUE)
  ras_fire_all <- mask(ras_fire, ras_diffDummy_masked)
  df_riskAll <- as.data.frame(ras_fire_all)
  colnames(df_riskAll) <- 'risk'
  df_riskAll <- df_riskAll[!is.na(df_riskAll$risk),]
  summary_fireRisk <- summary(df_riskAll)
  df_forestLoss[i,6:8] <- summary_fireRisk[c(2,3,5)]
  
  rm(fire_i, ras_diff, ras_diffDummy, ras_diffDummy_masked, ras_fire, ras_fire_all,
     ras_yearAfter, ras_yearBefore, df_riskAll, summary_fireRisk, year_i)
  gc()
  
  print(i)
}

rm(i)

# save data
saveRDS(df_forestLoss,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                      '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))




##### summarize fire risk - forest pixels crossing the 40% threshold #####

# read data
df_forestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                 '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))

# create new column for pixels lost under new definition (crossed 40% threshold)
df_forestLoss$pixelsLost_cross40pctThreshold <- NA
for(i in 1:nrow(sf_fires)){
  
  ### get forest pixels lost ###
  
  # get fire i
  fire_i <- sf_fires[i,]
  
  # get year of fire
  year_i <- fire_i$Year
  
  # skip if fire year is 1999 or 2016 (no prior or next year data to calculate effect)
  if(year_i <= 1999 | year_i >= 2016 | year_i %in% 2013:2015) next
  
  # get rasters year before and after fire
  ras_yearBefore <- rast_woody[paste0('wood_', year_i - 1)]
  ras_yearAfter  <- rast_woody[paste0('wood_', year_i + 1)]
  
  # find pixels that went from above 40% to below 40% woody cover
  ras_beforeAbove40 <- ras_yearBefore
  ras_beforeAbove40[ras_beforeAbove40 > 0.40 & !is.na(ras_beforeAbove40)] <- 2
  ras_beforeAbove40[ras_beforeAbove40 != 2 & !is.na(ras_beforeAbove40)] <- 0
  ras_afterBelow40 <- ras_yearAfter
  ras_afterBelow40[ras_afterBelow40 < 0.40 & !is.na(ras_afterBelow40)] <- 2
  ras_afterBelow40[ras_afterBelow40 != 2 & !is.na(ras_afterBelow40)] <- 0
  ras_diff <- ras_beforeAbove40 + ras_afterBelow40
  gc()
  
  # mask difference dummy raster with fire shapefile, adjust value for easier pixel count
  ras_diffDummy_masked <- mask(ras_diffDummy, fire_i)
  ras_diffDummy_masked[ras_diffDummy_masked == 2 & !is.na(ras_diffDummy_masked)] <- 1
  
  # save number of forest pixels lost
  df_forestLoss$pixelsLost_cross40pctThreshold[[i]] <-
    sum(values(ras_diffDummy_masked), na.rm = TRUE)
  gc()
  
  print(paste0(i, '-', Sys.time()))
  
}

# save data
saveRDS(df_forestLoss,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                      '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))





##### analyze fire risk and forest loss #####

# load data
df_forestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/',
                 '19 - fire-driven forest loss and fire risk - 1 year before after.rds'))

# remove fires where no forest was lost
df_forestLoss <-
  df_forestLoss[df_forestLoss$pixelsLost > 0 & !is.na(df_forestLoss$pixelsLost),]

# plot: risk vs forest loss
ggplot(data = df_forestLoss,
       aes(x = pixelsLost, y = risk_50thPct_forestOnly)) +
  geom_point() +
  facet_wrap(vars(county), scales = 'free')

# plot: boxplots of fire risk percentiles
plotdat <-
  with(df_forestLoss,
       data.frame(value = c(risk_25thPct, risk_50thPct, risk_75thPct),
                  percentile = rep(c('25th', '50th', '75th'), each = nrow(df_forestLoss)),
                  county = county)
       )
ggplot(data = plotdat,
       aes(x = percentile, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(county), scales = 'free') +
  theme(text = element_text(size = 14))

# correlations: forest pixels lost to median forest risk
cor(df_forestLoss[df_forestLoss$county == 'Hawaii', 'pixelsLost'],
    df_forestLoss[df_forestLoss$county == 'Hawaii', 'risk_50thPct_forestOnly'],
    use = 'pairwise.complete.obs')
cor(df_forestLoss[df_forestLoss$county == 'Hawaii', 'pixelsLost'],
    df_forestLoss[df_forestLoss$county == 'Hawaii', 'risk_75thPct_forestOnly'],
    use = 'pairwise.complete.obs')

