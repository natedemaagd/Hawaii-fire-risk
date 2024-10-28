
# This script calculates where the fire risk within the fire perimeters was
# in August 2023 relative to historical fire risk in the same area.

library(raster)
library(sf)
library(ggplot2)




##### load data #####

# load fire perimeters and
sf_lahaina <- 
  read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/2023 Lahaina and Kula fire perimeters/Lah_fireper_20230809.shp")
sf_kula <-
  read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/2023 Lahaina and Kula fire perimeters/Kula_fire.shp")
sf_lahaina <- sf

# load new (August 2023) fire risk raster for maui county
ras_maui_202308 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/2023 Lahaina and Kula fires/Fire risk rasters/",
                "monthly_fire_prob_MauiCounty_2023_08.tif"))

# list filenames of historical fire risk rasters for maui county
list_rasFireRisk_filenames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
             pattern = 'MauiCounty', full.names = TRUE)



##### format data #####

# transform sf to match rasters
sf_lahaina <- st_transform(sf_lahaina, crs(ras_maui_202308))
sf_kula <- st_transform(sf_kula, crs(ras_maui_202308))

# for each historical month, get all probabilities within the fire perimeters
list_fireProbs_lahaina <- list()
list_fireProbs_lahainaAugust <- list()  # separate lists for just August
list_fireProbs_kula <- list()
list_fireProbs_kulaAugust <- list()
for(i in 1:length(list_rasFireRisk_filenames)){
  
  # load raster
  r <- raster(list_rasFireRisk_filenames[[i]])
  
  # extract values within fire perimeters
  r_lahaina <- mask(r, sf_lahaina)
  r_kula <- mask(r, sf_kula)
  list_fireProbs_lahaina[[i]] <- values(r_lahaina)[!is.na(values(r_lahaina))]
  list_fireProbs_kula[[i]] <- values(r_kula)[!is.na(values(r_kula))]
  if(grepl('08.tif', list_rasFireRisk_filenames[[i]], fixed = TRUE)){  # if raster is for August, save additional copy of values
    list_fireProbs_lahainaAugust[[i]] <- values(r_lahaina)[!is.na(values(r_lahaina))]
    list_fireProbs_kulaAugust[[i]] <- values(r_kula)[!is.na(values(r_kula))]
  }
  
  rm(r, r_lahaina, r_kula); gc()
  
}

rm(i)

# concatenate historical fire risk values
vec_fireRisk_lahaina <-
  unlist(list_fireProbs_lahaina)
vec_fireRisk_lahainaAugust <-
  unlist(list_fireProbs_lahainaAugust)
vec_fireRisk_kula <-
  unlist(list_fireProbs_kula)
vec_fireRisk_kulaAugust <-
  unlist(list_fireProbs_kulaAugust)
rm(list_fireProbs_kula, list_fireProbs_kulaAugust,
   list_fireProbs_lahaina, list_fireProbs_lahainaAugust,
   list_rasFireRisk_filenames)
gc()

# get fire risk values for August 2023
vec_fireRisk_lahaina_202308 <-
  values(mask(ras_maui_202308, sf_lahaina))
vec_fireRisk_lahaina_202308 <-
  vec_fireRisk_lahaina_202308[!is.na(vec_fireRisk_lahaina_202308)]
vec_fireRisk_kula_202308 <-
  values(mask(ras_maui_202308, sf_kula))
vec_fireRisk_kula_202308 <-
  vec_fireRisk_kula_202308[!is.na(vec_fireRisk_kula_202308)]
gc()




##### compare August 2023 to historical data #####

# what percentile is the median August 2023 fire risk?
val_fireRisk_medianLahainaAug2023 <- median(vec_fireRisk_lahaina_202308)
ecdf(vec_fireRisk_lahaina)(val_fireRisk_medianLahainaAug2023)
val_fireRisk_medianKulaAug2023 <- median(vec_fireRisk_kula_202308)
ecdf(vec_fireRisk_kula)(val_fireRisk_medianKulaAug2023)

# what percentile is the median August 2023 fire risk (considering historical August only)?
val_fireRisk_medianLahainaAug2023 <- median(vec_fireRisk_lahaina_202308)
ecdf(vec_fireRisk_lahainaAugust)(val_fireRisk_medianLahainaAug2023)
val_fireRisk_medianKulaAug2023 <- median(vec_fireRisk_kula_202308)
ecdf(vec_fireRisk_kulaAugust)(val_fireRisk_medianKulaAug2023)

# lahaina - August only
ggplot(data = data.frame(val = vec_fireRisk_lahainaAugust),
       aes(val)) +
  geom_histogram(bins = 300) +
  labs(x = 'Fire risk', y = 'Number of pixels',
       title = 'Lahaina - Historical August Fire Risk') +
  theme(text = element_text(size = 20))
