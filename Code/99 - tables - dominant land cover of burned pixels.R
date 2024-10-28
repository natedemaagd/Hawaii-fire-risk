
# This script tabulates the dominant land cover (woody, berhaceous, bare) of burned pixels

library(sf); library(terra); library(ggplot2); library(tidyverse)

# set year range and "dominant" threshold
vec_years <- 1999:2016
val_dominant <- 0.40




##### load data #####

# fire shapefiles
sf_fires <- read_sf("D:/Nextcloud/LandcareLab_Cloud/FireShed/Nate full re-run/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")

# yearly land cover rasters
list_rast_herb <- list.files('D:/Nextcloud/LandcareLab_Cloud/FireShed/Nate full re-run/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split',
                             full.names = TRUE, pattern = '_herb_')
list_rast_herb_names <- list_rast_herb
list_rast_herb <- lapply(list_rast_herb, rast)
list_rast_herb_names <- strsplit(list_rast_herb_names, '_')
list_rast_herb_names <- paste0('herb', sapply(list_rast_herb_names, function(x) x[[length(x)]]))
list_rast_herb_names <- sapply(list_rast_herb_names, function(x) substr(x, 1, nchar(x) - 4))
names(list_rast_herb) <- list_rast_herb_names
list_rast_wood <- list.files('D:/Nextcloud/LandcareLab_Cloud/FireShed/Nate full re-run/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split',
                             full.names = TRUE, pattern = '_wood_')
list_rast_wood_names <- list_rast_wood
list_rast_wood <- lapply(list_rast_wood, rast)
list_rast_wood_names <- strsplit(list_rast_wood_names, '_')
list_rast_wood_names <- paste0('wood', sapply(list_rast_wood_names, function(x) x[[length(x)]]))
list_rast_wood_names <- sapply(list_rast_wood_names, function(x) substr(x, 1, nchar(x) - 4))
names(list_rast_wood) <- list_rast_wood_names
rm(list_rast_herb_names, list_rast_wood_names)
gc()

# remove rasters beyond last year of fire data (2016)
list_rast_herb <- list_rast_herb[as.numeric(substr(names(list_rast_herb),5,8)) <= max(vec_years)]
list_rast_wood <- list_rast_wood[as.numeric(substr(names(list_rast_wood),5,8)) <= max(vec_years)]

# match CRS
sf_fires <- st_transform(sf_fires, crs(list_rast_herb[[1]]))




##### create grass and wood dominant dummy rasters #####

# create reclassification matrix
mat_rcl <- matrix(c(0.00,         val_dominant, 0,
                    val_dominant, 1.00,         1),
                  ncol = 3, byrow = TRUE)

# reclassify rasters
list_rast_herb_dummy <- list()
list_rast_wood_dummy <- list()
for(y in 1:length(list_rast_herb)){
  list_rast_herb_dummy[[y]] <- classify(list_rast_herb[[y]], rcl = mat_rcl, include.lowest = TRUE)
  list_rast_wood_dummy[[y]] <- classify(list_rast_wood[[y]], rcl = mat_rcl, include.lowest = TRUE)
  print(paste0(y, ' of ', length(list_rast_herb), ' at ', Sys.time()))
  gc()
}
names(list_rast_herb_dummy) <- names(list_rast_herb)
names(list_rast_wood_dummy) <- names(list_rast_wood)
rm(y, mat_rcl)
gc()









########## V1 - 40% THRESHOLD ##########




##### count grass, wood, and wood to grass pixels for each fire #####

# run through each fire and count pixels
list_firedat <- list()
for(f in 1:nrow(sf_fires)){
  
  # get fire and its year
  sf_fire_f <- sf_fires[f,]
  sf_fire_f_year <- sf_fire_f$Year
  
  # skip if we don't have data 1 year prior or 1 year after fire, or if we need 2014 since we don't have that either
  if(sf_fire_f_year <= min(vec_years) | sf_fire_f_year >= max(vec_years)){
    
    # return NA
    list_firedat[[f]] <-
      data.frame('year' = NA,
                 'burned_herb' = NA,
                 'burned_wood' = NA,
                 'wood_to_grass' = NA
                 )
    
  } else {
    
    # get year prior and year after fire's herb and wood rasters - account for missing 2014
    if(sf_fire_f_year == 2013){
      
      ras_herb_before <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 1)][[1]]
      ras_wood_before <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 1)][[1]]
      ras_herb_after  <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year + 2)][[1]]  # "after" year will be 2015
      ras_wood_after  <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year + 2)][[1]]
      
    } else if(sf_fire_f_year == 2015){
      
      ras_herb_before <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 2)][[1]]  # "before" year will be 2013
      ras_wood_before <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 2)][[1]]
      ras_herb_after  <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year + 1)][[1]]
      ras_wood_after  <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year + 1)][[1]]
      
    } else {
      
      ras_herb_before <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 1)][[1]]
      ras_wood_before <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 1)][[1]]
      ras_herb_after  <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year + 1)][[1]]
      ras_wood_after  <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year + 1)][[1]]
    }
    
    # mask rasters to extent of fire
    ras_herb_before <- trim(mask(ras_herb_before, sf_fire_f))
    ras_wood_before <- crop(ras_wood_before, ras_herb_before)
    ras_herb_after  <- crop(ras_herb_after, ras_herb_before)
    ras_wood_after  <- crop(ras_wood_after, ras_herb_before)
    gc()
    
    # find pixels converted from wood to herb
    ras_herbToWood <- ras_herb_before
    values(ras_herbToWood) <- 0
    ras_herbToWood[ras_wood_before == 1 & ras_wood_before - ras_herb_after == 0] <- 1
    
    # tabulate
    list_firedat[[f]] <-
      data.frame('year' = sf_fire_f_year,
                 'burned_herb' = sum(values(ras_herb_before), na.rm = TRUE),
                 'burned_wood' = sum(values(ras_wood_before), na.rm = TRUE),
                 'wood_to_grass' = sum(values(ras_herbToWood),  na.rm = TRUE)
      )
    rm(ras_herb_before, ras_wood_before, ras_herb_after, ras_wood_after, ras_herbToWood)
    print(paste0(f, ' of ', nrow(sf_fires), ' at ', Sys.time()))
    gc()
    
  }
}
rm(f, sf_fire_f, sf_fire_f_year)




##### format data and plot #####

# combine into single data.frame
dat_fire <- do.call(rbind, list_firedat)
rm(list_firedat)

# convert pixels to hectares
dat_fire$burned_herb_hectares   <- dat_fire$burned_herb   * 0.09
dat_fire$burned_wood_hectares   <- dat_fire$burned_wood   * 0.09
dat_fire$wood_to_grass_hectares <- dat_fire$wood_to_grass * 0.09

# tabulate
tab_hectaresConverted <-
  as.data.frame(aggregate(x = cbind(burned_herb_hectares, burned_wood_hectares, wood_to_grass_hectares) ~ year,
                          data = dat_fire, FUN = summary))
vec_numFiresPerYear <- as.data.frame(table(dat_fire$year))
colnames(vec_numFiresPerYear) <- c('year', 'n_fires')
vec_numFiresPerYear$year <- as.numeric(as.character(vec_numFiresPerYear$year))
tab_hectaresConverted <- left_join(tab_hectaresConverted, vec_numFiresPerYear)
tab_hectaresConverted <- tab_hectaresConverted %>% relocate(n_fires, .after = year)

# create plot data
dat_plot <-
  as.data.frame(aggregate(x = cbind(burned_herb_hectares, burned_wood_hectares, wood_to_grass_hectares) ~ year,
                          data = dat_fire, FUN = sum, na.rm = TRUE))
dat_nFires <- as.data.frame(table(dat_fire$year))
colnames(dat_nFires) <- c('year', 'n_fires')
dat_nFires$year <- as.numeric(as.character(dat_nFires$year))
dat_plot <- left_join(dat_plot, dat_nFires)

# aggregate plot data for faceting
dat_plotMelt <-
  with(dat_plot,
       data.frame(
         year = year,
         val = c(burned_herb_hectares, burned_wood_hectares, wood_to_grass_hectares, n_fires),
         lab = rep(c('Grassy dominant', 'Woody dominant', 'Wood to grass', 'Number of fires'),
                   each = nrow(dat_plot))
         )
       )
dat_plotMelt$lab <-
  factor(dat_plotMelt$lab,
         levels = c('Grassy dominant', 'Woody dominant', 'Wood to grass', 'Number of fires'))

# plot - facets
ggplot(data = dat_plotMelt,
       aes(x = year, y = val)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(lab), scales = 'free_y') +
  labs(y = 'Hectares', x = NULL) +
  theme(text = element_text(size = 18))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels.png',
       dpi = 300, height = 10, width = 10)

# plot - stacked
ggplot() +
  geom_bar(data = dat_plotMelt[dat_plotMelt$lab != 'Number of fires',],
           aes(x = year, y = val, fill = lab, color = lab),
           stat = 'identity', position = position_dodge()) +
  labs(y = 'Hectares', x = NULL, fill = NULL, color = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = min(dat_plotMelt$year):max(dat_plotMelt$year),
                     labels = min(dat_plotMelt$year):max(dat_plotMelt$year)) +
  theme(text = element_text(size = 18),
        legend.position = c(0.875, 0.9),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels stacked.png',
       dpi = 300, height = 6, width = 10)




##### export data #####

write.csv(tab_hectaresConverted,
          file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels.csv',
          row.names = FALSE)









########## V2 - 40% CHANGE ##########

# run through each fire and count pixels
list_firedat <- list()
for(f in 1:nrow(sf_fires)){
  
  # get fire and its year
  sf_fire_f <- sf_fires[f,]
  sf_fire_f_year <- sf_fire_f$Year
  
  # skip if we don't have data 1 year prior or 1 year after fire, or if we need 2014 since we don't have that either
  if(sf_fire_f_year <= min(vec_years) | sf_fire_f_year >= max(vec_years)){
    
    # return NA
    list_firedat[[f]] <-
      data.frame('year' = NA,
                 'burned_herb' = NA,
                 'burned_wood' = NA,
                 'lost_woody_dominance' = NA
      )
    
  } else {
    
    # get year prior and year after fire's herb and wood rasters - account for missing 2014
    if(sf_fire_f_year == 2013){
      
      ras_wood_before <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year - 1)][[1]]
      ras_wood_after  <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year + 2)][[1]]
      ras_herb_before_dummy <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 1)][[1]]
      ras_wood_before_dummy <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 1)][[1]]
      
    } else if(sf_fire_f_year == 2015){
      
      ras_wood_before <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year - 2)][[1]]
      ras_wood_after  <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year + 1)][[1]]
      ras_herb_before_dummy <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 2)][[1]]
      ras_wood_before_dummy <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 2)][[1]]
      
    } else {
      
      ras_wood_before <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year - 1)][[1]]
      ras_wood_after  <- list_rast_wood[names(list_rast_wood) == paste0('wood', sf_fire_f_year + 1)][[1]]
      ras_herb_before_dummy <- list_rast_herb_dummy[names(list_rast_herb_dummy) == paste0('herb', sf_fire_f_year - 1)][[1]]
      ras_wood_before_dummy <- list_rast_wood_dummy[names(list_rast_wood_dummy) == paste0('wood', sf_fire_f_year - 1)][[1]]
    }
    
    # mask rasters to extent of fire
    ras_wood_before <- trim(mask(ras_wood_before, sf_fire_f))
    ras_wood_after  <- crop(ras_wood_after, ras_wood_before)
    ras_herb_before_dummy <- crop(ras_herb_before_dummy, ras_wood_before)
    ras_wood_before_dummy <- crop(ras_wood_before_dummy, ras_wood_before)
    gc()
    
    # find pixels that lost woody dominance
    ras_wood_diff <- ras_wood_after - ras_wood_before
    ras_wood_diff <-
      classify(ras_wood_diff,
               rcl = matrix(c(-1,           val_dominant, 0,
                              val_dominant, 1,            1),
                            byrow = TRUE, ncol = 3),
               include.lowest = TRUE)
    
    # tabulate
    list_firedat[[f]] <-
      data.frame('year' = sf_fire_f_year,
                 'burned_herb' = sum(values(ras_herb_before_dummy), na.rm = TRUE),
                 'burned_wood' = sum(values(ras_wood_before_dummy), na.rm = TRUE),
                 'lost_woody_dominance' = sum(values(ras_wood_diff),  na.rm = TRUE)
      )
    rm(ras_herb_before, ras_wood_before, ras_herb_after, ras_wood_after, ras_wood_diff,
       ras_herb_before_dummy, ras_wood_before_dummy, ras_herb_after_dummy, ras_wood_after_dummy)
    print(paste0(f, ' of ', nrow(sf_fires), ' at ', Sys.time()))
    gc()
    
  }
}

save.image('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - line 324.Rdata')
load('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - line 324.Rdata')




##### format data and plot #####

# combine into single data.frame
dat_fire <- do.call(rbind, list_firedat)
rm(list_firedat)

# convert pixels to hectares
dat_fire$burned_herb_hectares   <- dat_fire$burned_herb   * 0.09
dat_fire$burned_wood_hectares   <- dat_fire$burned_wood   * 0.09
dat_fire$lost_woody_dominance_hectares   <- dat_fire$lost_woody_dominance * 0.09

# tabulate
tab_hectaresConverted <-
  as.data.frame(aggregate(x = cbind(burned_herb_hectares, burned_wood_hectares, lost_woody_dominance_hectares) ~ year,
                          data = dat_fire, FUN = summary))
vec_numFiresPerYear <- as.data.frame(table(dat_fire$year))
colnames(vec_numFiresPerYear) <- c('year', 'n_fires')
vec_numFiresPerYear$year <- as.numeric(as.character(vec_numFiresPerYear$year))
tab_hectaresConverted <- left_join(tab_hectaresConverted, vec_numFiresPerYear)
tab_hectaresConverted <- tab_hectaresConverted %>% relocate(n_fires, .after = year)

# add totals by year
tab_hectaresConverted2 <-
  as.data.frame(aggregate(x = cbind(burned_herb_hectares, burned_wood_hectares, lost_woody_dominance_hectares) ~ year,
                          data = dat_fire, FUN = sum))
colnames(tab_hectaresConverted2)[2:ncol(tab_hectaresConverted2)] <-
  paste0(colnames(tab_hectaresConverted2)[2:ncol(tab_hectaresConverted2)], '_total')
tab_hectaresConverted <- left_join(tab_hectaresConverted, tab_hectaresConverted2)
rm(tab_hectaresConverted2)

# create plot data
dat_plot <-
  as.data.frame(aggregate(x = cbind(burned_herb_hectares, burned_wood_hectares, lost_woody_dominance_hectares) ~ year,
                          data = dat_fire, FUN = sum, na.rm = TRUE))
dat_nFires <- as.data.frame(table(dat_fire$year))
colnames(dat_nFires) <- c('year', 'n_fires')
dat_nFires$year <- as.numeric(as.character(dat_nFires$year))
dat_plot <- left_join(dat_plot, dat_nFires)

# aggregate plot data for faceting
dat_plotMelt <-
  with(dat_plot,
       data.frame(
         year = year,
         val = c(burned_herb_hectares, burned_wood_hectares, lost_woody_dominance_hectares, n_fires),
         lab = rep(c('Grassy dominant', 'Woody dominant', 'Wood to grass', 'Number of fires'),
                   each = nrow(dat_plot))
       )
  )
dat_plotMelt$lab <-
  factor(dat_plotMelt$lab,
         levels = c('Grassy dominant', 'Woody dominant', 'Wood to grass', 'Number of fires'))

# plot - facets
ggplot(data = dat_plotMelt,
       aes(x = year, y = val)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(lab), scales = 'free_y') +
  labs(y = 'Hectares', x = NULL) +
  theme(text = element_text(size = 18))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels.png',
       dpi = 300, height = 10, width = 10)

# plot - stacked
ggplot() +
  geom_bar(data = dat_plotMelt[dat_plotMelt$lab != 'Number of fires',],
           aes(x = year, y = val, fill = lab, color = lab),
           stat = 'identity', position = position_dodge()) +
  labs(y = 'Hectares', x = NULL, fill = NULL, color = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = min(dat_plotMelt$year):max(dat_plotMelt$year),
                     labels = min(dat_plotMelt$year):max(dat_plotMelt$year)) +
  theme(text = element_text(size = 18),
        legend.position = c(0.875, 0.9),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels stacked.png',
       dpi = 300, height = 6, width = 10)




##### export data #####

write.csv(tab_hectaresConverted,
          file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/99 - dominant land cover of burned pixels.csv',
          row.names = FALSE)



