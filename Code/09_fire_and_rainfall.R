
library(raster); library(rgdal); library(ggplot2); library(ggpmisc)

# load rainfall raster
ras_rainfallIn <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")

# load fires and rasterize
shp_fires <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters', layer = '2019_1999_Hawaii_Fire_Perimeters')
ras_fires <- rasterize(shp_fires,  ras_rainfallIn)
ras_fires[!is.na(ras_fires)] <- 1

# compare fires to rainfall
dat <- data.frame(fire_dummy = values(ras_fires),
                  rainAnnIn  = values(ras_rainfallIn))
dat[!is.na(dat$rainAnnIn) & is.na(dat$fire_dummy), 'fire_dummy'] <- 0
dat <- dat[complete.cases(dat),]

aggregate(dat$rainAnnIn, list(dat$fire_dummy), summary)

ggplot(data = dat) + geom_density(aes(rainAnnIn, color = as.character(fire_dummy), fill = as.character(fire_dummy)), alpha = 0.3) +
  labs(x = 'Mean annual rainfall (inches)', y = 'Density') +
  scale_fill_discrete(name = 'Fire dummy') + scale_color_discrete(name = 'Fire dummy') +
  scale_x_continuous(limits = c(0, 300)) +
  annotate(geom = 'table', x = 300, y = 0.04,
           label = list(data.frame(`Ann rainfall (in)`      = c('Min', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max'),
                                   `No fire` = c(8.0,   26.4,      49.3,     72.7,   98.5,      404),
                                   Fire      = c(8.5,   16.9,      22.7,     30.7,   33.4,      157))))
