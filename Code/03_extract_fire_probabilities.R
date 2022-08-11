library(rgdal); library(dplyr); library(raster)

# load fire data
shp <- readOGR("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters", layer = '2019_1999_Hawaii_Fire_Perimeters')

# create new year and month variabes: YYYY and MM
shp$year  <- shp$Year; shp$Year <- NULL
shp$month <- substr(shp$YYYYMMDD, 5, 6)

# create island variable to match with filenaming convention: island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
fire_location <- strsplit(shp$kmlname, '_')
fire_location <- sapply(fire_location, function(x) x[[4]])
fire_location[fire_location == 'HawaiiCountry'] <- 'HawaiiCounty'
fire_location[fire_location == 'OahuCounty'] <- 'HonoluluCounty'
fire_location <- as.data.frame(fire_location)
  names(fire_location) <- 'location_name'
fire_location_match <- as.data.frame(unique(fire_location))
  names(fire_location_match) <- 'location_name'
fire_location_match$island <- c('Hawaii', 'Oahu', 'MauiCounty', 'Kauai')
fire_location <- left_join(fire_location, fire_location_match)
shp$island <- fire_location$island
rm(fire_location, fire_location_match)

# split each fire into a separate polygon
shp_split <- lapply(1:length(shp), function(i) shp[i, ])

gc()


# for each fire, load corresponding probability raster and extract values within fire perimeter - error returned due to fires beyond 2016: OK
fire_data <- list()
for(i in 1:length(shp_split)){
  
  # crate list to store data about fire i: element 1 is values of pixels within fire perimeter, element 2 is samle of 1000 pixles outside of all fire perimeters in the same year-month and on the same island
  fire_data[[i]] <- list()
  
  # load raster for appropriate island and year-month
  fire_island <- shp$island[[i]]
  fire_year   <- shp$year[[i]]
  fire_month  <- shp$month[[i]]
  fire_raster <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/monthly_mean_fire_prob_',
                               fire_island, '_', fire_year, fire_month, '.tif'))
  
  # get values of pixels within fire perimeter
  fire_values <- unlist(extract(fire_raster, shp_split[[i]]))
  
  # check for other fires on the same island in the same month, then sample 1000 points not within any fire
  other_fires <- shp[shp$island == fire_island & shp$year == fire_year & shp$month == fire_month,]
  sample_nonfire_vals <- rasterize(other_fires, fire_raster, field = 99999, update = TRUE)
  sample_nonfire_vals <- values(sample_nonfire_vals)
  sample_nonfire_vals <- sample_nonfire_vals[sample_nonfire_vals != 99999 & !is.na(sample_nonfire_vals)]
  sample_nonfire_vals <- sample(sample_nonfire_vals, 1000)
  
  # save data to list
  fire_data[[i]][[1]] <- fire_values
  fire_data[[i]][[2]] <- sample_nonfire_vals
  fire_data[[i]][[3]] <- fire_island
  fire_data[[i]][[4]] <- paste0(fire_year, fire_month)
  names(fire_data[[i]]) <- c('fire_values', 'sample_nonfire_vals', 'island', 'yearmonth')
  
  gc()
}

# save fire data
saveRDS(fire_data, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_pixel_data.rds')
