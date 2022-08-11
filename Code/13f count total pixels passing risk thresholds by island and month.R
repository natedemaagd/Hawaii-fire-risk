
# this script creates scatter plots and regresses (burn area) ~ (fire risk threshold frequency)

library(raster); library(sf)




##### data reading and initial formatting #####

# define islands and risk levels
vec_islands <- c('Hawaii', 'Oahu', 'Kauai',  'MauiCounty')
vec_riskLevels <- c('low', 'moderate', 'high')

# load burn data
dat_burnPerimeters <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")

# separate fires by island
list_islandExtents <- list(  # define island extents
  extent(-156.2, -154.8, 18.8, 20.28),  # BI
  extent(-158.4, -157.5,  21.2, 21.8),  # Oahu
  extent(-159.9, -159.2 , 21.8, 22.3),  # Kauai
  extent(-157.5, -155.9, 20.45, 21.3)   # Maui Nui
)

list_burnPerimeters_byIsland <- list()
for(i in 1:length(vec_islands)){
  list_burnPerimeters_byIsland[[i]] <- st_crop(dat_burnPerimeters,
                                               list_islandExtents[[i]])
}
names(list_burnPerimeters_byIsland) <- vec_islands

gc()




##### for each island, get number of acres burned each year #####

# create data.frame from each island's burn data
list_burnData_byIsland <- list()
for(i in 1:length(vec_islands)){
  list_burnData_byIsland[[i]] <-
    with(list_burnPerimeters_byIsland[[i]],
         data.frame(yearmonth = as.Date(paste(Year, Month, '1', sep = '-')),
                    island    = vec_islands[[i]],
                    burnArea_acres = Sat_sz_ac)
         )
}

# aggregate burn data by year-month
for(i in 1:length(vec_islands)){
  list_burnData_byIsland[[i]] <-
    with(list_burnData_byIsland[[i]], aggregate(burnArea_acres,
                                                list(yearmonth),
                                                sum
                                                )
         )
  list_burnData_byIsland[[i]]$island <- vec_islands[[i]]
  colnames(list_burnData_byIsland[[i]]) <-
    c('yearmonth', 'burnArea_acres', 'island')
}
names(list_burnData_byIsland) <- vec_islands

# combine data into single data.frame
dat_burnData <- do.call(rbind, list_burnData_byIsland)

rm(dat_burnPerimeters, list_burnData_byIsland, list_burnPerimeters_byIsland,
   list_islandExtents, i)

gc()




##### for each island, sum number of pixels that crossed each risk threshold each month #####

# list island dummy raster directories
list_directories <- list.dirs('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters')
list_directories <- list_directories[grep('13b', list_directories)]

# for each island, load each monthly raster and sum the values (1 = risk threshold crossed)
list_totalFreqCount <- list()
for(i in 1:length(vec_islands)){
  
  # define appropriate directory
  dir <- list_directories[grep(vec_islands[[i]], list_directories)]
  
  # sum by risk level for island i
  list_totalFreqCount[[i]] <- list()
  for(r in 1:length(vec_riskLevels)){
    
    # list all files for island i, risk level r
    list_filenames <- list.files(path = dir,
                                 pattern = vec_riskLevels[[r]])
    
    # create vector: elements are month m's total sum of pixels passing threshold r each month on island i
    list_totalFreqCount[[i]][[r]] <- list()
    for(m in 1:length(list_filenames)){
      
      # load raster
      ras <- raster(paste0(dir, '/', list_filenames[[m]]))
      
      # return the sum of all non-NA values in the raster
      list_totalFreqCount[[i]][[r]][[m]] <- sum(values(ras), na.rm = TRUE)
      
      rm(ras); gc()
      
    }
    
  }
  
}

saveRDS(list_totalFreqCount, file = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13f list_titalFreqCount BACKUP.rds")

rm(dir, i, m, r)

# create dataframes from list
vec_yearmonths <- strsplit(list_filenames, '_')
vec_yearmonths <- sapply(vec_yearmonths, function(x){
  substr(x[[4]], 1, 6)
})
dat_totalFreqCount <- data.frame(island    = rep(vec_islands,  each = length(vec_riskLevels)*length(vec_yearmonths)),
                                 yearmonth = rep(vec_yearmonths, times = length(vec_riskLevels)*length(vec_islands)),
                                 riskLevel = rep(vec_riskLevels, each = length(vec_yearmonths)),
                                 sumPixels = unlist(list_totalFreqCount))
dat_totalFreqCount$sumAcreage <- dat_totalFreqCount$sumPixels / 0.22239484

saveRDS(dat_totalFreqCount, file = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13f dat_totalFreqCount.rds")
