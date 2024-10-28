
# this script finds the monthly hectares at each risk threshold in the historical data then aggregates it

library(raster)

# list all raster stacks holding threshold dummies
list_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters',
                             pattern = 'dummyRasterStack')

# get unique risk levels
list_riskLevels <- unique(
  sapply(strsplit(list_filenames, '_'),
                          function(x) x[[2]])
)

# get unique islands
list_islands <- unique(
  sapply(strsplit(list_filenames, '_'),
         function(x) x[[3]])
)
list_islands <-
  substr(list_islands,
         1, nchar(list_islands)-4)

# set up data.frame to tabulate total pixels
dat_areaTabulation <-
  as.data.frame(expand.grid(list_islands, list_riskLevels))
colnames(dat_areaTabulation) <- c('island', 'risk')
dat_areaTabulation <-
  dat_areaTabulation[with(dat_areaTabulation, order(island, risk)),]

# for each island-risk combo, calculate number of pixels that crossed the threshold
list_totalPixels <- list()
list_totalHectares <- list()
for(i in 1:nrow(dat_areaTabulation)){
  
  # read raster stack
  rStack <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13a fire risk threshold surpass dummy rasters/dummyRasterStack_',
                           dat_areaTabulation$risk[[i]], '_',
                           dat_areaTabulation$island[[i]], '.rds'))
  gc()
  
  # sum monthly dummy values to get total pixels in risk level
  totalPixels <- list()
  for(j in 1:length(rStack@layers)){
    vals <- values(rStack[[j]])
    vals <- vals[!is.na(vals)]
    totalPixels[[j]] <- sum(vals)
    rm(vals); gc()
  }
  totalPixels <- unlist(totalPixels)
  
  # add vector of totalPixels to data.frame, and convert to hectares
  list_totalPixels[[i]] <- totalPixels
  list_totalHectares[[i]] <- list_totalPixels[[i]] * 900 / 10000
  
  rm(rStack, i, j, totalPixels); gc()
}

# create summary data
names(list_totalPixels) <-
  names(list_totalHectares) <-
  with(dat_areaTabulation,
       paste(island, risk, sep = '_'))
list_totalHectares_summary <- lapply(list_totalHectares, summary)

# save summary data - "double counted" pixels
dat_totalHectares_summary <- as.data.frame(
  do.call(rbind, list_totalHectares_summary)
  )
saveRDS(dat_totalHectares_summary,
        file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14 historical monthly risk threshold tabulation/14 total monthly hectares summary.rds')

# convert hectare values so they aren't double counted: if, e.g., a pixel is in the high category, don't count it in the moderate category
rownames <- strsplit(rownames(dat_totalHectares_summary), '_')
dat_totalHectares_summary$island <- sapply(rownames, function(x) x[[1]])
dat_totalHectares_summary$risk <- factor(sapply(rownames, function(x) x[[2]]),
                                         levels = c('lowFireRisk', 'moderateFireRisk', 'highFireRisk'))
dat_totalHectares_summary <-
  dat_totalHectares_summary[with(dat_totalHectares_summary,
                                 order(island, risk)),]
dat_totalHectares_summaryNoDoubleCount <- dat_totalHectares_summary  # create new data.frame
for(i in seq(1, 10, 3)){
  dat_totalHectares_summaryNoDoubleCount[i,1:6] <-
    dat_totalHectares_summaryNoDoubleCount[i,1:6] -
    (dat_totalHectares_summaryNoDoubleCount[i+1,1:6] +
       dat_totalHectares_summaryNoDoubleCount[i+2,1:6])
}
for(i in seq(2, 11, 3)){
  dat_totalHectares_summaryNoDoubleCount[i,1:6] <-
    dat_totalHectares_summaryNoDoubleCount[i,1:6] -
    dat_totalHectares_summaryNoDoubleCount[i+1,1:6]
}

# save non-double-counted hectare tabulation
saveRDS(dat_totalHectares_summaryNoDoubleCount,
        file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/14 historical monthly risk threshold tabulation/14 total monthly hectares summary no double counting.rds')
