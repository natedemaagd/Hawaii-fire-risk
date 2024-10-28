library(raster)
rasterOptions(tmpdir="C:/Users/nated/Downloads/tempDir")

# define islands, months, years
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
month <- c('10', '11', '12', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
years <- 1999:2016

# load models
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

# resample annual rainfall for use in stacks
monthly_cumrf_12mo_statewide <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
#monthly_cumrf_12mo_statewide <- resample(monthly_cumrf_12mo_statewide, raster_lc)

# define island extents
island_extents <- list(
  extent(-156.2, -154.8, 18.8, 20.28),  # BI
  #extent(-156.75, -155.9, 20.4, 21.1),
  #extent(-156.8, -156.45,20.45, 20.61),
  #extent( -157.5, -156.5,  21.05, 21.3),
  extent(-158.4, -157.5,  21.2, 21.8),  # Oahu
  extent(-159.9, -159.2 , 21.8, 22.3),  # Kauai
  #extent( -157.1, -156.75,  20.66, 20.95),
  extent(-157.5, -155.9, 20.45, 21.3)  # Maui Nui
)

# create list of models
model_list <- list(m1.3.1.bi, m1.3.1.oa, m1.3.1.ka, m1.3.1.mn)

# get list of rainfall rasters (for previous-month(s) rainfall stacks)
rf_files <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly', pattern = '.tif')


# load/create rasters, stack them, and save prediction raster
for(i in 1:4){
  
  # start timer for removal of temp rasters
  a <- Sys.time()
  
  for(y in 1:length(years)){
  for(m in 4:length(month)){
    
    # herb cover, wood cover, soil moisture, max temp, annual temp, and ignition rasters
    herbcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_herb_', years[[y]], '.tif'))
      names(herbcov_yearof) <- 'herbcov_yearof'
      herbcov_yearof <- crop(herbcov_yearof, island_extents[[i]])  # crop statewide herbcov to island i - repeat for woodcov below
    woodcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_wood_', years[[y]], '.tif'))
      names(woodcov_yearof) <- 'woodcov_yearof'
      woodcov_yearof <- crop(woodcov_yearof, island_extents[[i]])
    HI_EVAP_min_monthly_soil_mst <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst_', island[[i]], '_.tif'))
      names(HI_EVAP_min_monthly_soil_mst) <- 'HI_EVAP_min_monthly_soil_mst'
    maxTemp_C <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/MonthlyHistorical/MaxTempMonthly_', island[[i]], '_', years[[y]], month[[m]], '.tif'))
      names(maxTemp_C) <- 'maxTemp_C'
    mean_annual_temp <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp_', island[[i]], '_.tif'))
      names(mean_annual_temp) <- 'mean_annual_temp'
    ign_trns <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/ign_trns/ign_trns_', island[[i]], '.tif'))
      names(ign_trns) <- 'ign_trns'
    
    # get one-month-prior rainfall
    rf_1mo_prior <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-1]]))
    names(rf_1mo_prior) <- 'rf_1mo_prior'
    
    # get previous-three-months rainfall
    monthly_cumrf_3mo <- Reduce('+', list(rf_1mo_prior,
                                          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-2]])),
                                          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-3]]))))
    names(monthly_cumrf_3mo) <- 'monthly_cumrf_3mo'
    gc()
    
    # get previous-12-months rainfall
    monthly_cumrf_12mo <- Reduce('+', list(rf_1mo_prior,
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-2]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-3]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-4]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-5]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-6]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-7]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-8]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-9]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-10]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-11]])),
                                           raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', rf_files[[which(rf_files == paste0('rf_same_month_', island[[i]], '_', years[[y]], '_', month[[m]], '_.tif'))-12]]))))
    names(monthly_cumrf_12mo) <- 'monthly_cumrf_12mo'
    gc()
    
    # dummy raster
    dum <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/island_dummy/dummy_', island[[i]], '_.tif'))
    names(dum) <- 'dum'
    
    # dummy year raster
    year <- dum
    values(year) <- years[[y]]
    names(year) <- 'year'
    
    # create stack
    raster_stack <- stack(list(herbcov_yearof, woodcov_yearof, HI_EVAP_min_monthly_soil_mst, maxTemp_C, mean_annual_temp,
                               ign_trns, rf_1mo_prior, monthly_cumrf_3mo, monthly_cumrf_12mo, dum, year))
    
    # create prediction raster
    raster_predict <- raster::predict(raster_stack, model = model_list[[i]], progress = 'text', type = 'response')
    
    # write raster
    writeRaster(raster_predict, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical/',
                                                  'monthly_mean_fire_prob_', island[[i]], '_', years[[y]], month[[m]], '.tif'),
                overwrite = TRUE)
    
    gc()
    removeTmpFiles(1)
    
    # if an hour has passed, restore temporary rasters and restart timer
    if(Sys.time() - a > 3600){
      monthly_cumrf_12mo_statewide <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
      raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
      a <- Sys.time()
    }
  }
  }
}
