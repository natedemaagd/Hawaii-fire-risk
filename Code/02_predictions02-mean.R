library(raster); library(fields)

# define islands and months
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
month <- c('10', '11', '12', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

# load models
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

# resample annual rainfall for use in stacks
monthly_cumrf_12mo_statewide <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
raster_lc <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/stack_BaseLandcover_30m_latlon.tif')
monthly_cumrf_12mo_statewide <- resample(monthly_cumrf_12mo_statewide, raster_lc)

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



###############################################
########## KAUAI AND MAUI TEST MODELS #########
###############################################

# load models
m1.3.2.ka <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/Kauai and Maui landcover interaction model test/',
                 'm1.3.2.ka.rds'))
m1.3.3.mn <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/Kauai and Maui landcover interaction model test/',
                 'm1.3.3.mn.rds'))

# replace kauai model in model list
model_list[[3]] <- m1.3.2.ka
model_list[[4]] <- m1.3.3.mn

###############################################
###############################################
###############################################




# load/create rasters, stack them, and save prediction raster with SEs
# island i, month m (month starts at 4 since prediction requires prior 3 months)
for(i in 1:4){  # for running new Kauai/Maui test models with herb/woody interaction
#for(i in 1:4){
  for(m in 4:length(month)){   # 4:length(month)
    
    # herb cover, wood cover, soil moisture, max temp, annual temp, and ignition rasters
    herbcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/herbcov_yearof/herbcov_yearof_', island[[i]], '_2016_.tif'))
    names(herbcov_yearof) <- 'herbcov_yearof'
    woodcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/woodcov_yearof/woodcov_yearof_', island[[i]], '_2016_.tif'))
    names(woodcov_yearof) <- 'woodcov_yearof'
    HI_EVAP_min_monthly_soil_mst <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst_', island[[i]], '_.tif'))
    names(HI_EVAP_min_monthly_soil_mst) <- 'HI_EVAP_min_monthly_soil_mst'
    maxTemp_C <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/max_temp/AvgMonthlyHistorical/AvgMonthlyHistorical_', island[[i]], '_month', month[[m]], '.tif'))
    names(maxTemp_C) <- 'maxTemp_C'
    mean_annual_temp <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp_', island[[i]], '_.tif'))
    names(mean_annual_temp) <- 'mean_annual_temp'
    ign_trns <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/ign_trns/ign_trns_', island[[i]], '.tif'))
    names(ign_trns) <- 'ign_trns'
    
    # get mean one-month-prior rainfall for the given month
    priorMonthRainfall_names <- list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly',
                                           pattern = paste0('_', month[[(m-1)]], '_'))
    priorMonthRainfall_names <- priorMonthRainfall_names[grep(island[[i]], priorMonthRainfall_names)] # keep only the necessary island
    priorMonthRainfall_rasters <- lapply(priorMonthRainfall_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', r)))
    rf_1mo_prior <- Reduce(mean, priorMonthRainfall_rasters)
    names(rf_1mo_prior) <- 'rf_1mo_prior'
    rm(priorMonthRainfall_names, priorMonthRainfall_rasters)
    gc()
    
    # get mean previous-three-months rainfall for the given month
    priorMonthRainfall_names <- c(list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly',
                                             pattern = paste0('_', month[[(m-1)]], '_')),
                                  list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly',
                                             pattern = paste0('_', month[[(m-2)]], '_')),
                                  list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly',
                                             pattern = paste0('_', month[[(m-3)]], '_')))
    priorMonthRainfall_names <- priorMonthRainfall_names[grep(island[[i]], priorMonthRainfall_names)] # keep only the necessary island
    priorMonthRainfall_rasters <- lapply(priorMonthRainfall_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/rf_monthly/', r)))
    monthly_cumrf_3mo <- Reduce(mean, priorMonthRainfall_rasters)
    gc()
    month_groups <- split(1:length(priorMonthRainfall_names), ceiling(seq_along(1:length(priorMonthRainfall_names))/3))
    rainfall_3mo_cum <- list()
    for(g in 1:length(month_groups)){
      rainfall_3mo_cum[[g]] <- Reduce('+', priorMonthRainfall_rasters[month_groups[[g]]])
      gc()
    }
    monthly_cumrf_3mo <- Reduce(mean, rainfall_3mo_cum)
    names(monthly_cumrf_3mo) <- 'monthly_cumrf_3mo'
    rm(rainfall_3mo_cum, month_groups, priorMonthRainfall_names, priorMonthRainfall_rasters)
    gc()
    
    # monthly annual rainfall
    monthly_cumrf_12mo <- crop(monthly_cumrf_12mo_statewide, island_extents[[i]])
    names(monthly_cumrf_12mo) <- 'monthly_cumrf_12mo'
    
    # dummy raster
    dum <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/island_dummy/dummy_', island[[i]], '_.tif'))
    names(dum) <- 'dum'
    
    # dummy year raster
    year <- dum
    year[year == 0] <- 2001 #  choose random year; doesn't matter
    names(year) <- 'year'
    
    # create stack
    raster_stack <- stack(list(herbcov_yearof, woodcov_yearof, HI_EVAP_min_monthly_soil_mst, maxTemp_C, mean_annual_temp,
                               ign_trns, rf_1mo_prior, monthly_cumrf_3mo, monthly_cumrf_12mo, dum, year))
    
    # create predictions and their SEs
    dat.df    <- data.frame(rasterToPoints(raster_stack))
    pred.vals <- predict(model_list[[i]], newdata = dat.df, progress = 'text', type = 'response', se.fit = TRUE)
    
    # create and write rasters - predicted values and std errors
    raster_predVals <- herbcov_yearof  # choose random raster to place pred vals into
    values(raster_predVals) <- c(pred.vals$fit)
    writeRaster(raster_predVals, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                                                   'monthly_mean_fire_prob_', island[[i]], '_month', month[[m]], '-landcoverInteraction.tif'), overwrite = TRUE)
    
    raster_predSEs <- herbcov_yearof  # choose random raster to place pred vals into
    values(raster_predSEs) <- c(pred.vals$se.fit)
    writeRaster(raster_predSEs, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                                                  'monthly_mean_fire_prob_', island[[i]], '_month', month[[m]], '_SE-landcoverInteraction.tif'), overwrite = TRUE)
    
    gc()
    print(paste(i, m))
  }
}
