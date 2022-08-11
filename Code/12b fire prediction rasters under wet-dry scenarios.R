
library(raster); library(doParallel); library(stringr); library(mgcv)
registerDoParallel(cores = 4)

# load models
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")
rm(evalbi, evalka, evalkaoa, evalmn,evaloa, m1.3.1.kaoa,  m1.3.2.mn)

# define islands, months, scenarios
vec_islands   <- c('Oahu', 'MauiCounty', 'Hawaii', 'Kauai')
vec_scenarios <- list(c('25', '25'), c('75', '75'), c('25', '75'), c('75', '25'))  # historical rainfall percentiles as made in script 12a
  names(vec_scenarios) <- c('dry-dry', 'wet-wet', 'dry-wet', 'wet-dry')   # element 1 is short-term (prior month and prior 3 months); element 2 is long-term (prior 12 months)
vec_months <- sprintf("%02d", 1:12)

# for each month, island, and scenario, create fire prediction raster
foreach(s = 1:length(vec_scenarios), .packages = c('raster', 'stringr', 'mgcv')) %dopar% {
  for(i in 1:length(vec_islands)){
    for(m in 1:length(vec_months)){
      
      # define model to be used according to island i
      reg_model <- if(i == 1) m1.3.1.oa else {
                   if(i == 2) m1.3.1.mn else {
                   if(i == 3) m1.3.1.bi else {
                   if(i == 4) m1.3.1.ka }}}
      
      # load non-rainfall covariate rasters
      herbcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/herbcov_yearof/herbcov_yearof_', vec_islands[[i]], '_2016_.tif'))
      woodcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/woodcov_yearof/woodcov_yearof_', vec_islands[[i]], '_2016_.tif'))
      HI_EVAP_min_monthly_soil_mst <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst_', vec_islands[[i]], '_.tif'))
      mean_annual_temp <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/mean_annual_temp/mean_annual_temp_', vec_islands[[i]], '_.tif'))
      ign_trns <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/ign_trns/ign_trns_', vec_islands[[i]], '.tif'))
      
      
      ### load prior-month rainfall according to scenario (wet/dry)
      
        # new months vector that defines December as month prior to January
        vec_months2 <- c('12', sprintf("%02d", 1:12))
        
        # get month of year prior to month m
        prior_month <- vec_months2[[which(vec_months == vec_months[[m]])]]
        
        # load raster according to `prior_month` and scenario s
        rf_1mo_prior <- if(vec_scenarios[[s]][[1]] == '25'){
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm25pctile_', vec_islands[[i]], '_month', prior_month, '.tif'))
        } else {
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm75pctile_', vec_islands[[i]], '_month', prior_month, '.tif'))
        }
      
      rm(vec_months2, prior_month)
      gc()
      
      
      ### sum prior three months of rainfall
        
        # new months vector to get m-3 and m-2 for months early in the year
        vec_months2 <- c('10',  '11', '12', sprintf("%02d", 1:12))
        
        # get month m-3 and month m-2
        month_minus3 <- vec_months2[[which(vec_months == vec_months[[m]])]]
        month_minus2 <- vec_months2[[which(vec_months == vec_months[[m]])+1]]
        
        # load prior 3- and 2-month rainfall rasters according to month m-2 and m-3, and scenario s
        rf_2mo_prior <- if(vec_scenarios[[s]][[1]] == '25'){
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm25pctile_', vec_islands[[i]], '_month', month_minus2, '.tif'))
        } else {
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm75pctile_', vec_islands[[i]], '_month', month_minus2, '.tif'))
        }
        
        rf_3mo_prior <- if(vec_scenarios[[s]][[1]] == '25'){
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm25pctile_', vec_islands[[i]], '_month', month_minus3, '.tif'))
        } else {
          raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/monthlyHistoricalRainfall_mm75pctile_', vec_islands[[i]], '_month', month_minus3, '.tif'))
        }
        
        # sum three prior months of rainfall
        monthly_cumrf_3mo <- calc(stack(rf_1mo_prior, rf_2mo_prior, rf_3mo_prior), sum)
        rm(rf_2mo_prior, rf_3mo_prior, vec_months2, month_minus2, month_minus3)
        gc()
        
        
      ### prior 12 months of rainfall is just all months for island i and scenario s (long-term, so element 2 of scenario vector)
        
        # list all raster filenames for island i and scenario s
        raster_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters', pattern = '.tif')
        raster_filenames <- raster_filenames[str_detect(raster_filenames, vec_islands[[i]]) &   # subset filenames by island
                                               str_detect(raster_filenames, paste0('mm', vec_scenarios[[s]][[2]]))]   # subset filenames by scenario (element 2 of scenario s is long-term, i.e. 12-month, component)
        
        # load listed files and create raster stack
        raster_stack <- lapply(raster_filenames, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/12_historical rainfall quantile rasters/', r)))
        raster_stack <- stack(raster_stack)
        
        # create 12-month-prior rainfall
        monthly_cumrf_12mo <- calc(raster_stack, sum)
        
        rm(raster_filenames, raster_stack)
        gc()
        
        
      ### year and dummy raster
        
        # year raster: set values of raster to arbitrary year
        year <- herbcov_yearof
        values(year) <- 2001
        
        # dummy raster
        dum <- year
        values(dum) <- 0
      
      
      ### prediction raster
        
        # stack all covariate rasters
        raster_stack <- stack(herbcov_yearof, woodcov_yearof, HI_EVAP_min_monthly_soil_mst, mean_annual_temp, ign_trns,
                              rf_1mo_prior, monthly_cumrf_3mo, monthly_cumrf_12mo, year, dum)
        names(raster_stack) <- c('herbcov_yearof', 'woodcov_yearof', 'HI_EVAP_min_monthly_soil_mst', 'mean_annual_temp', 'ign_trns',
                                 'rf_1mo_prior', 'monthly_cumrf_3mo', 'monthly_cumrf_12mo', 'year', 'dum')
        
        # run prediction
        pred_raster <- predict(object = raster_stack, model = reg_model, type = 'response')
        
        # write prediction raster
        writeRaster(pred_raster, paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_wetDryScenarios/fireRiskPrediction_',
                                        vec_islands[[i]], '_month', vec_months[[m]], '_', names(vec_scenarios)[[s]], '.tif'),
                    overwrite = TRUE)
      
    }
  }
}
