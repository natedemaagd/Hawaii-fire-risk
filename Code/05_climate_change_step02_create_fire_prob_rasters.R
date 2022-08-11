library(raster); library(fields); library(snow)

# define parameters
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
month <- c('10', '11', '12', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
Model <- c('sta', 'dyn')
rcp   <- c('rcp45', 'rcp85')
period <- c('2070', '2100')

# load models
load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

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
rm(m1.3.1.bi, m1.3.1.ka, m1.3.1.kaoa, m1.3.1.mn, m1.3.1.oa, evalbi, evalka, evalkaoa, evalmn, evaloa)
gc()

# create prediction raster from appropriate input rasters
for(i in 1:length(island)){   # length(island)
  for(m in 4:length(month)){
    for(p in 1:length(period)){
      for(M in 1:length(Model)){
        for(r in 1:length(rcp)){
          
          # skip 2070 for dynamical model - no data for this scenario
          if(p == 1 & M == 2) next
          
          # herb cover, wood cover, soil moisture, max temp, annual temp, and ignition rasters
          herbcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/herbcov_yearof/herbcov_yearof_', island[[i]], '_2016_.tif'))
          names(herbcov_yearof) <- 'herbcov_yearof'
          woodcov_yearof <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/woodcov_yearof/woodcov_yearof_', island[[i]], '_2016_.tif'))
          names(woodcov_yearof) <- 'woodcov_yearof'
          HI_EVAP_min_monthly_soil_mst <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/HIEVAP_min_monthly_soil_mst/HIEVAP_min_monthly_soil_mst_', island[[i]], '_.tif'))
          names(HI_EVAP_min_monthly_soil_mst) <- 'HI_EVAP_min_monthly_soil_mst'
          maxTemp_C <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp_max/future_tempmax_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m]], '.tif'))
          names(maxTemp_C) <- 'maxTemp_C'
          mean_annual_temp <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/temp/future_temp_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '.tif'))
          names(mean_annual_temp) <- 'mean_annual_temp'
          ign_trns <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/raster_stacks/ign_trns/ign_trns_', island[[i]], '.tif'))
          names(ign_trns) <- 'ign_trns'
          
          # get 1-month-prior rainfall
          rf_1mo_prior <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m-1]], '.tif'))
          names(rf_1mo_prior) <- 'rf_1mo_prior'
          
          # get previous 3 months rainfall
          monthly_cumrf_3mo <- list(raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m-1]], '.tif')),
                                    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m-2]], '.tif')),
                                    raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m-3]], '.tif')))
          monthly_cumrf_3mo <- Reduce('+', monthly_cumrf_3mo)
          names(monthly_cumrf_3mo) <- 'monthly_cumrf_3mo'
          
          # get previous 12 months rainfall
          monthly_cumrf_12mo <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/future_climate_rasters/rain/future_rain_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_ann.tif'))
          names(monthly_cumrf_12mo) <- 'monthly_cumrf_12mo'
          
          # dummy raster
          dum <- monthly_cumrf_12mo
          values(dum) <- 0
          names(dum) <- 'dum'
          dum[is.na(monthly_cumrf_12mo)] <- NA
          
          # dummy year raster
          year <- dum
          year[year == 0] <- as.numeric(period[[p]])
          names(year) <- 'year'
          year[is.na(monthly_cumrf_12mo)] <- NA
          
          # create stack
          raster_stack <- stack(list(herbcov_yearof, woodcov_yearof, HI_EVAP_min_monthly_soil_mst, maxTemp_C, mean_annual_temp,
                                     ign_trns, rf_1mo_prior, monthly_cumrf_3mo, monthly_cumrf_12mo, dum, year))
          
          # predicted values and their SEs - use custom function to return raster stack of both
          beginCluster(5)
          predfun <- function(model, data) {
            v <- predict(model, data, se.fit=TRUE, type = 'response')
            cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
          }
          raster_predVals <-
            predict(raster_stack, model_list[[i]], fun = predfun, index=1:2)
          endCluster()
          
          # write rasters - predicted values and std errors
          writeRaster(raster_predVals[[1]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/future_monthly_mean_fire_prob_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m]], '.tif'), overwrite = TRUE)
          writeRaster(raster_predVals[[2]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/future_monthly_mean_fire_prob_', Model[[M]], '_', rcp[[r]], '_', period[[p]], '_', island[[i]], '_month', month[[m]], '_SE.tif'), overwrite = TRUE)
          
          gc()
          removeTmpFiles(h=5)
        }
      }
    }
  }
}
