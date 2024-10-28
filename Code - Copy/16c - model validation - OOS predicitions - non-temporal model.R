
# This script performs out-of-sample prediction by removing one watershed from the data
# at a time, then running the model to predict its fire sizes.

library(ggplot2)
library(ggh4x)
library(mgcv)
library(rms)
library(doParallel)


# load data - non-temporal
dat_watershed <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'dat_watershed.rds'))

# add interaction variable
dat_watershed$numForestGrassPixels1999Interaction <-
  with(dat_watershed,
       numForestPixels1999 * numGrassPixels1999)

# set looping variables
vec_knots <- 3:5
vec_watersheds <- unique(dat_watershed$watershed_ID)
registerDoParallel(cores = length(vec_knots))




##### run GAM model for each knot-watershed combo #####

list_modelResults_GAM <- list()
  for(i in seq_along(vec_knots)){
                    
    # for k = vec_knots[[i]], run model excluding one watershed at a time
    list_modelsSameNumKnots <- list()
    for(w in seq_along(vec_watersheds)){
      
      # run model
      # mod <- gam(formula =
      #              forestLoss_totalBurnedPixels ~
      #              s(numGrassPixels1999, bs="tp", k = i) +
      #              s(numForestPixels1999, bs="tp", k = i) +
      #              te(numGrassPixels1999, numForestPixels1999, k = i) +
      #              s(rainfall_mmMeanAnn, bs="tp", k = i) +
      #              s(fireRisk_75thPctileMaxHistorical, bs="tp", k = i),
      #            data = dat_watershed[dat_watershed$watershed_ID != vec_watersheds[[w]],],
      #            family = 'poisson')
      
      mod <- ols(formula = 
                   forestLoss_totalBurnedPixels ~
                   rcs(numGrassPixels1999, parms = vec_knots[[i]]) +
                   rcs(numForestPixels1999, parms = vec_knots[[i]]) +
                   rcs(rainfall_mmMeanAnn,  parms = vec_knots[[i]]) +
                   rcs(fireRisk_75thPctileMaxHistorical,  parms = vec_knots[[i]]),
                 data = dat_watershed[dat_watershed$watershed_ID != vec_watersheds[[w]],]
                 )
      
      # use model to predict fire size values for watershed w
      vec_predVals <-
        predict(mod,
                newdata =
                  dat_watershed[dat_watershed$watershed_ID ==
                                                vec_watersheds[[w]],
                                ]
                )
      if(vec_predVals < 0){
        vec_predVals = 0
      }
      
      # get actual fire size values for watershed w
      vec_actualVals <-
        dat_watershed[dat_watershed$watershed_ID == vec_watersheds[[w]],
                      'forestLoss_totalBurnedPixels']
      
      # get summary stats
      dd <- datadist(dat_watershed)
      options(datadist="dd")
      
      list_modelsSameNumKnots[[w]] <-
        c('predVal' = vec_predVals,
          'actualVal' = vec_actualVals,
          'modelR2' = mod$stats[[4]],
          'Penalized max likelihood' = mod$stats[[6]],
          'AIC' = AIC(mod))
      
      gc()
        
    }
    
    # calculate RMSE
    resids <- sapply(list_modelsSameNumKnots, function(w){
      w[[1]] - w[[2]]
    })
    RMSE <- sqrt(sum(resids^2)/length(resids))
    
    # calculate absolute mean error
    MAE <- mean(abs(resids))
    
    # calculate mean R^2
    meanR2 <-
      mean(
        sapply(list_modelsSameNumKnots, function(w){
          w[[3]]
          }
          ),
        na.rm = TRUE
        )
    
    # calculate mean penalized max likelihood
    meanPenMaxLike <-
      mean(
        sapply(list_modelsSameNumKnots, function(w){
          w[[4]]
        }
        ),
        na.rm = TRUE
      )
    
    # calculate AIC
    meanAIC <-
      mean(
        sapply(list_modelsSameNumKnots, function(w){
          w[[5]]
        }
        ),
        na.rm = TRUE
      )
    
    # return RMSE and MAE
    results <- c('RMSE' = RMSE,
                 'MAE' = MAE,
                 'Mean R^2' = meanR2,
                 'Mean Penalized Max Likelihood' = meanPenMaxLike,
                 'Mean AIC' = meanAIC)
    gc()
    
    list_modelResults_GAM[[i]] <- results
  }




##### single-variable effect plots #####

# run models on full dataset
list_models <- list()
# for(m in 1:5){
#   list_models[[m]] <-
#     gam(formula =
#           forestLoss_totalBurnedPixels ~
#           s(numGrassPixels1999, bs="tp", k = m) +
#           s(numForestPixels1999, bs="tp", k = m) +
#           te(numGrassPixels1999, numForestPixels1999, k = m) +
#           s(rainfall_mmMeanAnn, bs="tp", k = m) +
#           s(fireRisk_75thPctileMaxHistorical, bs="tp", k = m),
#         data = dat_watershed,
#         family = 'poisson')
# }
# names(list_models) <- paste('k =', 1:5)
for(m in 1:3){
  list_models[[m]] <-
    ols(formula = 
          forestLoss_totalBurnedPixels ~
          rcs(numGrassPixels1999, parms = vec_knots[[m]]) +
          rcs(numForestPixels1999, parms = vec_knots[[m]]) +
          rcs(rainfall_mmMeanAnn,  parms = vec_knots[[m]]) +
          rcs(fireRisk_75thPctileMaxHistorical, parms = vec_knots[[m]]),
        data = dat_watershed
    )
}

# initiate plot data
list_plotdat <- list(dat_watershed, dat_watershed, dat_watershed,
                     dat_watershed)
names(list_plotdat) <-
  c('fireRisk_75thPctileMaxHistorical', 'rainfall_mmMeanAnn',
    'numGrassPixels1999', 'numForestPixels1999')
list_plotdat$fireRisk_75thPctileMaxHistorical$variable <- 'Fire risk'
list_plotdat$rainfall_mmMeanAnn$variable <- 'Rainfall'
list_plotdat$numGrassPixels1999$variable <- 'Grassland area'
list_plotdat$numForestPixels1999$variable <- 'Forest area'

# adjust column values accding to whether they're variable or control
  # Variable = sequence along range of values. Control = mean value
list_plotdat$fireRisk_75thPctileMaxHistorical$fireRisk_75thPctileMaxHistorical <-
  seq(quantile(dat_watershed$fireRisk_75thPctileMaxHistorical, 0.10),
      quantile(dat_watershed$fireRisk_75thPctileMaxHistorical, 0.90),
      length.out = nrow(dat_watershed)
      )
list_plotdat$fireRisk_75thPctileMaxHistorical$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$fireRisk_75thPctileMaxHistorical$numGrassPixels1999 <-
  mean(dat_watershed$numGrassPixels1999)
list_plotdat$fireRisk_75thPctileMaxHistorical$numForestPixels1999 <-
  mean(dat_watershed$numForestPixels1999)
list_plotdat$fireRisk_75thPctileMaxHistorical$numForestGrassPixels1999Interaction <-
  with(list_plotdat$fireRisk_75thPctileMaxHistorical,
       numGrassPixels1999 * numForestPixels1999)

list_plotdat$rainfall_mmMeanAnn$fireRisk_75thPctileMaxHistorical <-
  mean(dat_watershed$fireRisk_75thPctileMaxHistorical)
list_plotdat$rainfall_mmMeanAnn$rainfall_mmMeanAnn <-
  seq(quantile(dat_watershed$rainfall_mmMeanAnn, 0.10),
      quantile(dat_watershed$rainfall_mmMeanAnn, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$rainfall_mmMeanAnn$numGrassPixels1999 <-
  mean(dat_watershed$numGrassPixels1999)
list_plotdat$rainfall_mmMeanAnn$numForestPixels1999 <-
  mean(dat_watershed$numForestPixels1999)
list_plotdat$rainfall_mmMeanAnn$numForestGrassPixels1999Interaction <-
  with(list_plotdat$rainfall_mmMeanAnn,
       numGrassPixels1999 * numForestPixels1999)

list_plotdat$numGrassPixels1999$fireRisk_75thPctileMaxHistorical <-
  mean(dat_watershed$fireRisk_75thPctileMaxHistorical)
list_plotdat$numGrassPixels1999$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$numGrassPixels1999$numGrassPixels1999 <-
  seq(quantile(dat_watershed$numGrassPixels1999, 0.10),
      quantile(dat_watershed$numGrassPixels1999, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$numGrassPixels1999$numForestPixels1999 <-
  mean(dat_watershed$numForestPixels1999)
list_plotdat$numGrassPixels1999$numForestGrassPixels1999Interaction <-
  with(list_plotdat$numGrassPixels1999,
       numGrassPixels1999 * numForestPixels1999)

list_plotdat$numForestPixels1999$fireRisk_75thPctileMaxHistorical <-
  mean(dat_watershed$fireRisk_75thPctileMaxHistorical)
list_plotdat$numForestPixels1999$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$numForestPixels1999$numGrassPixels1999 <-
  mean(dat_watershed$numGrassPixels1999)
list_plotdat$numForestPixels1999$numForestPixels1999 <-
  seq(quantile(dat_watershed$numForestPixels1999, 0.10),
      quantile(dat_watershed$numForestPixels1999, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$numForestPixels1999$numForestGrassPixels1999Interaction <-
  with(list_plotdat$numForestPixels1999,
       numGrassPixels1999 * numForestPixels1999)

# stack data and repeat: one dataset for each model
# plotdat <- do.call(rbind, list_plotdat)
# plotdat <- list(plotdat, plotdat, plotdat, plotdat, plotdat)
# for(i in seq_along(plotdat)){
#   plotdat[[i]]$k <- i
# }
# names(plotdat) <- paste0('k',1:5)
plotdat <- do.call(rbind, list_plotdat)
plotdat <- list(plotdat, plotdat, plotdat)
for(i in seq_along(plotdat)){
  plotdat[[i]]$k <- i
}
names(plotdat) <- paste0('k',1:3)

# get predicted values for each model
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval =
    predict(object = list_models[[i]],
            newdata = plotdat[[i]])
}

# get 95% CI values for each model
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval_lower95 =
    plotdat[[i]]$predval - 1.96 *
    predict(object = list_models[[i]],
            newdata = plotdat[[i]],
            se.fit = TRUE)$se.fit
}
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval_upper95 =
    plotdat[[i]]$predval + 1.96 *
    predict(object = list_models[[i]],
            newdata = plotdat[[i]],
            se.fit = TRUE)$se.fit
}

# create final plotdat
plotdat_final <-
  data.frame(y = c(plotdat[[1]]$predval, plotdat[[2]]$predval, plotdat[[3]]$predval),
                   #plotdat[[4]]$predval, plotdat[[5]]$predval),
             
             y_lower95 = c(plotdat[[1]]$predval_lower95, plotdat[[2]]$predval_lower95, plotdat[[3]]$predval_lower95),
                           #plotdat[[4]]$predval_lower95, plotdat[[5]]$predval_lower95),
             
             y_upper95 = c(plotdat[[1]]$predval_upper95, plotdat[[2]]$predval_upper95, plotdat[[3]]$predval_upper95),
                           #plotdat[[4]]$predval_upper95, plotdat[[5]]$predval_upper95),
             
             x = c(plotdat[[1]][plotdat[[1]]$variable == 'Fire risk',      'fireRisk_75thPctileMaxHistorical'] * 100,
                   plotdat[[1]][plotdat[[1]]$variable == 'Forest area',    'numForestPixels1999'],
                   plotdat[[1]][plotdat[[1]]$variable == 'Grassland area', 'numGrassPixels1999'],
                   plotdat[[1]][plotdat[[1]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[2]][plotdat[[2]]$variable == 'Fire risk',      'fireRisk_75thPctileMaxHistorical'] * 100,
                   plotdat[[2]][plotdat[[2]]$variable == 'Forest area',    'numForestPixels1999'],
                   plotdat[[2]][plotdat[[2]]$variable == 'Grassland area', 'numGrassPixels1999'],
                   plotdat[[2]][plotdat[[2]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[3]][plotdat[[3]]$variable == 'Fire risk',      'fireRisk_75thPctileMaxHistorical'] * 100,
                   plotdat[[3]][plotdat[[3]]$variable == 'Forest area',    'numForestPixels1999'],
                   plotdat[[3]][plotdat[[3]]$variable == 'Grassland area', 'numGrassPixels1999'],
                   plotdat[[3]][plotdat[[3]]$variable == 'Rainfall',       'rainfall_mmMeanAnn']),
                   
                   # plotdat[[4]][plotdat[[4]]$variable == 'Fire risk',      'fireRisk_75thPctileMaxHistorical'] * 100,
                   # plotdat[[4]][plotdat[[4]]$variable == 'Forest area',    'numForestPixels1999'],
                   # plotdat[[4]][plotdat[[4]]$variable == 'Grassland area', 'numGrassPixels1999'],
                   # plotdat[[4]][plotdat[[4]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   # 
                   # plotdat[[5]][plotdat[[5]]$variable == 'Fire risk',      'fireRisk_75thPctileMaxHistorical'] * 100,
                   # plotdat[[5]][plotdat[[5]]$variable == 'Forest area',    'numForestPixels1999'],
                   # plotdat[[5]][plotdat[[5]]$variable == 'Grassland area', 'numGrassPixels1999'],
                   # plotdat[[5]][plotdat[[5]]$variable == 'Rainfall',       'rainfall_mmMeanAnn']),
             
             var = c(rep(rep(c('Fire risk (annual % prob)', 'Forest area (pixels)', 'Grassland area (pixels)', 'Rainfall (mean mm/yr)'),
                             each = nrow(dat_watershed)),
                         times = length(plotdat)
                         )
                     ),
             
             # k = rep(paste('k =', 1:5), each = nrow(plotdat[[1]])
             k = rep(paste('k =', 3:5), each = nrow(plotdat[[1]])
                     )
             )

# create plot
ggplot(data = plotdat_final,) +
  geom_ribbon(aes(x = x, ymin = y_lower95, ymax = y_upper95),
              alpha = 0.3) +
  geom_line(aes(x = x, y = y), size = 1.3) +
  ggh4x::facet_grid2(rows = vars(var), cols = vars(k),
                     scales = 'free', independent = 'all') +
  labs(x = NULL, y = 'Forest fire size (pixels)') +
  theme(text = element_text(size = 15))

rm(list_plotdat, plotdat, plotdat_final, i, m, vec_knots)




##### plots: quantile comparisons - forest quantiles #####

# initiate data
nobs <- 6000  # set number of observations to generate
plotdat_forestQuantiles <-
  data.frame(numGrassPixels1999 =
               rep(seq(quantile(dat_watershed$numGrassPixels1999, 0.10),
                       quantile(dat_watershed$numGrassPixels1999, 0.90),
                       length.out = nobs),
                   times = 3
                   ),
             numForestPixels1999 =
               c(rep(quantile(dat_watershed$numForestPixels1999, 0.25), times = nobs),
                 rep(quantile(dat_watershed$numForestPixels1999, 0.50), times = nobs),
                 rep(quantile(dat_watershed$numForestPixels1999, 0.75), times = nobs)
                 ),
             forestSizeQuartile = 
               rep(c('Forest size quartile = 25%',
                     'Forest size quartile = 50%',
                     'Forest size quartile = 75%'),
                   each = nobs),
             rainfall_mmMeanAnn =
               mean(dat_watershed$rainfall_mmMeanAnn),
             fireRisk_75thPctileMaxHistorical =
               mean(dat_watershed$fireRisk_75thPctileMaxHistorical)
             )
plotdat_forestQuantiles <-
  rbind(plotdat_forestQuantiles, plotdat_forestQuantiles)
plotdat_forestQuantiles$k <-
  as.character(rep(c(2, 4), each = nobs))

# generate predicted values and 95% CI
plotdat_forestQuantiles$forestPixelsLost_predicted <-
  c(predict(object = list_models$`k = 2`,
            newdata = plotdat_forestQuantiles[1:(nobs*3),]),
    predict(object = list_models$`k = 4`,
            newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),])
    )
plotdat_forestQuantiles$forestPixelsLost_predicted_lower95 <-
  c(plotdat_forestQuantiles$forestPixelsLost_predicted[1:(nobs*3)] -
      predict(object = list_models$`k = 2`,
              newdata = plotdat_forestQuantiles[1:(nobs*3),],
              se = TRUE)$se.fit * 1.96,
    plotdat_forestQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_forestQuantiles)] -
      predict(object = list_models$`k = 4`,
              newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),],
              se = TRUE)$se.fit * 1.96)
plotdat_forestQuantiles$forestPixelsLost_predicted_upper95 <-
  c(plotdat_forestQuantiles$forestPixelsLost_predicted[1:(nobs*3)] +
      predict(object = list_models$`k = 2`,
              newdata = plotdat_forestQuantiles[1:(nobs*3),],
              se = TRUE)$se.fit * 1.96,
    plotdat_forestQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_forestQuantiles)] +
      predict(object = list_models$`k = 4`,
              newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),],
              se = TRUE)$se.fit * 1.96)

# create plot
ggplot(data = plotdat_forestQuantiles) +
  geom_ribbon(aes(x = numGrassPixels1999,
                  ymin = forestPixelsLost_predicted_lower95,
                  ymax = forestPixelsLost_predicted_upper95,
                  fill = k),
              alpha = 0.4) +
  geom_line(aes(x = numGrassPixels1999, y = forestPixelsLost_predicted,
                linetype = k),
            size = 1.3) +
  scale_fill_manual(values = c('gray50', 'gray50')) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  facet_wrap(facets = . ~ forestSizeQuartile) +
  labs(x = 'Number of grass pixels 1999',
       y = 'Predicted forest pixels lost', linetype = 'k') +
  guides(linetype = guide_legend(keywidth = 2.5, keyheight = 1),
         fill     = guide_legend(keywidth = 2.5, keyheight = 1)) +
  theme(text = element_text(size = 14))




##### plots: quantile comparisons - grassland quantiles #####

# initiate data
nobs <- 6000  # set number of observations to generate
plotdat_grassQuantiles <-
  data.frame(numForestPixels1999 =
               rep(seq(quantile(dat_watershed$numForestPixels1999, 0.10),
                       quantile(dat_watershed$numForestPixels1999, 0.90),
                       length.out = nobs),
                   times = 3
               ),
             numGrassPixels1999 =
               c(rep(quantile(dat_watershed$numGrassPixels1999, 0.25), times = nobs),
                 rep(quantile(dat_watershed$numGrassPixels1999, 0.50), times = nobs),
                 rep(quantile(dat_watershed$numGrassPixels1999, 0.75), times = nobs)
               ),
             forestSizeQuartile = 
               rep(c('Grass size quartile = 25%',
                     'Grass size quartile = 50%',
                     'Grass size quartile = 75%'),
                   each = nobs),
             rainfall_mmMeanAnn =
               mean(dat_watershed$rainfall_mmMeanAnn),
             fireRisk_75thPctileMaxHistorical =
               mean(dat_watershed$fireRisk_75thPctileMaxHistorical)
  )
plotdat_grassQuantiles <-
  rbind(plotdat_grassQuantiles, plotdat_grassQuantiles)
plotdat_grassQuantiles$k <-
  as.character(rep(c(2, 4), each = nobs))

# generate predicted values and 95% CI
plotdat_grassQuantiles$forestPixelsLost_predicted <-
  c(predict(object = list_models$`k = 2`,
            newdata = plotdat_grassQuantiles[1:(nobs*3),],
            type = 'response'),
    predict(object = list_models$`k = 4`,
            newdata = plotdat_grassQuantiles[((nobs*3)+1):nrow(plotdat_grassQuantiles),],
            type = 'response')
  )
plotdat_grassQuantiles$forestPixelsLost_predicted_lower95 <-
  c(plotdat_grassQuantiles$forestPixelsLost_predicted[1:(nobs*3)] -
      predict(object = list_models$`k = 2`,
              newdata = plotdat_grassQuantiles[1:(nobs*3),],
              type = 'response', se = TRUE)$se.fit * 1.96,
    plotdat_grassQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_grassQuantiles)] -
      predict(object = list_models$`k = 4`,
              newdata = plotdat_grassQuantiles[((nobs*3)+1):nrow(plotdat_grassQuantiles),],
              type = 'response', se = TRUE)$se.fit * 1.96)
plotdat_grassQuantiles$forestPixelsLost_predicted_upper95 <-
  c(plotdat_grassQuantiles$forestPixelsLost_predicted[1:(nobs*3)] +
      predict(object = list_models$`k = 2`,
              newdata = plotdat_grassQuantiles[1:(nobs*3),],
              type = 'response', se = TRUE)$se.fit * 1.96,
    plotdat_grassQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_grassQuantiles)] +
      predict(object = list_models$`k = 4`,
              newdata = plotdat_grassQuantiles[((nobs*3)+1):nrow(plotdat_grassQuantiles),],
              type = 'response', se = TRUE)$se.fit * 1.96)

# create plot
ggplot(data = plotdat_grassQuantiles) +
  geom_ribbon(aes(x = numForestPixels1999,
                  ymin = forestPixelsLost_predicted_lower95,
                  ymax = forestPixelsLost_predicted_upper95,
                  fill = k),
              alpha = 0.4) +
  geom_line(aes(x = numForestPixels1999, y = forestPixelsLost_predicted,
                linetype = k),
            size = 1.3) +
  scale_fill_manual(values = c('gray50', 'gray50')) +
  scale_linetype_manual(values = c('solid', 'dashed')) +
  facet_wrap(facets = . ~ forestSizeQuartile) +
  labs(x = 'Number of forest pixels 1999',
       y = 'Predicted forest pixels lost', linetype = 'k') +
  guides(linetype = guide_legend(keywidth = 2.5, keyheight = 1),
         fill     = guide_legend(keywidth = 2.5, keyheight = 1)) +
  scale_y_continuous(limits = c(0, 1000)) +
  theme(text = element_text(size = 14))

rm(plotdat_forestQuantiles, plotdat_grassQuantiles, nobs)




##### scatterplots of actual data #####

plotdat <- 
  data.frame(forestPixelsLost =
               rep(dat_watershed$forestLoss_totalBurnedPixels, times = 4),
             var = with(dat_watershed,
                        c(fireRisk_75thPctileMaxHistorical,
                          numForestPixels1999,
                          numGrassPixels1999,
                          rainfall_mmMeanAnn)
                        ),
             varname = rep(c('Fire risk (75th %tile max historical)',
                             'Number of forest pixels 1999',
                             'Number of grassland pixels 1999',
                             'Mean annual rainfall (mm)'),
                           each = nrow(dat_watershed)
                           )
             )

ggplot(data = plotdat,
       aes(x = var, y = forestPixelsLost)
       ) +
  geom_point(alpha = 0.6) +
  facet_wrap(~varname,
             nrow = 2, ncol = 2,
             scales = 'free') +
  scale_y_continuous(limits = c(0, 1250)) +
  labs(x = 'Variable value (see facet labels)', y = 'Forest lost (pixels)') +
  theme(text = element_text(size = 14))




##### forest loss distributions #####

# find predicted values
dat_watershed$forestLoss_totalBurnedPixels_predictedK2 <-
  predict(object = gam(formula =
                         forestLoss_totalBurnedPixels ~
                         s(numGrassPixels1999, bs="tp", k = 2) +
                         s(numForestPixels1999, bs="tp", k = 2) +
                         te(numGrassPixels1999, numForestPixels1999, k = 2) +
                         s(rainfall_mmMeanAnn, bs="tp", k = 2) +
                         s(fireRisk_75thPctileMaxHistorical, bs="tp", k = 2),
                       data = dat_watershed,
                       family = 'poisson'),
          newdata = dat_watershed, type = 'response')
dat_watershed$forestLoss_totalBurnedPixels_predictedK4 <-
  predict(object = gam(formula =
                         forestLoss_totalBurnedPixels ~
                         s(numGrassPixels1999, bs="tp", k = 4) +
                         s(numForestPixels1999, bs="tp", k = 4) +
                         te(numGrassPixels1999, numForestPixels1999, k = 4) +
                         s(rainfall_mmMeanAnn, bs="tp", k = 4) +
                         s(fireRisk_75thPctileMaxHistorical, bs="tp", k = 4),
                       data = dat_watershed,
                       family = 'poisson'),
          newdata = dat_watershed, type = 'response')

# get forest size lost quartiles
vec_forestSizeLost_k2 <-
  quantile(dat_watershed$forestLoss_totalBurnedPixels_predictedK2,
           probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
vec_forestSizeLost_k4 <-
  quantile(dat_watershed$forestLoss_totalBurnedPixels_predictedK4,
           probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

# create plot data
nobs <- 500
plotdat <-
  data.frame(val = c(rpois(nobs, vec_forestSizeLost_k2[[1]]),
                     rpois(nobs, vec_forestSizeLost_k2[[2]]),
                     rpois(nobs, vec_forestSizeLost_k2[[3]]),
                     rpois(nobs, vec_forestSizeLost_k2[[4]]),
                     rpois(nobs, vec_forestSizeLost_k2[[5]]),
                     rpois(nobs, vec_forestSizeLost_k4[[1]]),
                     rpois(nobs, vec_forestSizeLost_k4[[2]]),
                     rpois(nobs, vec_forestSizeLost_k4[[3]]),
                     rpois(nobs, vec_forestSizeLost_k4[[4]]),
                     rpois(nobs, vec_forestSizeLost_k4[[5]])
                     ),
             quartile = rep(rep(paste0('Mean value pctile = ',
                                       c('10%', '25%', '50%', '75%', '90%')
                                       ),
                                each = nobs),
                            times = 2),
             k = rep(paste0('k = ', c(2, 4)), each = nobs*5)
             )
plotdat$quartile <-
  factor(plotdat$quartile,
         levels = paste0('Mean value pctile = ',
                         c('10%', '25%', '50%', '75%', '90%')
  ))

# create plot
ggplot(data = plotdat,
       aes(x = val)
       ) +
  geom_histogram(fill = 'gray40', alpha = 0.5, color = 'black') +
    facet_grid2(rows = vars(quartile), cols = vars(k),
                scales = 'free', independent = 'all') +
  labs(x = 'Forest size lost to fire (pixels)',
       y = 'Number of predicted values') +
  theme(text = element_text(size = 14))





##### export model #####

# use k=3 model
model_final <-
  ols(formula = 
        forestLoss_totalBurnedPixels ~
        rcs(numGrassPixels1999, parms = 3) +
        rcs(numForestPixels1999, parms = 3) +
        rcs(rainfall_mmMeanAnn,  parms = 3) +
        rcs(fireRisk_90thPctileMaxHistorical,  parms = 3),
      data = dat_watershed
  )


saveRDS(model_final,
        file =
          paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 '16c - model validation - final RCS 3 knots.rds')
        )
