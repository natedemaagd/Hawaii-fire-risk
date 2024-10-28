
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
                 'dat_watershed_temporal.rds'))

# add interaction variable
dat_watershed$numForestGrassPixelsInteraction <-
  with(dat_watershed,
       (numForestPixelsBeforeFire/1e5 * numGrassPixelsBeforeFire/1e5))

# format data for model
dat_watershed <- dat_watershed[!is.na(dat_watershed$watershedID),]

# set looping variables
vec_knots <- 1:5
vec_watersheds <- unique(dat_watershed$watershedID)
registerDoParallel(cores = length(vec_knots))

# # run test model
# mod_test <-
#   gam(formula =
#         forestPixelsLost ~
#         s(numGrassPixelsBeforeFire, bs="tp", k = 2) +
#         s(numForestPixelsBeforeFire, bs="tp", k = 2) +
#         te(numGrassPixelsBeforeFire, numForestPixelsBeforeFire, k = 2) +
#         s(rainfall_mmMeanAnn, bs="tp", k = 2) +
#         s(fireRisk_75thPctileMax, bs="tp", k = 2),
#       data = dat_watershed,
#       family = 'poisson')




##### run GAM model for each knot-watershed combo #####

list_modelResults_GAM <-
  foreach(i = seq_along(vec_knots),
          .packages = c('mgcv')) %dopar% {
    
    # for k = vec_knots[[i]], run model excluding one watershed at a time
    list_modelsSameNumKnots <- list()
    for(w in seq_along(vec_watersheds)){
      
      # run model
      mod <- gam(formula =
                   forestPixelsLost ~
                   s(numGrassPixelsBeforeFire, bs="tp", k = i) +
                   s(numForestPixelsBeforeFire, bs="tp", k = i) +
                   te(numGrassPixelsBeforeFire, numForestPixelsBeforeFire, k = i) +
                   s(rainfall_mmMeanAnn, bs="tp", k = i) +
                   s(fireRisk_75thPctileMax, bs="tp", k = i),      # TRY AS ONLY COVARIATE (% forest lost within fire footprint ~ fire risk) <-- IGNORE WATERSHEDS
                 data = dat_watershed[dat_watershed$watershedID != vec_watersheds[[w]],],
                 family = 'poisson')
      
      # use model to predict fire size values for watershed w
      vec_predVals <-
        predict(mod,
                newdata =
                  dat_watershed[dat_watershed$watershedID ==
                                                vec_watersheds[[w]],
                                ]
                )
      
      # get actual fire size values for watershed w
      vec_actualVals <-
        dat_watershed[dat_watershed$watershedID == vec_watersheds[[w]],
                      'forestPixelsLost']
      
      list_modelsSameNumKnots[[w]] <-
        c('predVal' = vec_predVals,
          'actualVal' = vec_actualVals,
          'modelR2' = summary(mod)$r.sq,
          'modelExpDev' = summary(mod)$dev.expl,
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
          )
        )
    
    # calculate mean explained deviance
    meanExpDev <-
      mean(
        sapply(list_modelsSameNumKnots, function(w){
          w[[4]]
        }
        )
      )
    
    # calculate AIC
    meanAIC <-
      mean(
        sapply(list_modelsSameNumKnots, function(w){
          w[[5]]
        }
        )
      )
    
    # return RMSE and MAE
    results <- c('RMSE' = RMSE,
                 'MAE' = MAE,
                 'Mean R^2' = meanR2,
                 'Mean Exp. Dev.' = meanExpDev,
                 'Mean AIC' = meanAIC)
    gc()
    return(results)
  }




##### single-variable effect plots #####

# run models on full dataset
list_models <- list()
for(m in 1:5){
  list_models[[m]] <-
    gam(formula =
          forestPixelsLost ~
          s(numGrassPixelsBeforeFire, bs="tp", k = m) +
          s(numForestPixelsBeforeFire, bs="tp", k = m) +
          te(numGrassPixelsBeforeFire, numForestPixelsBeforeFire, k = m) +
          s(rainfall_mmMeanAnn, bs="tp", k = m) +
          s(fireRisk_75thPctileMax, bs="tp", k = m),
        data = dat_watershed,
        family = 'poisson')
}
names(list_models) <- paste('k =', 1:5)

# initiate plot data
list_plotdat <- list(dat_watershed, dat_watershed, dat_watershed,
                     dat_watershed)
names(list_plotdat) <-
  c('fireRisk_75thPctileMax', 'rainfall_mmMeanAnn',
    'numGrassPixelsBeforeFire', 'numForestPixelsBeforeFire')
list_plotdat$fireRisk_75thPctileMax$variable <- 'Fire risk'
list_plotdat$rainfall_mmMeanAnn$variable <- 'Rainfall'
list_plotdat$numGrassPixelsBeforeFire$variable <- 'Grassland area'
list_plotdat$numForestPixelsBeforeFire$variable <- 'Forest area'

# adjust column values accding to whether they're variable or control
  # Variable = sequence along range of values. Control = mean value
list_plotdat$fireRisk_75thPctileMax$fireRisk_75thPctileMax <-
  seq(quantile(dat_watershed$fireRisk_75thPctileMax, 0.10),
      quantile(dat_watershed$fireRisk_75thPctileMax, 0.90),
      length.out = nrow(dat_watershed)
      )
list_plotdat$fireRisk_75thPctileMax$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$fireRisk_75thPctileMax$numGrassPixelsBeforeFire <-
  mean(dat_watershed$numGrassPixelsBeforeFire)
list_plotdat$fireRisk_75thPctileMax$numForestPixelsBeforeFire <-
  mean(dat_watershed$numForestPixelsBeforeFire)
list_plotdat$fireRisk_75thPctileMax$numForestGrassPixels1999Interaction <-
  with(list_plotdat$fireRisk_75thPctileMax,
       numGrassPixelsBeforeFire * numForestPixelsBeforeFire)

list_plotdat$rainfall_mmMeanAnn$fireRisk_75thPctileMax <-
  mean(dat_watershed$fireRisk_75thPctileMax)
list_plotdat$rainfall_mmMeanAnn$rainfall_mmMeanAnn <-
  seq(quantile(dat_watershed$rainfall_mmMeanAnn, 0.10),
      quantile(dat_watershed$rainfall_mmMeanAnn, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$rainfall_mmMeanAnn$numGrassPixelsBeforeFire <-
  mean(dat_watershed$numGrassPixelsBeforeFire)
list_plotdat$rainfall_mmMeanAnn$numForestPixelsBeforeFire <-
  mean(dat_watershed$numForestPixelsBeforeFire)
list_plotdat$rainfall_mmMeanAnn$numForestGrassPixels1999Interaction <-
  with(list_plotdat$rainfall_mmMeanAnn,
       numGrassPixelsBeforeFire * numForestPixelsBeforeFire)

list_plotdat$numGrassPixelsBeforeFire$fireRisk_75thPctileMax <-
  mean(dat_watershed$fireRisk_75thPctileMax)
list_plotdat$numGrassPixelsBeforeFire$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$numGrassPixelsBeforeFire$numGrassPixelsBeforeFire <-
  seq(quantile(dat_watershed$numGrassPixelsBeforeFire, 0.10),
      quantile(dat_watershed$numGrassPixelsBeforeFire, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$numGrassPixelsBeforeFire$numForestPixelsBeforeFire <-
  mean(dat_watershed$numForestPixelsBeforeFire)
list_plotdat$numGrassPixelsBeforeFire$numForestGrassPixels1999Interaction <-
  with(list_plotdat$numGrassPixelsBeforeFire,
       numGrassPixelsBeforeFire * numForestPixelsBeforeFire)

list_plotdat$numForestPixelsBeforeFire$fireRisk_75thPctileMax <-
  mean(dat_watershed$fireRisk_75thPctileMax)
list_plotdat$numForestPixelsBeforeFire$rainfall_mmMeanAnn <-
  mean(dat_watershed$rainfall_mmMeanAnn)
list_plotdat$numForestPixelsBeforeFire$numGrassPixelsBeforeFire <-
  mean(dat_watershed$numGrassPixelsBeforeFire)
list_plotdat$numForestPixelsBeforeFire$numForestPixelsBeforeFire <-
  seq(quantile(dat_watershed$numForestPixelsBeforeFire, 0.10),
      quantile(dat_watershed$numForestPixelsBeforeFire, 0.90),
      length.out = nrow(dat_watershed)
  )
list_plotdat$numForestPixelsBeforeFire$numForestGrassPixels1999Interaction <-
  with(list_plotdat$numForestPixelsBeforeFire,
       numGrassPixelsBeforeFire * numForestPixelsBeforeFire)

# stack data and repeat: one dataset for each model
plotdat <- do.call(rbind, list_plotdat)
plotdat <- list(plotdat, plotdat, plotdat, plotdat, plotdat)
for(i in seq_along(plotdat)){
  plotdat[[i]]$k <- i
}
names(plotdat) <- paste0('k',1:5)

# get predicted values for each model
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval =
    predict(object = list_models[[i]],
            newdata = plotdat[[i]],
            type = 'response')
}

# get 95% CI values for each model
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval_lower95 =
    plotdat[[i]]$predval - 1.96 *
    predict(object = list_models[[i]],
            newdata = plotdat[[i]],
            type = 'response',
            se.fit = TRUE)$se.fit
}
for(i in seq_along(plotdat)){
  plotdat[[i]]$predval_upper95 =
    plotdat[[i]]$predval + 1.96 *
    predict(object = list_models[[i]],
            newdata = plotdat[[i]],
            type = 'response',
            se.fit = TRUE)$se.fit
}

# create final plotdat
plotdat_final <-
  data.frame(y = c(plotdat[[1]]$predval, plotdat[[2]]$predval, plotdat[[3]]$predval,
                   plotdat[[4]]$predval, plotdat[[5]]$predval),
             
             y_lower95 = c(plotdat[[1]]$predval_lower95, plotdat[[2]]$predval_lower95, plotdat[[3]]$predval_lower95,
                           plotdat[[4]]$predval_lower95, plotdat[[5]]$predval_lower95),
             
             y_upper95 = c(plotdat[[1]]$predval_upper95, plotdat[[2]]$predval_upper95, plotdat[[3]]$predval_upper95,
                           plotdat[[4]]$predval_upper95, plotdat[[5]]$predval_upper95),
             
             x = c(plotdat[[1]][plotdat[[1]]$variable == 'Fire risk',      'fireRisk_75thPctileMax'] * 100,
                   plotdat[[1]][plotdat[[1]]$variable == 'Forest area',    'numForestPixelsBeforeFire'],
                   plotdat[[1]][plotdat[[1]]$variable == 'Grassland area', 'numGrassPixelsBeforeFire'],
                   plotdat[[1]][plotdat[[1]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[2]][plotdat[[2]]$variable == 'Fire risk',      'fireRisk_75thPctileMax'] * 100,
                   plotdat[[2]][plotdat[[2]]$variable == 'Forest area',    'numForestPixelsBeforeFire'],
                   plotdat[[2]][plotdat[[2]]$variable == 'Grassland area', 'numGrassPixelsBeforeFire'],
                   plotdat[[2]][plotdat[[2]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[3]][plotdat[[3]]$variable == 'Fire risk',      'fireRisk_75thPctileMax'] * 100,
                   plotdat[[3]][plotdat[[3]]$variable == 'Forest area',    'numForestPixelsBeforeFire'],
                   plotdat[[3]][plotdat[[3]]$variable == 'Grassland area', 'numGrassPixelsBeforeFire'],
                   plotdat[[3]][plotdat[[3]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[4]][plotdat[[4]]$variable == 'Fire risk',      'fireRisk_75thPctileMax'] * 100,
                   plotdat[[4]][plotdat[[4]]$variable == 'Forest area',    'numForestPixelsBeforeFire'],
                   plotdat[[4]][plotdat[[4]]$variable == 'Grassland area', 'numGrassPixelsBeforeFire'],
                   plotdat[[4]][plotdat[[4]]$variable == 'Rainfall',       'rainfall_mmMeanAnn'],
                   
                   plotdat[[5]][plotdat[[5]]$variable == 'Fire risk',      'fireRisk_75thPctileMax'] * 100,
                   plotdat[[5]][plotdat[[5]]$variable == 'Forest area',    'numForestPixelsBeforeFire'],
                   plotdat[[5]][plotdat[[5]]$variable == 'Grassland area', 'numGrassPixelsBeforeFire'],
                   plotdat[[5]][plotdat[[5]]$variable == 'Rainfall',       'rainfall_mmMeanAnn']),
             
             var = c(rep(rep(c('Fire risk (annual % prob)', 'Forest area (pixels)', 'Grassland area (pixels)', 'Rainfall (mean mm/yr)'),
                             each = nrow(dat_watershed)),
                         times = length(plotdat)
                         )
                     ),
             
             k = rep(paste('k =', 1:5), each = nrow(plotdat[[1]])
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
  data.frame(numGrassPixelsBeforeFire =
               rep(seq(quantile(dat_watershed$numGrassPixelsBeforeFire, 0.10),
                       quantile(dat_watershed$numGrassPixelsBeforeFire, 0.90),
                       length.out = nobs),
                   times = 3
                   ),
             numForestPixelsBeforeFire =
               c(rep(quantile(dat_watershed$numForestPixelsBeforeFire, 0.25), times = nobs),
                 rep(quantile(dat_watershed$numForestPixelsBeforeFire, 0.50), times = nobs),
                 rep(quantile(dat_watershed$numForestPixelsBeforeFire, 0.75), times = nobs)
                 ),
             forestSizeQuartile = 
               rep(c('Forest size quartile = 25%',
                     'Forest size quartile = 50%',
                     'Forest size quartile = 75%'),
                   each = nobs),
             rainfall_mmMeanAnn =
               mean(dat_watershed$rainfall_mmMeanAnn),
             fireRisk_75thPctileMax =
               mean(dat_watershed$fireRisk_75thPctileMax)
             )
plotdat_forestQuantiles <-
  rbind(plotdat_forestQuantiles, plotdat_forestQuantiles)
plotdat_forestQuantiles$k <-
  as.character(rep(c(2, 4), each = nobs))

# generate predicted values and 95% CI
plotdat_forestQuantiles$forestPixelsLost_predicted <-
  c(predict(object = list_models$`k = 2`,
            newdata = plotdat_forestQuantiles[1:(nobs*3),],
            type = 'response'),
    predict(object = list_models$`k = 4`,
            newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),],
            type = 'response')
    )
plotdat_forestQuantiles$forestPixelsLost_predicted_lower95 <-
  c(plotdat_forestQuantiles$forestPixelsLost_predicted[1:(nobs*3)] -
      predict(object = list_models$`k = 2`,
              newdata = plotdat_forestQuantiles[1:(nobs*3),],
              type = 'response', se = TRUE)$se.fit * 1.96,
    plotdat_forestQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_forestQuantiles)] -
      predict(object = list_models$`k = 4`,
              newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),],
              type = 'response', se = TRUE)$se.fit * 1.96)
plotdat_forestQuantiles$forestPixelsLost_predicted_upper95 <-
  c(plotdat_forestQuantiles$forestPixelsLost_predicted[1:(nobs*3)] +
      predict(object = list_models$`k = 2`,
              newdata = plotdat_forestQuantiles[1:(nobs*3),],
              type = 'response', se = TRUE)$se.fit * 1.96,
    plotdat_forestQuantiles$forestPixelsLost_predicted[((nobs*3)+1):nrow(plotdat_forestQuantiles)] +
      predict(object = list_models$`k = 4`,
              newdata = plotdat_forestQuantiles[((nobs*3)+1):nrow(plotdat_forestQuantiles),],
              type = 'response', se = TRUE)$se.fit * 1.96)

# create plot
ggplot(data = plotdat_forestQuantiles) +
  geom_ribbon(aes(x = numGrassPixelsBeforeFire,
                  ymin = forestPixelsLost_predicted_lower95,
                  ymax = forestPixelsLost_predicted_upper95,
                  fill = k),
              alpha = 0.4) +
  geom_line(aes(x = numGrassPixelsBeforeFire, y = forestPixelsLost_predicted,
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
  data.frame(numForestPixelsBeforeFire =
               rep(seq(quantile(dat_watershed$numForestPixelsBeforeFire, 0.10),
                       quantile(dat_watershed$numForestPixelsBeforeFire, 0.90),
                       length.out = nobs),
                   times = 3
               ),
             numGrassPixelsBeforeFire =
               c(rep(quantile(dat_watershed$numGrassPixelsBeforeFire, 0.25), times = nobs),
                 rep(quantile(dat_watershed$numGrassPixelsBeforeFire, 0.50), times = nobs),
                 rep(quantile(dat_watershed$numGrassPixelsBeforeFire, 0.75), times = nobs)
               ),
             forestSizeQuartile = 
               rep(c('Grass size quartile = 25%',
                     'Grass size quartile = 50%',
                     'Grass size quartile = 75%'),
                   each = nobs),
             rainfall_mmMeanAnn =
               mean(dat_watershed$rainfall_mmMeanAnn),
             fireRisk_75thPctileMax =
               mean(dat_watershed$fireRisk_75thPctileMax)
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
  geom_ribbon(aes(x = numForestPixelsBeforeFire,
                  ymin = forestPixelsLost_predicted_lower95,
                  ymax = forestPixelsLost_predicted_upper95,
                  fill = k),
              alpha = 0.4) +
  geom_line(aes(x = numForestPixelsBeforeFire, y = forestPixelsLost_predicted,
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
               rep(dat_watershed$forestPixelsLost, times = 4),
             var = with(dat_watershed,
                        c(fireRisk_75thPctileMax,
                          numForestPixelsBeforeFire,
                          numGrassPixelsBeforeFire,
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
dat_watershed$forestPixelsLost_predictedK2 <-
  predict(object = gam(formula =
                         forestPixelsLost ~
                         s(numGrassPixelsBeforeFire, bs="tp", k = 2) +
                         s(numForestPixelsBeforeFire, bs="tp", k = 2) +
                         te(numGrassPixelsBeforeFire, numForestPixelsBeforeFire, k = 2) +
                         s(rainfall_mmMeanAnn, bs="tp", k = 2) +
                         s(fireRisk_75thPctileMax, bs="tp", k = 2),
                       data = dat_watershed,
                       family = 'poisson'),
          newdata = dat_watershed, type = 'response')
dat_watershed$forestPixelsLost_predictedK4 <-
  predict(object = gam(formula =
                         forestPixelsLost ~
                         s(numGrassPixelsBeforeFire, bs="tp", k = 4) +
                         s(numForestPixelsBeforeFire, bs="tp", k = 4) +
                         te(numGrassPixelsBeforeFire, numForestPixelsBeforeFire, k = 4) +
                         s(rainfall_mmMeanAnn, bs="tp", k = 4) +
                         s(fireRisk_75thPctileMax, bs="tp", k = 4),
                       data = dat_watershed,
                       family = 'poisson'),
          newdata = dat_watershed, type = 'response')

# get forest size lost quartiles
vec_forestSizeLost_k2 <-
  quantile(dat_watershed$forestPixelsLost_predictedK2,
           probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
vec_forestSizeLost_k4 <-
  quantile(dat_watershed$forestPixelsLost_predictedK4,
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





##### preliminary fire risk change effects #####

