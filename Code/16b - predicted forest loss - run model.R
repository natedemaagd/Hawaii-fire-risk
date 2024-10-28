
# This script runs the forest loss model using data generated from
# 16a V1 (temporal) and 16a V2 (aggregated) data.

library(ggplot2); library(mgcv)


# load data - non-temporal
dat_watershed <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                 'dat_watershed.rds'))




##### analyze data #####

# # is relationship between fire size and proportion of grass-dominated land linear?
# ggplot(data = dat_watershed,
#        aes(x = proportion_grassDom, y = forestLoss_totalBurnedPixels)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = 'loess', se = TRUE) +
#   labs(x = 'Proportion of grass-dominated land w/in watershed',
#        y = 'Total burned pixels') +
#   theme(text = element_text(size = 15))




##### run models #####

# run linear model - non-temporal
gamPoisson <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        proportion_grassDom + rainfall_mmMeanAnn + fireRisk_75thPctileMaxHistorical,
      data = dat_watershed,
      family = 'poisson')
gamGaussian <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        proportion_grassDom + rainfall_mmMeanAnn + fireRisk_75thPctileMaxHistorical,
      data = dat_watershed,
      family = 'gaussian')

# run non-linear model - non-temporal
gamPoisson_nonlinear <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        s(proportion_grassDom, bs="tp", k=5) +
        s(rainfall_mmMeanAnn, bs="tp", k=5) +
        s(fireRisk_75thPctileMaxHistorical, bs="tp", k=5),
      data = dat_watershed,
      family = 'poisson')
gamGaussian_nonlinear <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        s(proportion_grassDom, bs="tp", k=5) +
        s(rainfall_mmMeanAnn, bs="tp", k=5) +
        s(fireRisk_75thPctileMaxHistorical, bs="tp", k=5),
      data = dat_watershed,
      family = 'gaussian')

# run non-linear model w/ interaction - non-temporal
gamPoisson_nonlinear_interaction <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        s(numGrassPixels1999, bs="tp", k=5) +
        s(numForestPixels1999, bs="tp", k=5) +
        te(numGrassPixels1999, numForestPixels1999) +
        s(rainfall_mmMeanAnn, bs="tp", k=5) +
        s(fireRisk_75thPctileMaxHistorical, bs="tp", k=5),
      data = dat_watershed,
      family = 'poisson')






##### analyze explained deviance - non-temporal #####

summary(gamGaussian)$dev.expl*100
summary(gamGaussian_nonlinear)$dev.expl*100
summary(gamPoisson)$dev.expl*100
summary(gamPoisson_nonlinear)$dev.expl*100




##### analyze predicted values - non-temporal #####

# create data.frame of predicted and actual values
dat_predVals <-
  data.frame(value =
               c(dat_watershed$forestLoss_totalBurnedPixels,
                 gamGaussian$fitted.values,
                 gamGaussian_nonlinear$fitted.values,
                 gamPoisson$fitted.values,
                 gamPoisson_nonlinear$fitted.values),
             model =
               rep(c('Actual',
                     'Gaussian - Linear',
                     'Gaussian = Nonlinear',
                     'Poisson - Linear',
                     'Poisson - Nonlinear'),
                   each = nrow(dat_watershed)
                   ),
             fireRisk_75thPctileMaxHistorical =
               dat_watershed$fireRisk_75thPctileMaxHistorical
             )
dat_predVals$model <-
  factor(dat_predVals$model,
         levels = c('Actual',
                    'Gaussian - Linear',
                    'Gaussian = Nonlinear',
                    'Poisson - Linear',
                    'Poisson - Nonlinear')
         )

# plot
ggplot(data = dat_predVals,
       aes(x = fireRisk_75thPctileMaxHistorical,
           y = value,
           color = model)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 800))




##### analyze residuals - non-temporal #####

# create dataframe
dat_residsNonTemporal <-
  data.frame(resids = c(gamGaussian$residuals,
                        gamGaussian_nonlinear$residuals,
                        gamPoisson$residuals,
                        gamPoisson_nonlinear$residuals,
                        rnorm(n = nrow(dat_watershed))),
             model = rep(c('Gaussian - Linear', 'Gaussian - Nonlinear',
                           'Poisson - Linear', 'Poisson - Nonlinear',
                           'Normal'),
                         each = nrow(dat_watershed)
                         )
             )
dat_residsNonTemporal$model <-
  factor(dat_residsNonTemporal$model,
         levels = c('Gaussian - Linear', 'Gaussian - Nonlinear',
                    'Poisson - Linear', 'Poisson - Nonlinear',
                    'Normal')
         )

# shapiro-wilk test
shapiro.test(dat_residsNonTemporal$resids[dat_residsNonTemporal$model == 'Gaussian - Linear'])
shapiro.test(dat_residsNonTemporal$resids[dat_residsNonTemporal$model == 'Gaussian - Nonlinear'])
shapiro.test(dat_residsNonTemporal$resids[dat_residsNonTemporal$model == 'Poisson - Linear'])
shapiro.test(dat_residsNonTemporal$resids[dat_residsNonTemporal$model == 'Poisson - Nonlinear'])
shapiro.test(dat_residsNonTemporal$resids[dat_residsNonTemporal$model == 'Normal'])

# sum of squared residuals
aggregate(dat_residsNonTemporal$resids,
          list(dat_residsNonTemporal$model),
          function(r){
            sum(r^2)
          })

# mean and median absolute residual
aggregate(dat_residsNonTemporal$resids,
          list(dat_residsNonTemporal$model),
          function(r){
            c(mean(abs(r)),
              median(abs(r)))
          })

rm(gamGaussian, gamGaussian_nonlinear, gamPoisson)




##### Non-linear Poisson - non-temporal, variable selection #####

# compare dropping proportion grass pixels and dropping fire risk percentile
gamPoisson_nonlinear_noGrassProportion <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        s(rainfall_mmMeanAnn, bs="tp", k=5) +
        s(fireRisk_75thPctileMaxHistorical, bs="tp", k=5),
      data = dat_watershed,
      family = 'poisson')
gamPoisson_nonlinear_noFireRisk <-
  gam(formula =
        forestLoss_totalBurnedPixels ~
        s(proportion_grassDom, bs="tp", k=5) +
        s(rainfall_mmMeanAnn, bs="tp", k=5),
      data = dat_watershed,
      family = 'poisson')




##### sample forest loss size for each watershed #####

# 
  