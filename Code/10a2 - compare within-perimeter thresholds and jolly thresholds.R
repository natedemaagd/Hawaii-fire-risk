
# This script compares our own thresholds (within-fire quartiles) with the
# thresholds from Jolly et al.

library(ggplot2)




##### load data #####

# load our data
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/10_determine high risk threshold.Rdata')

# load Jolly data
# vec_allFireProbs <-
#   readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/10 - fire risk thresholds/',
#                  '10a - vec of all historical pixel-level fire probabilities.rds'))
dat_thresholdsJolly <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Tables/Fire/',
                 '10a fire risk thresholds - Jolly et al percentiles.rds'))
vec_thresholdsJolly_rownames <- rownames(dat_thresholdsJolly)
vec_thresholdsJolly_rownames <- vec_thresholdsJolly_rownames[!(rownames(dat_thresholdsJolly) %in% c('0%', '100%'))]
dat_thresholdsJolly <- data.frame(vec_percentiles = dat_thresholdsJolly[!(rownames(dat_thresholdsJolly) %in% c('0%', '100%')),])

dat_thresholdsOurs <- highRiskThresholds
rm(highRiskThresholds)
gc()




##### format threshold data and plot #####

# combine threshold types for plotting
plotdat_thresholds <-
  data.frame(value = unlist(c(dat_thresholdsOurs[dat_thresholdsOurs$island == 'Statewide', 2:4],
                              dat_thresholdsJolly$vec_percentiles)),
             source = c(rep('Within-fire quartiles', times = 3),
                        rep('Jolly et al.', times = nrow(dat_thresholdsJolly))))

# create plot
ggplot() +
  geom_density(data = dat_fire2,
               aes(x = prob_burned, fill = status, color = status),
               alpha = 0.8) +
  geom_vline(data = plotdat_thresholds, aes(xintercept = value, linetype = source)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(0, 0.25/4)) +
  labs(x = 'Pixel-level fire probability', y = 'Pixel density') +
  guides(linetype = guide_legend('Risk threshold\nsource'),
         color = guide_legend('Status'),
         fill = guide_legend('Status')) +
  theme(text = element_text(size = 20))
