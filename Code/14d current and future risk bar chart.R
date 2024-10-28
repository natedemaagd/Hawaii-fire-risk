
# This script creates a bar chart of the current and future hectares at risk

library(ggplot2)




##### load data #####

# load current risk data
list_riskCurrentNames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total current area at risk',
             pattern = '.csv', full.names = TRUE)
list_riskCurrent <- lapply(list_riskCurrentNames, read.csv)

# load future risk
list_riskFutureNames <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Total future area at risk',
             pattern = 'Annual.csv', full.names = TRUE)
list_riskFutureNames <- list_riskFutureNames[grep('rcp85', list_riskFutureNames)]  # keep only RCP 8.5
list_riskFutureNames <- list_riskFutureNames[grep('Max', list_riskFutureNames)]    # keep only max (and not median)
list_riskFuture <- lapply(list_riskFutureNames, read.csv)
list_riskFuture <- lapply(list_riskFuture, function(df){
  colnames(df) <- colnames(list_riskCurrent[[1]])
  df
})




##### format data #####

# add island name and model to current climate data
vec_islandCurrent <- strsplit(list_riskCurrentNames, ' ')
vec_islandCurrent <- lapply(vec_islandCurrent, function(x){
  str <- x[[length(x)]]
  substr(str, 1, nchar(str) - 4)
})
vec_islandCurrent <- do.call(c, vec_islandCurrent)
vec_islandCurrent[vec_islandCurrent == 'MauiCounty'] <- 'Maui County'
for(i in 1:length(list_riskCurrent)){
  list_riskCurrent[[i]]$island <- vec_islandCurrent[[i]]
  list_riskCurrent[[i]]$model <- 'Current'
}
rm(i, list_riskCurrentNames, vec_islandCurrent); gc()

# add island to future climate data
vec_islandFuture <- strsplit(list_riskFutureNames, '/')
vec_islandFuture <- sapply(vec_islandFuture, function(x) x[[length(x)]])
vec_islandFuture <- strsplit(vec_islandFuture, ' ')
vec_islandFuture <- sapply(vec_islandFuture, function(x) x[[2]])
vec_islandFuture[vec_islandFuture == 'MauiCounty'] <- 'Maui County'
for(i in 1:length(list_riskFuture)){
  list_riskFuture[[i]]$island <- vec_islandFuture[[i]]
}
rm(vec_islandFuture, i); gc()

# add model name to future climate data
vec_modelFuture <- strsplit(list_riskFutureNames, '/')
vec_modelFuture <- sapply(vec_modelFuture, function(x) x[[length(x)]])
vec_modelFuture <- strsplit(vec_modelFuture, ' ')
vec_modelFuture <- lapply(vec_modelFuture, function(x) x[3:5])
vec_modelFuture <- lapply(vec_modelFuture,function(x){
  stringr::str_replace_all(x, c(dyn = 'Dyn', sta = 'Sta',
                                rcp45 = 'RCP 4.5', rcp85 = 'RCP 8.5',
                                `2070` = 'Mid', `2100` = 'Late'))
  })
vec_modelFuture <- lapply(vec_modelFuture, function(x) paste(x, collapse = ' '))
vec_modelFuture <- lapply(vec_modelFuture, function(x) gsub('RCP 8.5 ', '', x))
for(i in 1:length(list_riskFuture)){
  list_riskFuture[[i]]$model <- vec_modelFuture[[i]]
}
rm(vec_modelFuture, i, list_riskFutureNames); gc()

# combine into one data.frame
dat_current <- do.call(rbind, list_riskCurrent)
dat_future <- do.call(rbind, list_riskFuture)
dat <- rbind(dat_current, dat_future)
rm(dat_current, dat_future, list_riskCurrent, list_riskFuture); gc()

dat <- dat[dat$Risk.level != 'Low',]  # remove low-risk count




##### plot #####

# create factor levels
dat$Risk.level <- factor(dat$Risk.level, levels = rev(c('Moderate', 'High', 'Very high')))
dat$model <- factor(dat$model, levels = c('Current', 'Sta Mid', 'Sta Late', 'Dyn Late'))
dat$island <- factor(dat$island, levels = c('Kauai', 'Oahu', 'Maui County', 'Hawaii'))

ggplot(data = dat,
       aes(x = model, y = hectares/1000, fill = Risk.level)) +
  geom_col() +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(vars(island), scales = 'free', ncol = length(unique(dat$island))/2) +
  labs(x = 'Model', y = '1000s ha', fill = 'Risk') +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/',
                         '14d - current and future risk hectare barchart.png'),
       dpi = 300, width = 9, height = 10, units = 'in')
