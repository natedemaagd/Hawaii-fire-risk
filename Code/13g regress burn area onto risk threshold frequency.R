
# this script creates scatter plots and regresses (burn area) ~ (fire risk threshold frequency)

library(raster); library(sf); library(ggplot2); library(tidyverse);
library(cowplot); library(grid); library(gridExtra)




##### data reading and initial formatting #####

# define islands and risk levels
vec_islands <- c('Hawaii', 'Oahu', 'Kauai',  'MauiCounty')
vec_riskLevels <- c('low', 'moderate', 'high')

# load burn data
dat_burnPerimeters <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")

# load risk level frequency data.frame from script 13f
dat_totalFreqCount <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/13f dat_totalFreqCount.rds")

# separate fires by island
list_islandExtents <- list(  # define island extents
  extent(-156.2, -154.8, 18.8, 20.28),  # BI
  extent(-158.4, -157.5,  21.2, 21.8),  # Oahu
  extent(-159.9, -159.2 , 21.8, 22.3),  # Kauai
  extent(-157.5, -155.9, 20.45, 21.3)   # Maui Nui
)

list_burnPerimeters_byIsland <- list()
for(i in 1:length(vec_islands)){
  list_burnPerimeters_byIsland[[i]] <- st_crop(dat_burnPerimeters,
                                               list_islandExtents[[i]])
}
names(list_burnPerimeters_byIsland) <- vec_islands

gc()




##### for each island, get number of acres burned each year #####

# create data.frame from each island's burn data
list_burnData_byIsland <- list()
for(i in 1:length(vec_islands)){
  list_burnData_byIsland[[i]] <-
    with(list_burnPerimeters_byIsland[[i]],
         data.frame(yearmonth = as.Date(paste(Year, Month, '1', sep = '-')),
                    island    = vec_islands[[i]],
                    burnArea_acres = Sat_sz_ac)
    )
}

# aggregate burn data by year-month
for(i in 1:length(vec_islands)){
  list_burnData_byIsland[[i]] <-
    with(list_burnData_byIsland[[i]], aggregate(burnArea_acres,
                                                list(yearmonth),
                                                sum
    )
    )
  list_burnData_byIsland[[i]]$island <- vec_islands[[i]]
  colnames(list_burnData_byIsland[[i]]) <-
    c('yearmonth', 'burnArea_acres', 'island')
}
names(list_burnData_byIsland) <- vec_islands

# combine data into single data.frame
dat_burnData <- do.call(rbind, list_burnData_byIsland)

rm(dat_burnPerimeters, list_burnData_byIsland, list_burnPerimeters_byIsland,
   list_islandExtents, i)

gc()




##### merge two data.frames together #####

# format columns for merging
dat_totalFreqCount$yearmonth <- with(dat_totalFreqCount,
                                     as.Date(paste0(substr(yearmonth, 1, 4),
                                                    '-',
                                                    substr(yearmonth, 5, 6),
                                                    '-01')
                                             )
                                     )
# merge data
dat <- left_join(dat_totalFreqCount, dat_burnData, c('island', 'yearmonth'))




##### plot data #####

# create labels for islands/risks corresponding to existing vecs
vec_islandsLabels <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')
vec_riskLevelsLabels <- c('Low', 'Moderate', 'High')

# for each level of risk and each island, create an individual scatter plot
plotList <- list()
for(i in 1:length(vec_islands)){
  plotList[[i]] <- list()
  for(r in 1:length(vec_riskLevels)){
    
    # get data for island i and risk level r
    plotdat <- dat[dat$island == vec_islands[[i]] & dat$riskLevel == vec_riskLevels[[r]],]
    plotdat <- plotdat[!is.na(plotdat$burnArea_acres),]  # remove rows (months) with no burns
    
    # plot the data
    plotList[[i]][[r]] <-
      ggplot(data = plotdat,
             aes(x = sumAcreage, y = burnArea_acres)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = 'lm')  +
      theme(axis.ticks = element_blank(), axis.text = element_blank(),
            axis.title = element_blank()) +
      labs(title = paste0(vec_islandsLabels[[i]], ', ', vec_riskLevelsLabels[[r]], ' Risk'),
           subtitle = paste0('Cor = ', round(cor(plotdat$sumAcreage, plotdat$burnArea_acres), 3)))
    
  }
}
plotList <- unlist(plotList, recursive = FALSE)

gplot <- plot_grid(plotlist = plotList, ncol = 3)  # create initial plot
y.grob <- textGrob("Aggregate monthly acreage burned", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)  # create common x and y axis titles
x.grob <- textGrob("Aggregate monthly acreage at risk", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

gplot <- grid.arrange(arrangeGrob(gplot, left = y.grob, bottom = x.grob))

ggsave(gplot, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/13g scatterplots - aggregate monthly acreage burned onto aggregate monthly acreage at risk.png',
       dpi = 300, height = 8, width = 8)
