
library(raster); library(ggplot2); library(stringr); library(tidyverse)
library(gtable); library(grid); library(cowplot)

# list raster filenames
filenames_rasters <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_wetDryScenarios/',  pattern = '.tif')

# define islands and scenarios
islands <- c('Oahu', 'Kauai', 'Maui County', 'Hawaii')
scenarios <- c('Dry-dry',  'Dry-wet', 'Wet-dry', 'Wet-wet')

# for each island, create mean wet-dry scenario
dat_Freq <- list()
for(i in 1:length(islands)){
  
  
  
  
  ##### load rasters for all scenarios in island i
  
  # list all filenames for island i
  filenames_rasters_island_i <- filenames_rasters[str_detect(filenames_rasters, gsub(' ', '', islands[[i]]))]  # subset filenames by island
  
  # read filenames
  rasters_fireProb <- lapply(filenames_rasters_island_i, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_wetDryScenarios/', r)))
  names(rasters_fireProb) <- rep(scenarios, times = length(rasters_fireProb)/4)
  gc()
  
  
  
  
  ##### take mean of each scenario (monthly --> monthly average) #####
  
  # split raster list by name
  datByScenario <- list()
  plots <- list()
  for(s in 1:length(scenarios)){
    
    # get all rasters for scenario s
    rasters_fireProb_scenarioS <- rasters_fireProb[names(rasters_fireProb) == scenarios[[s]]]
    
    # cbind all values of scenario s into a matrix
    dat_scenarioS <- lapply(rasters_fireProb_scenarioS, function(r){
      values(r)[!is.na(values(r))]
    })
    dat_scenarioS <- do.call(cbind, dat_scenarioS)
    
    # get rowMeans (annual mean) of all data
    dat_scenarioS <- rowMeans(dat_scenarioS)
    
    # add annual mean data to coordinates of values, along with scenario name, to finish data.frame for island i
    dat_scenarioS_coords <-
      as.data.frame(rasters_fireProb_scenarioS[[1]], xy = TRUE)[,c('x', 'y')]
    dat_scenarioS_coords <-
      dat_scenarioS_coords[!is.na(values(rasters_fireProb_scenarioS[[1]])),]
    dat_scenarioS <- cbind(dat_scenarioS_coords, dat_scenarioS)
    colnames(dat_scenarioS)[[3]] <- 'value'
    dat_scenarioS$scenario <- scenarios[[s]]
    
    # add final data.frame to master data for island i
    datByScenario[[s]] <- dat_scenarioS
    
    rm(dat_scenarioS, dat_scenarioS_coords)
    gc()
  }
  
  # rowbind data for all scenarios
  dat_fireProbAndRisk <- do.call(rbind, datByScenario)
  rm(datByScenario, rasters_fireProb_scenarioS, rasters_fireProb,
     filenames_rasters_island_i, s)
  gc()
  
  # create more explicit scenario variables
  dat_fireProbAndRisk <- dat_fireProbAndRisk %>%
    mutate(scenario_shortTerm = substr(scenario, 1, 3),
           scenario_longTerm  = R.utils::capitalize(substr(scenario, 5, 7)),
           .after = scenario)
  dat_fireProbAndRisk$scenario <- NULL; gc()
  
  # save data to list for tabulation
  dat_Freq[[i]] <- dat_fireProbAndRisk
  
  
  
  
  ##### assign risk level based on thresholds #####
  
  # load thresholds
  load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/10_determine high risk threshold.Rdata")
  rm(dat_fire,  dat_fire2)
  
  # create risk level variable
  vec_riskLevel <- unlist(highRiskThresholds[highRiskThresholds$island == gsub(' ', '', islands[[i]]),
                                             c("hiRiskThresh_25pctileBurned", "hiRiskThresh_50pctileBurned", "hiRiskThresh_75pctileBurned")])
  dat_fireProbAndRisk$riskLevel <- with(dat_fireProbAndRisk, ifelse(value  < vec_riskLevel[[1]],                              'Low',
                                                                    ifelse(value >= vec_riskLevel[[1]] & value < vec_riskLevel[[2]], 'Moderate',
                                                                           ifelse(value >= vec_riskLevel[[2]] & value < vec_riskLevel[[3]], 'High',
                                                                                  ifelse(value >= vec_riskLevel[[3]],                              'Very high', NA)))))
  dat_fireProbAndRisk$riskLevel <- factor(dat_fireProbAndRisk$riskLevel, levels = c('Very high', 'High', 'Moderate', 'Low'))
  
  
  
  
  ##### create plot #####
  
  # initiate plot
  gplot <- ggplot(data = dat_fireProbAndRisk, aes(x = x, y = y, fill = riskLevel, color = riskLevel)) +
    geom_raster() +
    coord_equal() +
    facet_grid(vars(scenario_shortTerm), vars(scenario_longTerm), switch = 'y') +
    scale_fill_manual(values = c(rev(viridis::viridis(3)), 'lightgray')) +
    labs(fill = 'Fire risk') +
    theme(text = element_text(size = 30), axis.title = element_blank(), panel.background = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())
  
  ### next steps add facet titles ('short-term' and 'long-term')
  
  # convert to grob
  gplot <- ggplotGrob(gplot)
  
  # get the positions of the facet strips in the layout: t = top, l = left, ...
  strip <- c(subset(gplot$layout, grepl("strip-l", gplot$layout$name)))
  
  # new column to the right of current strip
  # adjust the width to suit
  gplot <- gtable_add_cols(gplot, unit(3, "lines"), pos = 0) 
  
  # add text grob to new column; adjust cex (i.e., size) to suit
  gplot <- gtable_add_grob(gplot,
                           textGrob("Short term", rot = 90,
                                    gp = gpar(cex = 2, col = "black")),
                           t = min(strip$t), l = 1, b = max(strip$b))
  
  # repeat for column title (long-term)
  strip <- c(subset(gplot$layout, grepl("strip-t", gplot$layout$name)))
  gplot <- gtable_add_rows(gplot, unit(3, "lines"), pos = 0)
  gplot <- gtable_add_grob(gplot,
                           textGrob("Long term", rot = 0,
                                    gp = gpar(cex = 2, col = "black")),
                           t = 1, l = min(strip$l), r = max(strip$r))
  
  grid.newpage()
  grid.draw(gplot)
  
  ggsave(gplot, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Wet-dry scenarios/wetDryScenarios_',
                           islands[[i]], '.png'),
         dpi = 300, height = 8, width = 8)
  
  plots[[i]] <- gplot
  
  rm(i, dat_fireProbAndRisk, gplot, highRiskThresholds, strip, vec_riskLevel)
  gc()
  
}


