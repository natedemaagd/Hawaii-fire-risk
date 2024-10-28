library(data.table); library(mgcv); library(ggplot2); library(e1071)
#bigextfinal<-readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/bigextfinal.rds")
bigextfinal<-fread( "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84_OVERWRITE.csv")

head(bigextfinal)

#CHECK NAMES
names(bigextfinal)
#names(bigextfinal)[9]<-"county"
names(bigextfinal)[which(names(bigextfinal) == 'HI_EVAP_mean_annual_temp_statewide_250m')]<-"mean_annual_temp"
names(bigextfinal)[which(names(bigextfinal) == 'HI_EVAP_mean_annual_rainfall__statewide_250m')]<-"mean_annual_rainfall"
names(bigextfinal)[which(names(bigextfinal) == 'rf_0mo_prior')]<-"monthly_rf_0_mo_prior"

tail(bigextfinal, 100)


#COUNTY CUSTOM MODELS - subsets
unique(bigextfinal$county)
cut<-bigextfinal[bigextfinal$year<2017,] #remove years w/o missing land cover

oadat<-cut[cut$county=="oahu",]
nrow(oadat[oadat$burnt==1,])
mndat<-cut[cut$county=="mauinui",]
nrow(mndat[mndat$burnt==1,])
bidat<-cut[cut$county=="bigisland", ]
nrow(bidat[bidat$burnt==1,])
kadat<-cut[cut$county=="kauai", ]
nrow(kadat[kadat$burnt==1,])
kaoadat<-cut[which(cut$county=="kauai" | cut$county=="oahu"), ]
nrow(kaoadat[kaoadat$burnt==1,])
unique(kaoadat$county)

head(oadat , 100)


#
#MODEL 1.3.1 - VEGETATION-RAINFALL INTERACTION

#m1.3.1<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
#                       s(woodcov_yearof, bs="tp", k=5)+
#                       s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
#                       s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
#                       s(rf_1mo_prior, bs="tp", k=5)+
#                       s(monthly_cumrf_3mo, bs="tp", k=5)+
#                       s(monthly_cumrf_12mo, bs="tp", k=5)+
#                       te(herbcov_yearof,rf_1mo_prior)+
#                       te(herbcov_yearof,monthly_cumrf_3mo)+
#                       te(herbcov_yearof,monthly_cumrf_12mo)+
#                       s(year, bs="re",by=dum),data=cut, family=binomial)



#Predictors to loop (30 total)
predictornames<-c("herbcov_yearof", "woodcov_yearof", 'rf_1mo_prior',
                  "mean_annual_rainfall", "HI_EVAP_min_monthly_soil_mst", 
                  "mean_annual_temp", "ign_trns", "monthly_cumrf_18mo",
                  "monthly_cumrf_12mo", "monthly_cumrf_3mo", "monthly_rf_0_mo_prior")

predictornames_full<-c("herbcov_yearof", "woodcov_yearof", "barecov_yearof", 
                  "mean_annual_rainfall", "HI_EVAP_min_monthly_soil_mst", 
                  "mean_annual_temp", "ign_trns","HI_DEM_10m" , "X2020_Hawaii_population_GHS_global_250m" , "mean_annual_rainfall", "monthly_cumrf_18mo",
                  "monthly_cumrf_12mo", "monthly_cumrf_3mo", "monthly_rf_0_mo_prior",
                  "aspect" ,"roughness")

#county data cuts
datacuts<-c("bidat", "mndat", "oadat","kadat", "kaoadat" )

#Single-term effects models as glms and gams
#1-11: big island
#12-22: maui nui
#24-33: oahu 
#34-44: kauai
#45-55: kauai oahu combined

glmlist<-list()
gamlist<-list()

for(h in 1:5){
  dat<-get(datacuts[h])
  for(i in 1:length(predictornames)){
    gamform<-as.formula(paste("burnt ~ s(",predictornames[i], ")" ))
    glmform<-as.formula(paste("burnt ~ ",predictornames[i]))
    gamlist[[length(predictornames)*(h-1)+i]] <- gam(formula=gamform, data=dat, family=binomial)
    glmlist[[length(predictornames)*(h-1)+i]] <- glm(formula=glmform, data=dat, family=binomial)
         }
}


#Run single-term GAM & GLM model predictions over min(var):max(var), N=500

#prediction data.frame
bigpredict<-data.frame(datacut=c(rep(datacuts[1], 500), 
                                 rep(datacuts[2], 500), 
                                 rep(datacuts[3], 500),
                                 rep(datacuts[4], 500), 
                                 rep(datacuts[5], 500)))

for(j in 1:5){
  dat<-get(datacuts[j])
  for (k in 1:length(predictornames)){
    colnum<-grep(predictornames[k], names(dat))
    predictdf<-data.frame( var1=seq(min(na.omit(as.data.frame(dat)[,colnum])),max(na.omit(as.data.frame(dat)[,colnum])), length.out=500)) 
    names(predictdf)<-predictornames[k]
    gamprediction<-predict(gamlist[[length(predictornames)*(j-1)+k]], predictdf, se.fit=TRUE, type="response")
    predictdf$gampred<-gamprediction[[1]]
    predictdf$gamse<-gamprediction[[2]]
    glmprediction<-predict(glmlist[[length(predictornames)*(j-1)+k]], predictdf, se.fit=TRUE, type="response")
    predictdf$glmpred<-glmprediction[[1]]
    predictdf$glmse<-glmprediction[[2]]
    names(predictdf)<-c(predictornames[k], paste0(predictornames[k],"_gampred"), paste0(predictornames[k],"_gamse"), paste0(predictornames[k],"_glmpred"), paste0(predictornames[k],"_glmse"))
    bigpredict[(500*j-499):(500*j),(1+k*5-4):(1+k*5)]<-predictdf
       }
}

head(bigpredict)

#Capture explained deviance of each model pair

predictor_devs<-data.frame(datacut=c(rep(datacuts[1], length(predictornames)), 
                                     rep(datacuts[2], length(predictornames)), 
                                     rep(datacuts[3], length(predictornames)), 
                                     rep(datacuts[4], length(predictornames)),
                                     rep(datacuts[5], length(predictornames))),
                                    predictor=rep(predictornames, 5))

for(l in 1:length(gamlist)){
  predictor_devs[l,3]<-deviance(gamlist[[l]])
  predictor_devs[l,4]<-deviance(glmlist[[l]])
}

names(predictor_devs)[3:4]<-c("gamdevs", "glmdevs")
predictor_devs$nulldevs <- NA
predictor_devs$nulldevs[1:11]<-deviance(glm(burnt~1, data=bidat, family=binomial))
predictor_devs$nulldevs[12:22]<-deviance(glm(burnt~1, data=mndat, family=binomial))
predictor_devs$nulldevs[23:33]<-deviance(glm(burnt~1, data=oadat, family=binomial))
predictor_devs$nulldevs[34:44]<-deviance(glm(burnt~1, data=kadat, family=binomial))
predictor_devs$nulldevs[45:55]<-deviance(glm(burnt~1, data=kaoadat, family=binomial))
predictor_devs$expldev_gam<-1-(predictor_devs$gamdevs/predictor_devs$nulldevs)
predictor_devs$expldev_glm<-1-(predictor_devs$glmdevs/predictor_devs$nulldevs)



#PLot mean probability +/- SE from raw data with model fits

#plot function - 
#datacuts_string: character or string, single vs multiple region per plot
#predictor: character, predictor name string
#maxprob: numeric, ymax for probability plot
#withglm: TRUE/FALSE; plot glm model fit also?








#probplot<-function(datacuts_string, predictor, bins, maxprob, withglm)

predictornames

for(i in 1:length(predictornames)){
probplot(c("bidat", "mndat"), predictornames[i], 40, 0.015, FALSE) 
}

for(i in 1:length(predictornames)){
  probplot(c("oadat", "kaoadat"), predictornames[i], 40, 0.015, FALSE) 
}

for(i in 1:length(predictornames)){
  probplot(c("oadat", "kadat", "kaoadat"), predictornames[i], 40, 0.015, FALSE) 
}

 
 probplot(c("bidat", "mndat", "oadat"), "barecov_yearof", 40, 0.012, FALSE) 


probplot(c("oadat", "kadat", "kaoadat"), "herbcov_yearof", 40, 0.012, FALSE) 
probplot(c("oadat", "kadat", "kaoadat"), "woodcov_yearof", 40, 0.012, FALSE) 

save.image(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_2021_09_all_island_effects_plots/00_2021_09_all_island_effects_plots_line242.Rdata')




# # shortcut - load all the above
# load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_2021_09_all_island_effects_plots/00_2021_09_all_island_effects_plots_line242.Rdata')
# save(bidat, kadat, mndat, oadat, kaoadat, bigpredict, cut, predictornames, predictor_devs,
#      file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_2021_09_all_island_effects_plots/00_2021_09_all_island_effects_plots_plotData.Rdata')

# shortcut - load only what's needed for plotting
library(data.table); library(mgcv); library(ggplot2)
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_2021_09_all_island_effects_plots/00_2021_09_all_island_effects_plots_plotData.Rdata')
gc()

# create variable names table
predictornames_df<-data.frame(predictornames=c("herbcov_yearof", "woodcov_yearof", "barecov_yearof",
                                               "mean_annual_rainfall", "HI_EVAP_min_monthly_soil_mst",
                                               "mean_annual_temp", "ign_trns","HI_DEM_10m" , "X2020_Hawaii_population_GHS_global_250m",
                                               "monthly_cumrf_12mo", "monthly_cumrf_3mo", "rf_1_mo_prior", 'rf_1mo_prior', "monthly_rf_0_mo_prior",
                                               "aspect" ,"roughness"), 
                              label         =c("Same-year herbaceous landcover (%)", "Same-year woody landcover (%)", "Same-year bare landcover (%)",
                                               "Mean annual rainfall (mm)", "Min. monthly soil moisture",
                                               "Mean annual temperature (C)", "Ignition density","HI_DEM_10m" , "X2020_Hawaii_population_GHS_global_250m",
                                               "Rainfall, previous 12 months (mm)", "Rainfall, previous 3 months (mm)", "Rainfall, previous month (mm)", "Rainfall, previous month (mm)", "Same-month rainfall (mm)",
                                               "Aspect" ,"Roughness"))

predictornames_full<-c("herbcov_yearof", "woodcov_yearof", "barecov_yearof", 
                       "mean_annual_rainfall", "HI_EVAP_min_monthly_soil_mst", 
                       "mean_annual_temp", "ign_trns","HI_DEM_10m" , "X2020_Hawaii_population_GHS_global_250m" , "mean_annual_rainfall", "monthly_cumrf_18mo",
                       "monthly_cumrf_12mo", "monthly_cumrf_3mo", "monthly_rf_0_mo_prior",
                       "aspect" ,"roughness")




##### plot function - ggplot #####

rm(cut)

# melt data
alldat <- as.data.frame(rbind(oadat, kadat, bidat, mndat))
alldat <- alldat[,c('burnt', predictornames_full, 'rf_1mo_prior')]
alldat$island <- c(rep('Oahu', times = nrow(oadat)),
                   rep('Kauai', times = nrow(kadat)),
                   rep('Hawaii', times = nrow(bidat)),
                   rep('Maui County', times = nrow(mndat)))
alldat$datacut <- c(rep('oadat', times = nrow(oadat)),
                    rep('kadat', times = nrow(kadat)),
                    rep('bidat', times = nrow(bidat)),
                    rep('mndat', times = nrow(mndat)))
alldat$monthly_rf_1_mo_prior <- alldat$rf_1mo_prior
rm(bidat, kadat, oadat, mndat, kaoadat)
gc()

# write function

#predictor = 'woodcov_yearof'; bins = 40; maxprob = NA; errorBarWidth = 0  # TEST VALUES

probggplot <- function(predictor, bins = 40, maxprob = NA, errorBarWidth = 0){
  
  
  
  
  ##### plot data calculations #####
  
  # define plot formula
  plotform <- as.formula(paste0('alldat$burnt ~ alldat$', predictor))
  
  # split dat aby island
  alldat_split <- split(alldat, alldat$island)
  
  # cut predictor values into bins by island
  cut_x <- lapply(alldat_split, function(df){
    cut(df[,predictor], bins)
  })
  
  # associated probabilities (y-values)
  probs_x <- list()
  for(i in 1:length(alldat_split)){
    probs_x[[i]] <-
      as.vector(tapply(alldat_split[[i]]$burnt, cut_x[[i]], sum) / table(cut_x[[i]]))
  }
  
  # get mean predictor value by bin
  x_means <- list()
  for(i in 1:length(alldat_split)){
    x_means[[i]] <-
      as.vector(tapply(alldat_split[[i]][,predictor],
                       cut_x[[i]], mean, na.rm = TRUE))
  }
  
  # get SEs for each x bin
  x_se <- list()
  for(i in 1:length(alldat_split)){
    x_se[[i]] <-
      as.vector(sqrt(probs_x[[i]]*(1-probs_x[[i]])/table(cut_x[[i]])))
  }
  
  # use SEs to define upper and lower points of SE on plot
  se_plus <- list()
  for(i in 1:length(alldat_split)){
    se_plus[[i]] <-
      probs_x[[i]] + x_se[[i]]
  }
  
  se_minus <- list()
  for(i in 1:length(alldat_split)){
    se_minus[[i]] <-
      probs_x[[i]] - x_se[[i]]
  }
  
  # calculate d2
  d2 <- list()
  for(i in 1:length(alldat_split)){
    d2[[i]] <- 
      100 * round(predictor_devs$expldev_gam[which(predictor_devs$datacut == unique(alldat_split[[i]]$datacut[[1]]) &
                                                     predictor_devs$predictor == predictor)],
                  4)
  }
  d2 <- unlist(d2)
  
  
  
  
  ##### plot data formatting #####
  
  # define position dodge
  #pd <- position_dodge(position.dodge) # move them .05 to the left and right
  
  # combine point data
  dat_points <- data.frame(x = unlist(x_means),
                           y = unlist(probs_x),
                           island = rep(names(alldat_split), each = bins))
  
  # combine SE data
  dat_SE <- data.frame(x = unlist(x_means),
                       ymin = unlist(se_minus),
                       ymax = unlist(se_plus),
                       island = rep(names(alldat_split), each = bins))
  
  # create line data
  dat_lines <- list()
  for(i in 1:length(alldat_split)){
    dat_lines[[i]] <-
      data.frame(x = bigpredict[,predictor][bigpredict$datacut==alldat_split[[i]]$datacut[[1]]],
                 y = bigpredict[,paste0(predictor, "_gampred")][bigpredict$datacut==alldat_split[[i]]$datacut[[1]]],
                 island = names(alldat_split)[[i]]
                 )
  }
  dat_lines <- do.call(rbind, dat_lines)
  
  
  
  
  
  ##### plot #####
  
  finalPlot <- ggplot() +
    geom_point(data = dat_points, aes(x = x, y = y, color = island),
               size= 2) +
    geom_errorbar(data = dat_SE, aes(x = x, ymin = ymin, ymax = ymax, color = island),
                  alpha = 0.4, size = 1.1,
                  width = errorBarWidth) +
    geom_line(data = dat_lines, aes(x = x, y = y, color = island), size = 1.3) +
    scale_color_discrete(labels = c(as.expression(bquote(Hawaii~GAM~d^2 == .(signif(d2[[1]], digits = 3))*'%')),
                                    as.expression(bquote(Kauai~GAM~d^2 == .(signif(d2[[2]], digits = 3))*'%')),
                                    as.expression(bquote(Maui~County~GAM~d^2 == .(signif(d2[[3]], digits = 3))*'%')),
                                    as.expression(bquote(Oahu~GAM~d^2 == .(signif(d2[[4]], digits = 3))*'%')))) +
    labs(x = predictornames_df[which(predictornames_df$predictornames == predictor),'label'],
         y = 'Fire probability', color = 'Island') +
    theme(text = element_text(size = 15))
  
  # change y-axis limit if needed
  if(!is.na(maxprob)){
    finalPlot <- finalPlot +
      scale_y_continuous(limits = c(0, maxprob))
  }
  
  return(finalPlot)
  
}

# create plots
predictors_to_plot <- c('herbcov_yearof', 'woodcov_yearof', 'HI_EVAP_min_monthly_soil_mst',
                        'mean_annual_temp', 'monthly_cumrf_3mo',
                        'monthly_cumrf_12mo', 'rf_1mo_prior', 'ign_trns')

y_axis_limits <- c(NA,NA,NA,NA,NA,NA,NA,NA)  # change y-axis limit of figure i if needed

for(i in 1:length(predictors_to_plot)){
  
  probggplot(predictor = predictors_to_plot[[i]], maxprob = y_axis_limits[[i]])
  
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Fire/Figures for publication/Effects plots/ggplot/effectplot_',
                           predictors_to_plot[[i]], '.png'),
         dpi = 300, height = 6, width = 9)
  
}




##### plot function - original #####
# 
# 
# # function
# probplot<-function(datacuts_string, predictor, bins, maxprob, legend_location_GAM = 'topright', legend_location_island = 'top'){
#   if(length(datacuts_string)==1){
#     plotform<-as.formula(paste0(datacuts_string,"$burnt~", datacuts_string,"$", predictor))
#     par(bty = "l")
#     plot(plotform, ylab="Fire probability", pch="", ylim=c(0,maxprob), las=1,xlab=predictornames_df[which(predictornames_df$predictornames == predictor), 'label'])
#     cut_x<-cut(eval(parse(text=paste0(datacuts_string,"$",predictor))), bins)
#     props_x<-with(get(datacuts_string), tapply(burnt, cut_x, sum)/ table(cut_x))
#     x_means<-as.vector(with(get(datacuts_string), tapply(eval(parse(text=predictor)), cut_x, mean)))
#     points(x_means, props_x, pch=14, cex=1.5, col=2)
#     x_se<-sqrt(props_x*(1-props_x)/table(cut_x))
#     se_plus<-props_x+as.vector(x_se)
#     se_minus<-props_x-as.vector(x_se)
#     d2<-100*round(predictor_devs$expldev_gam[which(predictor_devs$datacut==datacuts_string & predictor_devs$predictor==predictor)], 3)
#     for(i in 1:bins){
#       lines(c(x_means[i], x_means[i]), c(se_plus[i], se_minus[i]), lwd=2, col=1)
#     }
#     lines(eval(parse(text=paste0("bigpredict$",predictor,"_gampred")))[bigpredict$datacut==datacuts_string] ~ 
#             eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string], col=2)
#     legend(legend_location_GAM,
#            legend = bquote(GAM ~ d^2 == .(d2)~'%'),
#            lty=1, col=2, bty="n", xjust=0)
#   }else{
#     plotform<-as.formula(paste0(datacuts_string[1],"$burnt~", datacuts_string[1],"$", predictor))
#     par(bty = "l")
#     plot(plotform, ylab="Fire probability", pch="", ylim=c(0,maxprob), las=1,xlab=predictornames_df[which(predictornames_df$predictornames == predictor), 'label'])
#     for(m in 1:length(datacuts_string)){
#       cut_x<-cut(eval(parse(text=paste0(datacuts_string[m],"$",predictor))), bins)
#       props_x<-with(get(datacuts_string[m]), tapply(burnt, cut_x, sum)/ table(cut_x))
#       x_means<-as.vector(with(get(datacuts_string[m]), tapply(eval(parse(text=predictor)), cut_x, mean)))
#       points(x_means, props_x, pch=14+m, cex=1.5, col=1+m)
#       x_se<-sqrt(props_x*(1-props_x)/table(cut_x))
#       se_plus<-props_x+as.vector(x_se)
#       se_minus<-props_x-as.vector(x_se)
#       d2<-100*round(predictor_devs$expldev_gam[which(predictor_devs$datacut==datacuts_string[m] & predictor_devs$predictor==predictor)], 3)
#       regiondf<-data.frame(datacuts=c("bidat", "mndat", "oadat","kadat", "kaoadat" ), 
#                            name=c("Hawaii", "Maui County", "Oahu", "Kauai", "Kauai-Oahu combined")) 
#       
#       # standard error lines - no bars
#       # for(i in 1:bins){
#       #   lines(c(x_means[i], x_means[i]), c(se_plus[i], se_minus[i]), lwd=2, col=1+m)
#       # }
#       
#       # standard error lines - bar endcaps
#       for(i in 1:bins){
#         arrows(x0=x_means[i], y0=se_minus[i], x1=x_means[i], y1=se_plus[i], lwd=1, col=1+m, code=3, angle=90, length=0.05)
#       }
#       lines(eval(parse(text=paste0("bigpredict$",predictor,"_gampred")))[bigpredict$datacut==datacuts_string[m]] ~ 
#               eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string[m]], col=1+m, lwd=1.5)
#       legend(x=legend_location_GAM,
#              inset = c(0,-.05+m*.05),
#              legend = bquote(.(regiondf$name[regiondf$datacuts==datacuts_string[m]]) ~ GAM ~ d^2 == .(d2)~'%'),
#              lty=1, col=1+m, bty="n", pch=14+m, xjust=0)
#       
#       # ISLAND LEGEND - SEPARATED FROM GAM LEGEND
#       # if(legend_location_island %in% c('left', 'right')){
#       #   legend(legend_location_island,
#       #          inset = c(-.05+m*.05,0),
#       #          regiondf$name[regiondf$datacuts==datacuts_string[m]], pch=14+m,col=1+m, bty="n",
#       #          ncol = 1, xjust = 0)
#       # } else {
#       #   legend(legend_location_island,
#       #          inset = c(0,-.05+m*.05),
#       #          regiondf$name[regiondf$datacuts==datacuts_string[m]], pch=14+m,col=1+m, bty="n",
#       #          xjust = 0)
#       # }
#     }
#   }
# } 
# 
# probplot(datacuts_string = c("oadat"), predictor = "woodcov_yearof", bins = 40, maxprob = 0.012)
# probplot(datacuts_string = c("oadat", "kadat", 'mndat', 'bidat'), predictor = "woodcov_yearof", bins = 40, maxprob = 0.010, legend_location_GAM = 'topright')








##### Clay's original code #####

# probplot<-function(datacuts_string, predictor, bins, maxprob, withglm){
#   if(length(datacuts_string)==1){
#     plotform<-as.formula(paste0(datacuts_string,"$burnt~", datacuts_string,"$", predictor))
#     plot(plotform, ylab="Fire probability", pch="", ylim=c(0,maxprob), las=1,xlab=predictor)
#     cut_x<-cut(eval(parse(text=paste0(datacuts_string,"$",predictor))), bins)
#     props_x<-with(get(datacuts_string), tapply(burnt, cut_x, sum)/ table(cut_x))
#     x_means<-as.vector(with(get(datacuts_string), tapply(eval(parse(text=predictor)), cut_x, mean)))
#     points(x_means, props_x, pch=14, cex=1.5, col=2)
#     x_se<-sqrt(props_x*(1-props_x)/table(cut_x))
#     se_plus<-props_x+as.vector(x_se)
#     se_minus<-props_x-as.vector(x_se)
#     for(i in 1:bins){
#       lines(c(x_means[i], x_means[i]), c(se_plus[i], se_minus[i]), lwd=2, col=1)
#     }
#   lines(eval(parse(text=paste0("bigpredict$",predictor,"_gampred")))[bigpredict$datacut==datacuts_string] ~ 
#           eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string], col=2)
#   legend(min(eval(parse(text=paste0("bigpredict$",predictor)))), maxprob, 
#          paste0("GAM d^2=", 100*round(predictor_devs$expldev_gam[which(predictor_devs$datacut==datacuts_string & predictor_devs$predictor==predictor)], 3), "%"),
#          lty=1, col=2, bty="n")    
#   legend(min(eval(parse(text=paste0("bigpredict$",predictor))))+min(eval(parse(text=paste0("bigpredict$",predictor))))*.4, maxprob, 
#          datacuts_string)
#    if(withglm==TRUE){
#     lines(eval(parse(text=paste0("bigpredict$",predictor,"_glmpred")))[bigpredict$datacut==datacuts_string] ~ 
#             eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string],col=2, lty=2)
#     legend(min(eval(parse(text=paste0("bigpredict$",predictor)))), maxprob-.0005, 
#            paste0("GLM %d^2=", 100*round(predictor_devs$expldev_glm[which(predictor_devs$datacut==datacuts_string & predictor_devs$predictor==predictor)], 3), "%"),
#            lty=2, col=2, bty="n")      
#   }
#   }else{
#     plotform<-as.formula(paste0(datacuts_string[1],"$burnt~", datacuts_string[1],"$", predictor))
#     plot(plotform, ylab="Fire probability", pch="", ylim=c(0,maxprob), las=1,xlab=predictor)
#        for(m in 1:length(datacuts_string)){
#          cut_x<-cut(eval(parse(text=paste0(datacuts_string[m],"$",predictor))), bins)
#          props_x<-with(get(datacuts_string[m]), tapply(burnt, cut_x, sum)/ table(cut_x))
#          x_means<-as.vector(with(get(datacuts_string[m]), tapply(eval(parse(text=predictor)), cut_x, mean)))
#          points(x_means, props_x, pch=14+m, cex=1.5, col=1+m)
#          x_se<-sqrt(props_x*(1-props_x)/table(cut_x))
#          se_plus<-props_x+as.vector(x_se)
#          se_minus<-props_x-as.vector(x_se)
#          for(i in 1:bins){
#            lines(c(x_means[i], x_means[i]), c(se_plus[i], se_minus[i]), lwd=2, col=1)
#          }
#          lines(eval(parse(text=paste0("bigpredict$",predictor,"_gampred")))[bigpredict$datacut==datacuts_string[m]] ~ 
#                  eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string[m]], col=1+m)
#          legend(min(na.omit(eval(parse(text=paste0(datacuts_string[1],"$",predictor))))), maxprob-m*.0005, 
#                 paste0("GAM d^2=", 100*round(predictor_devs$expldev_gam[which(predictor_devs$datacut==datacuts_string[m] & predictor_devs$predictor==predictor)], 3), "%"),
#                 lty=1, col=1+m, bty="n")    
#         regiondf<-data.frame(datacuts=c("bidat", "mndat", "oadat","kadat", "kaoadat" ), 
#                              name=c("Big Island", "Maui Nui", "Oahu", "Kauai", "Kauai-Oahu combined")) 
#         legend(min(na.omit(eval(parse(text=paste0(datacuts_string[1],"$",predictor)))))+max(na.omit(eval(parse(text=paste0(datacuts_string[1],"$",predictor)))))*.4, maxprob-m*.0005, 
#                 regiondf$name[regiondf$datacuts==datacuts_string[m]], pch=14+m,col=1+m, bty="n" )  
#        if(withglm==TRUE){
#            lines(eval(parse(text=paste0("bigpredict$",predictor,"_glmpred")))[bigpredict$datacut==datacuts_string[m]] ~ 
#                    eval(parse(text=paste0("bigpredict$",predictor)))[bigpredict$datacut==datacuts_string[m]],col=1+m, lty=2)
#            legend(min(na.omit(eval(parse(text=paste0(datacuts_string[1],"$",predictor))))), maxprob-(.001*length(datacuts_string[m]))-m*.0005, 
#                   paste0("GLM d^2=", 100*round(predictor_devs$expldev_glm[which(predictor_devs$datacut==datacuts_string[m] & predictor_devs$predictor==predictor)], 3), "%"),
#                   lty=2, col=1+m, bty="n")      
#          }
#     }
#   }
# } 
