library(ggplot2)

# load data
dat_list <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_pixel_data.rds')

# list all islands
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
island_name <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')

# get island for each fire
island_fires <- unlist(sapply(dat_list, function(x) x$island))

# get all fire values for each island
fire_values <- list()
for(i in 1:length(island)){
  
  # get all fire pixels on island i
  fire_pixels <- c(unlist(sapply(dat_list[which(island_fires == island[[i]])], function(x) x$fire_values)))
  
  # get all sampled non-fire pixels on island i
  nonfire_pixels <- c(unlist(sapply(dat_list[which(island_fires == island[[i]])], function(x) x$sample_nonfire_vals)))
  
  # save data
  fire_values[[i]] <- list()
  fire_values[[i]][[1]] <- fire_pixels
  fire_values[[i]][[2]] <- nonfire_pixels
  names(fire_values[[i]]) <- c('fire_pixels', 'nonfire_pixels')
  
  rm(i, fire_pixels, nonfire_pixels)
}
names(fire_values) <- island

# create distribution plot for each island
firedat_Hawaii <- data.frame(value = c(fire_values$Hawaii$fire_pixels, fire_values$Hawaii$nonfire_pixels),
                             type = c(rep('Fire', times = length(fire_values$Hawaii$fire_pixels)),
                                      rep('Non-fire', times = length(fire_values$Hawaii$nonfire_pixels))))
  #ggplot(data = firedat_Hawaii) + geom_density(aes(value, fill = type, color = type), alpha = 0.3)
  tapply(firedat_Hawaii$value, firedat_Hawaii$type, summary)

firedat_Oahu <- data.frame(value = c(fire_values$Oahu$fire_pixels, fire_values$Oahu$nonfire_pixels),
                             type = c(rep('Fire', times = length(fire_values$Oahu$fire_pixels)),
                                      rep('Non-fire', times = length(fire_values$Oahu$nonfire_pixels))))
  #ggplot(data = firedat_Oahu) + geom_density(aes(value, fill = type, color = type), alpha = 0.3)
  tapply(firedat_Oahu$value, firedat_Oahu$type, summary)

firedat_Kauai <- data.frame(value = c(fire_values$Kauai$fire_pixels, fire_values$Kauai$nonfire_pixels),
                             type = c(rep('Fire', times = length(fire_values$Kauai$fire_pixels)),
                                      rep('Non-fire', times = length(fire_values$Kauai$nonfire_pixels))))
  #ggplot(data = firedat_Kauai) + geom_density(aes(value, fill = type, color = type), alpha = 0.3)
  tapply(firedat_Kauai$value, firedat_Kauai$type, summary)

firedat_MauiCounty <- data.frame(value = c(fire_values$MauiCounty$fire_pixels, fire_values$MauiCounty$nonfire_pixels),
                             type = c(rep('Fire', times = length(fire_values$MauiCounty$fire_pixels)),
                                      rep('Non-fire', times = length(fire_values$MauiCounty$nonfire_pixels))))
  #ggplot(data = firedat_MauiCounty) + geom_density(aes(value, fill = type, color = type), alpha = 0.3)
  tapply(firedat_MauiCounty$value, firedat_MauiCounty$type, summary)

# deciles by island
tapply(firedat_Hawaii$value,     firedat_Hawaii$type,     function(x) quantile(x, probs = seq(0,1,0.1), na.rm = TRUE))
tapply(firedat_Oahu$value,       firedat_Oahu$type,       function(x) quantile(x, probs = seq(0,1,0.1), na.rm = TRUE))
tapply(firedat_Kauai$value,      firedat_Kauai$type,      function(x) quantile(x, probs = seq(0,1,0.1), na.rm = TRUE))
tapply(firedat_MauiCounty$value, firedat_MauiCounty$type, function(x) quantile(x, probs = seq(0,1,0.1), na.rm = TRUE))




##### clay's analysis - can run on its own #####

library(ggplot2)

# list all islands
island <- c('Hawaii', 'Oahu', 'Kauai', 'MauiCounty')
island_name <- c('Hawaii', 'Oahu', 'Kauai', 'Maui County')

##Fire perimeter pixel values

allvals<-readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_pixel_data.rds')

for(i in 1:length(island)){
  
  island_vals<-allvals[grepl("Hawaii", allvals )]
  
  island_valsdf<-data.frame(probs=as.numeric(), sample=as.numeric())
  
  for(j in 1:length(island_vals)){
    fire_vals<-island_vals[[j]]$fire_values
    non_fire_values<-island_vals[[j]]$sample_nonfire_vals
    probs<-c(fire_vals, non_fire_values)
    sample<-c(rep("fires", length(fire_vals)), rep("random", length(non_fire_values)))
    minidf<-cbind(probs, sample)
    names(minidf)<-c("probs", "sample")
    island_valsdf<-rbind(island_valsdf, minidf )
  }
  
  island_valsdf$probs<-as.numeric(island_valsdf$probs)
  island_valsdf<-island_valsdf[complete.cases(island_valsdf)==TRUE,]
  
  length(island_valsdf$probs[island_valsdf$sample=="fires"])
  length(island_valsdf$probs[island_valsdf$sample=="random"])
  
  
  
  plot(density(island_valsdf$probs[island_valsdf$sample=="random"]))
  lines(density(island_valsdf$probs[island_valsdf$sample=="fires"]), col="red")
  
  island_valsdf$burnt<-ifelse(island_valsdf$sample=="fires", 1, 0)
  
  #set up a balanced data.frame to fit the glm - this will center the midpoint of the threshold (the logistic curve) at 0.5 more or less
  
  thresholddf<-data.frame(probs=c(sample(island_valsdf$probs[island_valsdf$sample=="random"], 10000),
                                  sample(island_valsdf$probs[island_valsdf$sample=="fires"], 10000)),
                          burnt=c(rep(0, 10000,), rep(1,10000)))
  
  test<-glm(burnt~probs, data=thresholddf, family="binomial")
  null<-glm(burnt~1, data=thresholddf, family="binomial")
  
  summary(test)
  
  1-deviance(test)/deviance(null)
  
  predictiondf<-data.frame(probs=seq(0,1,length.out=1000))
  predictiondf$prediction<-predict(test, predictiondf, type="response")
  predictiondf$se<-predict(test, predictiondf, type="response", se.fit=TRUE)[[2]]
  
  closest_val_to_threshold <- predictiondf[which.min(abs(predictiondf$prediction - 0.999)),'probs']
  
  ggplot(data = predictiondf) + geom_line(aes(x = probs, y = prediction)) +
    geom_line(aes(x = probs, y = (prediction - se)), linetype = 'longdash') +
    geom_line(aes(x = probs, y = (prediction + se)), linetype = 'longdash') +
    geom_vline(xintercept = closest_val_to_threshold, linetype = 'longdash', color = 'red') +
    annotate('text', label = round(closest_val_to_threshold, 3), x = closest_val_to_threshold, y = -0.07) +
    coord_cartesian(ylim=c(0,1), clip="off") +
    labs(x = 'Probability', y = 'Prediction', title = island_name[[i]])
  
  # save figure, data, and model
  ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_prob_thresholds/plot_', island[[i]], '.png'))
  write.csv(predictiondf, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_prob_thresholds/data_', island[[i]], '.csv'))
  saveRDS(test, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/fire_prob_thresholds/model_', island[[i]], '.rds'))
}
