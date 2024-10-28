library(raster); library(zoo)


# load beginning and ending landcover rasters
lc_bare_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled', pattern = 'bare')
lc_herb_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled', pattern = 'herb')
lc_wood_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled', pattern = 'wood')
lc_bare <- lapply(lc_bare_filenames, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))
lc_herb <- lapply(lc_herb_filenames, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))
lc_wood <- lapply(lc_wood_filenames, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/', r)))


# add values to a matrix: rows are years, columns are pixels. For now, omit NAs to save space
mat_bare <- matrix(nrow = length(1999:2016), ncol = length(values(lc_bare[[1]])[!is.na(values(lc_bare[[1]]))]))
mat_bare[1,]              <- values(lc_bare[[1]])[!is.na(values(lc_bare[[1]]))]
mat_bare[nrow(mat_bare),] <- values(lc_bare[[2]])[!is.na(values(lc_bare[[2]]))]
mat_bare <- na.approx(mat_bare)
gc()
save(mat_bare, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_landcover_formatting/landcover_yearly_matrix_noNAs_bare.Rdata')

mat_herb <- matrix(nrow = length(1999:2016), ncol = length(values(lc_herb[[1]])[!is.na(values(lc_herb[[1]]))]))
mat_herb[1,]              <- values(lc_herb[[1]])[!is.na(values(lc_herb[[1]]))]
mat_herb[nrow(mat_herb),] <- values(lc_herb[[2]])[!is.na(values(lc_herb[[2]]))]
mat_herb <- na.approx(mat_herb)
gc()
save(mat_herb, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_landcover_formatting/landcover_yearly_matrix_noNAs_herb.Rdata')

mat_wood <- matrix(nrow = length(1999:2016), ncol = length(values(lc_wood[[1]])[!is.na(values(lc_wood[[1]]))]))
mat_wood[1,]              <- values(lc_wood[[1]])[!is.na(values(lc_wood[[1]]))]
mat_wood[nrow(mat_wood),] <- values(lc_wood[[2]])[!is.na(values(lc_wood[[2]]))]
mat_wood <- na.approx(mat_wood)
gc()
save(mat_wood, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00_landcover_formatting/landcover_yearly_matrix_noNAs_wood.Rdata')


# save each row of the matrices as a landcover percentage raster
year <- 1999:2016

raster_base <- lc_bare[[1]]
for(y in 1:nrow(mat_bare)){
  r <- lc_bare[[1]]
  r[!is.na(r)] <- mat_bare[y,]
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_bare_', year[[y]], '.tif'),
              overwrite = TRUE)
  gc()
}

raster_base <- lc_herb[[1]]
for(y in 1:nrow(mat_herb)){
  r <- lc_herb[[1]]
  r[!is.na(r)] <- mat_herb[y,]
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_herb_', year[[y]], '.tif'),
              overwrite = TRUE)
  gc()
}

raster_base <- lc_wood[[1]]
for(y in 1:nrow(mat_wood)){
  r <- lc_wood[[1]]
  r[!is.na(r)] <- mat_wood[y,]
  writeRaster(r, filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_wood_', year[[y]], '.tif'),
              overwrite = TRUE)
  gc()
}
