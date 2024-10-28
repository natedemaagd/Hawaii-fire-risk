library(raster)

# load statewide raster and maui nui raster
r_statewide <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Statewide_Ign_Dens/original statewide/Statewide_Ign_Per_Sq_Mi.tif")
r_maui <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Statewide_Ign_Dens/Ignition_dens_mauinui_firemod.tif")

# re-project raster
r_maui <- projectRaster(r_maui, r_statewide)

# merge rasters - want to include values for Kahoolawe in `r_maui` in `r_statewide`
r_merge <- merge(r_statewide, r_maui)

# save statewide raster with Kahoolawe values added
writeRaster(r_merge, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Statewide_Ign_Dens/Statewide_Ign_Per_Sq_Mi.tif')
