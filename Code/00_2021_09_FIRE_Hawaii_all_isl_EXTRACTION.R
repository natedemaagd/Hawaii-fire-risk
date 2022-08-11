rm(list=ls())
options(scipen=999)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(raster)
library(rgdal)
library(sp)
library(exactextractr)
library(stringr)
library(sf)
library(data.table)
library(mgcv)
library(MLeval)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   HAWAII STATE ANNUAL FIRE PROBABILITY MODEL (aka LANDSCAPE FLAMMABILITY)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   Fire Data - 2019_1999 Hawaii_Fire_Perimeters 
#   1999-2019 Hawaii statewide 'Large Fire' (>20 ha) polygons
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

starttime<-Sys.time()

#import shapefile as spatial.polygons.data.frame
#fires<-readOGR("/Volumes/LaCie/Documents/Data/Hawaii Data Sets/Hawaii Fires/2019_1999_Hawaii_Fire_Perimeters", "2019_1999_Hawaii_Fire_Perimeters")
fires<-st_read("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")
fires<-as(fires, "Spatial") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   Predictor Variables (Vegetation, Climate, Ignitions, etc)
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predictorlist<-c(

#~~~~~~~~~~CLIMATE ~~~~~~~~~~
#2020_12 - UPDATED MONTHLY HISTORICAL RAINFALL 1990-2019, LUCAS & GIAMBELLUCA (ET AL.) in prep
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2020_HI_monthly_rainfall_grids/grids/rf_mm/hawaii_1990_2019_monthly_rf_mm/grid_surface_data/statewide/rf_mm",
#2012 Hawaii Moisture Zones; PRice and Jacobi https://www.sciencebase.gov/catalog/item/57a902e8e4b05e859bdf3c83
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP",
#H2013 Hawaii Evapotranspiration Climate products (UH-Manoa)	Giambelluca et al. http://evapotranspiration.geography.hawaii.edu/
#mean annual temp
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_ann",
#mean annual rainfall
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann",
#minimum monthly soil moisture
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/min_monthly_soil_mst",

#~~~~~~~~~~TOPOGRAPHY~~~~~~~~~~
#USGS 10-m DEM + Raster package to create for 30m slope, aspect, etc.
#Elevation -
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_DEM_10m",
#roughness
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Roughness",
#ruggedness
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Ruggedness",
#Aspect
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Aspect",
#slope
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Slope",

#~~~~~~~~~~VEGETATION~~~~~~~~~~
#2017 USGS Land Cover Jacobi et al. 2017  Carbon Assessment of Hawaii: U.S. Geological Survey data release, https://doi.org/10.5066/F7DB80B9. 
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/CAH_LandCover_HI_GAP_revised_2017",
#2017 Hawaii Annual Fractional Land Cover (Lucas 2017); Annual** 1999-2016 (**NO 2014) % Cover Woody, Herbaceous, Bare; 30m
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split",

#~~~~~~~~~~IGNITIONS~~~~~~~~~~
##2013 HWMO ignition density (# ignitions per square mile per year) Trauernicht and Lucas 2016. derived from all statewide wildfire ignition points 2002-2013
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Statewide_Ign_Dens",
#2015 Population density (#ppl/250m cell) from EU Global Human Settlement project; https://ghsl.jrc.ec.europa.eu/ghs_pop2019.php (European Union; 250m)
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii_Population_density_statewide_GHS",
#Custom Road "density" = Count of roads per 250m cell derived from Open Street Maps data (all hwys != "unclassified")
"H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Hawaii road density OSM"
)

#CHECK DATA RASTERS ARE THERE
for(i in 2:length(predictorlist)){
    print(list.files(predictorlist[i], pattern=".tif$"))
}

test<-raster(predictorlist[3])
plot(test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   I. COMPILE TRAINING DATA
#  
##      - Sample points over study region, 
##      - classify as BURNT (1) or UNBURNT (0) using FIRE PERIMETER DATA (vector)
##      - extract predictors to x,y points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sampling scheme as of 4/2021
#
#   A. BIG SAMPLE of BURNT/UNBURNT points at 30m resolution (land cover) at MONTHLY time-steps
#     1. sample N=10000 random points per year-month over entire fire history
#     2. sample ALL burnt points (30m grid) within fire perimeters per year-month
#     3. check for duplicate unburnt/burnt year-month-pixels (voxels); keep burnt, remove uburnt
#     4. check for duplicate voxels (30m) within 250m grid (climate variables) and ditch duplicates
#
#   B. Proportionally Subsample BURNT/UNBURNT relative to area burned per time-step (annual sampling)
#     1.  predicted probabilities then reflect/are bounded by burned area extents (eg. 10% area burned/yr = 0.1 ann. prob. of burning)
#
#   C.  Extract data from predictor rasters to proportional subsample of BURNT/UNBURNT (as a spatial.points.data.frame)
#     1. F0r Rainfall explicitly by year-month-pixel (voxels) for month-of rainfall + 24 prior months
#     2. For annual/nontemporal data (eg. mean annual climate vars), all data are extracted 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reference rasters for different resolutions among response and predictor variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# BURNT/UNBURNT sampled at 30m; 
# But Variables scaled to 30m (land cover) AND 250m (climate vars):
# SO, to remove replicate points within climate grids, create 'ref250id', a 30m raster of 250m grid cell IDs 

## 30m reference raster:  Hawaii State from CAH land cover
ref30m<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/CAH_LandCover_HI_GAP_revised_2017/CAH_LandCover.tif")
## 250m reference raster - Rainfall atlas - temperature (Giambelluca et al)
ref250m<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_ann/w001001.adf")

# ref250id<-ref250m
# values(ref250id)<-seq(1, ncell(ref250id),1)
# ref250id<-projectRaster(ref250id, ref30m, method="ngb")
# mask30m<-ref30m   #GET RID OF OCEAN CELLS
# mask30m[mask30m==0]<-NA
# mask30m[mask30m==65535]<-NA
# ref250id<-mask(ref250id, mask=mask30m)
# writeRaster(ref250id, "C:/Users/nated/Google Drive (demaagdn@hawaii.edu)/Projects/Fire/2021_HI_FIre_Model_Data/2020_HI_250m_gridcell_reference_HIevap_30m.tif", overwrite=TRUE)
ref250id<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2020_HI_250m_gridcell_reference_HIevap_30m.tif")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A. BIG SAMPLE of BURNT/UNBURNT points at 30m resolution (land cover) at MONTHLY time-steps
#     1. sample N=10000 random points per year-month over entire fire history
#     2. sample ALL burnt points (30m grid) within fire perimeters per year-month
#     3. check for duplicate unburnt/burnt year-month-pixels (voxels); keep burnt, remove uburnt
#     4. check for duplicate voxels (30m) within 250m grid (climate variables) and ditch duplicates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fires<-readOGR("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters", "2019_1999_Hawaii_Fire_Perimeters")

#PREP FIRE DATA
fires<-spTransform(fires, CRS=crs(ref250id))
# add year_mo; county; area in hectares
fires@data$yearmo<-paste(fires@data$Year,fires@data$Month, sep="_")

#couple fixes for county

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#for loop draws monthly random samples, checks for fires, and samples all burnt points using exact_extract() function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##REF250 = Nad834

# create big xy data.frame
bigext<-data.frame(cell=as.numeric(), x=as.numeric(), y=as.numeric(), id250=as.numeric(), yearmo=as.numeric(), burnt=as.numeric())
#create YEAR-MONTH Sequence
yearmos<-character()
for(p in 1999:2019){
  yearmopiece<-paste(rep(p, 12), seq(1, 12, 1), sep="_")
  yearmos<-c(yearmos, yearmopiece)
}

a<-Sys.time()
for(i in 1:length(yearmos)){
  rndm<-as.data.frame(sampleRandom(ref250id, 20000, xy=TRUE, cell=TRUE))		#sample randomly using 30m grid labeled w 250 grid cell numbers
  names(rndm)[4]<-"id250"
  rndm$yearmo<-rep(yearmos[i], nrow(rndm))
  rndm$burnt<-rep(0, nrow(rndm))	
  bigext[(nrow(bigext)+1):(nrow(bigext)+nrow(rndm)),]<-rndm
  fireyearmo<-fires[fires$yearmo==yearmos[i],]	#check for fires
  if(nrow(fireyearmo)>0){
    burntlist<-exact_extract(ref250id, st_as_sf(fireyearmo),  include_xy=TRUE, include_cell=TRUE)	#extract cells intersecting burned polygons
    burntcells<-do.call(rbind, burntlist)							#unlist features into single df
    #			burntcells<-burntcells[burntcells$coverage_fraction>.25 ,c(4,2,3,1)] 		#keep cells w >25% intersect w burn polygons 
    burntcells<-burntcells[ ,c(4,2,3,1)] 		 
    names(burntcells)[c(1,4)]<-c("cell", "id250")
    burntcells$yearmo<-yearmos[i]
    burntcells$burnt<-1
    bigext[(nrow(bigext)+1):(nrow(bigext)+nrow(burntcells)),]<-burntcells
  }
}
Sys.time()-a
length(bigext$burnt[bigext$burnt==1])
length(bigext$burnt[bigext$burnt==0])
nrow(bigext)

#GET RID OF DUPLICATE "UNBURNT" PIXELS (eg. randomly drawn 'unburnt' pixels that lie within fire perimeters)		
noburndups<-as.data.frame(bigext)[as.numeric(),]
for(i in 1:length(yearmos)){
  sub<-as.data.frame(bigext[bigext$yearmo==yearmos[i],])
  burn_dups <- data.frame(table(sub$cell))	
  sub2<-sub[-which(sub$cell%in% burn_dups$Var1[burn_dups$Freq > 1] & sub$burnt==0),]
  if(nrow(sub2)>=1){
    noburndups[nrow(noburndups)+1:nrow(sub2),]<-as.data.frame(sub2)
  }else{
    noburndups[nrow(noburndups)+1:nrow(sub),]<-as.data.frame(sub)		
  }		
}

nrow(bigext)
nrow(noburndups)
bigext<-noburndups  # redefine bigext sp.df
rm(noburndups)

#GET RID OF DUPLICATE SAMPLES within 250m climate variable grids, PER MONTH
nodups250<-as.data.frame(bigext)[as.numeric(),]
for(i in 1:length(yearmos)){
  sub<-bigext[bigext$yearmo==yearmos[i],]
  sub<-sub[order(sub$id250),]
  subrm<-sub[!duplicated(sub$id250),]
  nodups250[(nrow(nodups250)+1):(nrow(nodups250)+nrow(subrm)),]<-subrm
}

nrow(bigext) - nrow(nodups250)
bigext<-nodups250  #redefine bigext sp.df
rm(nodups250)

#easier YYYYMM date?
yyyymm <- function(d){
  split<- strsplit(d, "_" , fixed=TRUE)
  year <- as.numeric(sapply(split, "[", 1))
  mo <- as.numeric(sapply(split, "[", 2))
  return((year*100)+mo) 
}

bigext$yyyymm<-yyyymm(bigext$yearmo)
bigext$year <- substring(bigext$yyyymm, 1, 4)

head(bigext)
nrow(bigext)

coordinates(bigext)<-~x+y
crs(bigext)<-crs(ref250id)
bigext<-spTransform(bigext, CRS="+proj=longlat +datum=WGS84 +no_defs")

#write.csv(as.data.frame(bigext), "/Users/clayt/Desktop/2021_firepts_only_WGS84.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   B. Proportionally Subsample BURNT/UNBURNT relative to area burned per time-step (annual sampling)
#     1.  predicted probabilities then reflect/are bounded by burned area extents (eg. 10% area burned/yr = 0.1 ann. prob. of burning)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	PROPORTIONAL SAMPLING BY SPATIAL EXTENT OF FIRES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bounds predicted probability by BURNRD AREA PER TIME STEP
# e.g. 10% of the landscape burns per year = ~10% chance any given pixel burns, or annual probability of 0.1  
#
# stratify sampling to guarantee burnt AND unburnt points
# BUT ensure sample size of burnt points is proportional to extent of fire at a given time step
#  IE:  length(burnt) = (prop. burned area per time step) * length(unburnt)

#   DESPITE MONTHLY CLIMATE PREDICTORS, 
#   NOT ENOUGH TOTAL RANDOM SAMPLES (N=1.4 mil; 2021) TO REPRESENT FIRES PROPORTIONALLY by Month
#     EG, out of 208 "fire months", even with 10000 points sampled per month,
#     49 fire months result in a proportional sample of less than 1 pixel (wah wah)

#  So....SAMPLE BY YEAR!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Proportional sampling in space by YEAR  
#Prediction is annual fire probabality per cell
# (even w climate on monthly time steps) <-- ??
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#bigext<-read.csv("/Users/clayt/Desktop/2021_firepts_only_WGS84.csv")
#coordinates(bigextest)<-~x+y
#crs(bigextest)<-"+proj=longlat +datum=WGS84 +no_defs"

head(bigext)
nrow(bigext)

#County rasters for reference (proportional sampling per island) - Jon Price Moisture Zones
kauairef<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/kauai_moisture_zones.tif")
#remove niihau
newkaext<-extent(-159.8, -159.1999, 21.7501 , 22.25001)
kauairef<-crop(kauairef, newkaext)
oahuref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/Oahu_moisture_zones.tif")
mauinuiref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/mauinui_moisture_zones.tif")
bigislandref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/bigisland_moisture_zones.tif")

plot(kauairef)
test<-area(kauairef)

countyareas<-data.frame(county=c("KauaiCounty", "HonoluluCounty", "MauiCounty", "HawaiiCounty" ), 
                        area=c((length(na.omit(getValues(kauairef)))*900)/10000,
                               (length(na.omit(getValues(oahuref)))*900)/10000,
                               (length(na.omit(getValues(mauinuiref)))*900)/10000,
                               (length(na.omit(getValues(bigislandref)))*900)/10000))


#All samples and burnt samples per year [REQUIRES bigext to be spdf]
obsperyr<-aggregate(burnt~year, data=bigext@data, FUN=length)
burntperyr<-aggregate(burnt~year, data=bigext@data, FUN=sum)

#samples per year per island 
kauaiobsann<-aggregate(burnt~year, data=crop(bigext, kauairef), FUN=length)
oahuobsann<-aggregate(burnt~year, data=crop(bigext, oahuref), FUN=length)
mauinuiobsann<-aggregate(burnt~year, data=crop(bigext, mauinuiref), FUN=length)
bigislandobsann<-aggregate(burnt~year, data=crop(bigext, bigislandref), FUN=length)

kauaiobsannburnt<-aggregate(burnt~year, data=crop(bigext, kauairef), FUN=sum)
oahuobsannburnt<-aggregate(burnt~year, data=crop(bigext, oahuref), FUN=sum)
mauinuiobsannburnt<-aggregate(burnt~year, data=crop(bigext, mauinuiref), FUN=sum)
bigislandobsannburnt<-aggregate(burnt~year, data=crop(bigext, bigislandref), FUN=sum)

#area burned per year
fires$area_ha <- fires$Sat_sz_ac * 0.40468564
areaperyr<-aggregate(area_ha~Year, data=fires@data, FUN=sum)
areaperyr$prop_burned<-(areaperyr$area_ha)/sum(countyareas$area)

#area burned per year per COUNTY

splitvec <- strsplit(fires$kmlname, '_')   # this line and the 5 that follow format and add the county variable
county <- sapply(splitvec, function(x) x[[4]])
county[county == 'HawaiiCountry'] <- 'HawaiiCounty'
county[county == 'OahuCounty'] <- 'HonoluluCounty'
fires$county <- county
rm(county, splitvec)
areaperyrcnty<-aggregate(area_ha~Year+county, data=fires@data, FUN=sum)
areaperyrcnty<-merge(areaperyrcnty, countyareas, by="county", all.x=TRUE)
areaperyrcnty$prop_burned<-(areaperyrcnty$area_ha)/areaperyrcnty$area

#Eg with 10000 samples PER YEAR, only 2 years fall into subpixel proportions
ex2<-areaperyr$prop_burned*10000
length(ex2[ex2<1])

#ANNUAL SAMPLES 60-70k per year range ~6k (oa/ka); 12k (mn); 40k (bi)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANNUAL Proportions: all unburnt samples per year + proportional sample of burnt points
#   propscnty = burnt points sampled proportionally to county land area (for county-specific models) 
#   propsallisl = burnt points sampled proportionally to total land area (statewide, single model??)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# proportional sampling based on burned area per county
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

propscnty<-as.data.frame(bigext[bigext@data$burnt==0, ])
nrow(propscnty)

head(propscnty)

for(i in 1:length(unique(bigext$year))){
  burnt_cnt_ka<-nrow(crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], kauairef))
  if(is.integer(burnt_cnt_ka) == TRUE){
    unburnt_cnt_ka<-nrow(crop(bigext[which(bigext@data$burnt==0 & bigext@data$year==areaperyr[i,1]),], kauairef))
    ka1s<-crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], kauairef)
    ka1sub<-ka1s[sample(1:nrow(ka1s), unburnt_cnt_ka*(areaperyrcnty$prop_burned[which(areaperyrcnty$Year== (1998+i) & areaperyrcnty$county=="KauaiCounty")])),]
  }else{
    ka1sub<-bigext[as.numeric(), ]
  }
  burnt_cnt_oa<-nrow(crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], oahuref))
  if(is.integer(burnt_cnt_oa) == TRUE){
    unburnt_cnt_oa<-nrow(crop(bigext[which(bigext@data$burnt==0 & bigext@data$year==areaperyr[i,1]),], oahuref))
    oa1s<-crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], oahuref)
    oa1sub<-oa1s[sample(1:nrow(oa1s), unburnt_cnt_oa*(areaperyrcnty$prop_burned[which(areaperyrcnty$Year== (1998+i) & areaperyrcnty$county=="HonoluluCounty")])),]
  }else{
    oa1sub<-bigext[as.numeric(), ]
  }
  burnt_cnt_mn<-nrow(crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], mauinuiref))
  if(is.integer(burnt_cnt_mn) == TRUE){
    unburnt_cnt_mn<-nrow(crop(bigext[which(bigext@data$burnt==0 & bigext@data$year==areaperyr[i,1]),], mauinuiref))
    mn1s<-crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], mauinuiref)
    mn1sub<-mn1s[sample(1:nrow(mn1s), unburnt_cnt_mn*(areaperyrcnty$prop_burned[which(areaperyrcnty$Year== (1998+i) & areaperyrcnty$county=="MauiCounty")])),]
  }else{
    mn1sub<-bigext[as.numeric(), ]
  }
  burnt_cnt_bi<-nrow(crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], bigislandref))
  if(is.integer(burnt_cnt_bi) == TRUE){
    unburnt_cnt_bi<-nrow(crop(bigext[which(bigext@data$burnt==0 & bigext@data$year==areaperyr[i,1]),], bigislandref))
    bi1s<-crop(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),], bigislandref)
    bi1sub<-bi1s[sample(1:nrow(bi1s), unburnt_cnt_bi*(areaperyrcnty$prop_burned[which(areaperyrcnty$Year== (1998+i) & areaperyrcnty$county=="HawaiiCounty")])),]
  }else{
    bi1sub<-bigext[as.numeric(), ]
  }
  allburnt<-as.data.frame(rbind(ka1sub, oa1sub, mn1sub, bi1sub))
  propscnty[(nrow(propscnty)+1):(nrow(propscnty)+nrow(allburnt)),]   <-  allburnt
}

nrow(propscnty)
nrow(propscnty[propscnty$burnt==1,])
str(propscnty)

coordinates(propscnty)<- ~x + y
crs(propscnty)<-"+proj=longlat +datum=WGS84 +no_defs"

#Add county names to data.frame
kauai<-as.data.frame(crop(propscnty, kauairef))
kauai$county<-"kauai"
oahu<-as.data.frame(crop(propscnty, oahuref))
oahu$county<-"oahu"
mauinui<-as.data.frame(crop(propscnty, mauinuiref))
mauinui$county<-"mauinui"
bigisland<-as.data.frame(crop(propscnty, bigislandref))
bigisland$county<-"bigisland"

propscnty<-rbind(kauai, oahu, mauinui, bigisland)
rm(kauai, oahu, mauinui, bigisland)
nrow(propscnty)

coordinates(propscnty)<- ~x + y
crs(propscnty)<-"+proj=longlat +datum=WGS84 +no_defs"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#proportional sampling based on burned area statewide
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# propsallisl<-as.data.frame(bigext[bigext@data$burnt==0, ])
# 
# for(i in 1:length(unique(bigext$year))){
#   burntcnt<-nrow(bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),])
#   if(is.integer(burntcnt)==TRUE){
#     unburntcnt<-nrow(bigext[which(bigext@data$burnt==0 & bigext@data$year==areaperyr[i,1]),])
#     allburnt<-bigext[which(bigext@data$burnt==1 & bigext@data$year==areaperyr[i,1]),]
#     burntsub<-allburnt[sample(1:nrow(allburnt), unburntcnt*(areaperyr$prop_burned[areaperyr$Year== (1998+i)])),]
#   }else{
#     burntsub<-bigext[as.numeric(), ]
#   }
#   propsallisl[(nrow(propsallisl)+1):(nrow(propsallisl)+nrow(burntsub)),] <- as.data.frame(burntsub)
# }
# 
# nrow(propsallisl)
# nrow(propsallisl[propsallisl$burnt==1,])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# C.  EXTRACT DATA from predictorlist to proportional subsample of BURNT/UNBURNT (as a spatial.points.data.frame)
#     1. For monthly rainfall, monthlyextract() extracts explicitly by year-month-pixel (voxels) for month-of rainfall + 24 prior months to points
#     2. For annual/nontemporal data (eg. mean annual climate vars), annextract() extracts all data to points 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  DATA EXTRACTION functions to extract monthly + annual/static spatial layers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# MONTHLY DATA EXTRACT FUNCTION (so far only rainfall)
# monthlyextract(x, y); x = the 'extract-to' spatial.points.data.frame; y = directory w raster(s) for prediction


#MONTHLY VARIABLE EXTRACTION

monthlyextraction<-propscnty
monthlyvar<-predictorlist[1]

yearmos<-unique(as.data.frame(monthlyextraction)[,"yyyymm"])	## to partition data by year-month; **assumes YYYYMO**
rasternames_mon<-list.files(monthlyvar, pattern=".tif$")
setwd(monthlyvar)
#x<-spTransform(x, crs(raster(rasternames[1])))  ## reproject extraction DF to match raster
xrefy<-ncol(monthlyextraction) 					# starting number of columns in data.frame


# rainfall rasters
a<-Sys.time()
for(j in 1:length(yearmos)){		#	
  year <- substring(yearmos[j], 1, 4)
  month <- substring(yearmos[j], 5, 6)
  rastindex<-grep(paste(year,month,sep="_"), rasternames_mon)		#matches raster to year-mo sample
  raststack<-stack(rasternames_mon[(rastindex-24):rastindex])
  xsub<-spTransform(monthlyextraction[monthlyextraction@data$yyyymm==yearmos[j],], CRS=crs(raststack))
  newextract<-extract(raststack, xsub)
 # ? newextract<-extract(stack(rasternames_mon[(rastindex-24):rastindex]), monthlyextraction[monthlyextraction@data$yyyymm==yearmos[j],])  #extracts 0-24 months prior to year-mo
  xrefx<-min(grep(paste0(year,month), propscnty@data$yyyymm, fixed=TRUE))  # NOT NECESSARY FOR TEMP
#  monthlyextraction@data[((xrefx):((xrefx-1)+nrow(newextract))), ((xrefy+1):(xrefy+(ncol(newextract))))]<- newextract
  monthlyextraction@data[monthlyextraction@data$yyyymm==yearmos[j], ((xrefy+1):(xrefy+(ncol(newextract))))]<- newextract
  }
Sys.time()-a

# for(i in 1:nlayers(raststack)){
#     plot(raststack[[i]])
# }
raststack_rain <- raststack

#rename monthly rainfall vars
rfnames<-as.character()
for(k in 1:25){
  name<-paste0("rf_",k-1,"mo_prior")
  rfnames<-c(rfnames, name)
}

##rainfall stacks from 24 months (col 8) to 0 months (col 32)

names(monthlyextraction@data)[8:length(names(monthlyextraction@data))]<-rev(rfnames)
par(mar=c(5,5,5,5))
hist(monthlyextraction@data[,9])


# temperature rasters
xrefy<-ncol(monthlyextraction) # update starting number of columns in data.frame
yearmos_temp <- yearmos[1:240]
a<-Sys.time()
for(j in 1:length(yearmos_temp)){		#	
  year <- substring(yearmos_temp[j], 1, 4)
  month <- substring(yearmos_temp[j], 5, 6)
  rast_temp_yearmo <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/tair_monthly/monthly/Tmax_map_state_', year, month, '_monthly_comp.tif'))
  xsub<-spTransform(monthlyextraction[monthlyextraction@data$yyyymm==yearmos[j],], CRS=crs(raststack))
  newextract_temp<-extract(rast_temp_yearmo, xsub)
  monthlyextraction@data[monthlyextraction@data$yyyymm==yearmos_temp[j], xrefy+1]<- newextract_temp/100
}
Sys.time()-a

# for(i in 1:nlayers(raststack)){
#   plot(raststack[[i]])
# }

# rename temperature column
names(monthlyextraction@data)[length(names(monthlyextraction@data))]<-'maxTemp_C'




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ANNUAL AND NON-TEMPORAL DATA EXTRACT FUNCTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# annextract(l, m); l = spatial.points.data.frame; m = directory w raster(s) for prediction
  annextract<-function(l, m){
    yearmos<-unique(as.data.frame(l)[,"yyyymm"])	## to partition data by year-month; **assumes YYYYMO**
    rasternames<-list.files(m, pattern=".tif$")
    setwd(m)
    l<-spTransform(l, crs(raster(rasternames[1])))  ## reproject extraction DF to match raster
    lrefy<-ncol(l) 					# starting number of columns in data.frame
    for(j in 1:length(rasternames)){ 	
      if(j==1){
        newextract<-as.data.frame(extract(raster(rasternames[j]), l))
        names(newextract)<-names(raster(rasternames[j]))
      }else{
        extract1<-extract(raster(rasternames[j]), l)
        newextract<-as.data.frame(cbind(newextract, extract1))
        names(newextract)[ncol(newextract)]<-names(raster(rasternames[j]))
      }
    }
    l@data<- cbind(l@data, newextract)
    return(l)
  }
  
#RUN EXTRACT FUNCTION
bigext2<-propscnty  #set extract-to sp.df from proportional samples 
predictorlist2<-predictorlist[-1]  #remove monthly rainfall directory, all other data annual/non-temporal
  
#lapply() w annextract() runs thru list of directories containing predictors as rasters   
# returns 'annualnontemps' = a list of sp.df's each with predictor extracted to bigext2
a<-Sys.time()
annualnontemps<-lapply(predictorlist2, function(x) annextract(bigext2, x))
Sys.time()-a
  
#check cell numbers match 
datamatch<-bigext2$cell %in%  annualnontemps[[1]]$cell
length(datamatch[datamatch==FALSE])


#grab dfs from list and cbind 
for(i in 1:length(annualnontemps)){
  extracteds<-spTransform(annualnontemps[[i]], CRS=crs(bigext2))
  bigext2<-cbind(bigext2, extracteds[,8:length(names(extracteds))])
}

#Check names

names(bigext2)

#FINAL STEP -> combine annual/nontemp w monthly extractions 

datamatch2<-bigext2$cell %in%  monthlyextraction$cell
length(datamatch2[datamatch2==FALSE])



bigextfinal<-cbind(bigext2, monthlyextraction[,8:ncol(monthlyextraction)])
names(bigextfinal)
 
Sys.time()-starttime

 
#double-check coord. ref. system
crs(bigextfinal)


#clean up data
#1 - combine values from county rasters to new vars and remove old ones
?as.data.frame
names(bigextfinal)[8:11]
bigextfinal$moisture_zones<-rowSums(bigextfinal@data[,8:11], na.rm=TRUE)

names(bigextfinal)[16:19]
bigextfinal$roughness<-rowSums(bigextfinal@data[,16:19], na.rm=TRUE)

names(bigextfinal)[20:23]
bigextfinal$ruggedness<-rowSums(bigextfinal@data[,20:23], na.rm=TRUE)

names(bigextfinal)[24:27]
bigextfinal$aspect<-rowSums(bigextfinal@data[,24:27], na.rm=TRUE)

names(bigextfinal)[28:31]
bigextfinal$slope<-rowSums(bigextfinal@data[,28:31], na.rm=TRUE)

bigextfinal<-bigextfinal[,-c(8:11, 16:31)]

names(bigextfinal)

head(bigextfinal,100)
tail(bigextfinal,100)


crs(bigextfinal)

head(bigextfinal)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
##   DATA PREP FOR EFFECTS PLOTS AND MODEL FITTING
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Year and month columns
bigextfinal$year<-rapply(strsplit(bigextfinal$yearmo, "_"), function(x) as.numeric(head(x, 1)))
bigextfinal$month<-rapply(strsplit(bigextfinal$yearmo, "_"), function(x) tail(x, 1))

#Attribute Year of Fire Land Cover - 'herb/wood/bare_yearof'
# ie create column with values of land cover at each pixel the year the fire burned**

#** NO 2014 LAND COVER therefore 2014 = 2013 land cover

bigextfinal$HI_FracLC_bare_2014<-bigextfinal$HI_FracLC_bare_2013
bigextfinal$HI_FracLC_wood_2014<-bigextfinal$HI_FracLC_wood_2013
bigextfinal$HI_FracLC_herb_2014<-bigextfinal$HI_FracLC_herb_2013
bigextfinal$HI_FracLC_RMSE_2014<-bigextfinal$HI_FracLC_RMSE_2013

#create reference columns for year
bigextfinal$herbref<-paste0("HI_FracLC_herb_",bigextfinal$year)
bigextfinal$woodref<-paste0("HI_FracLC_wood_",bigextfinal$year)
bigextfinal$bareref<-paste0("HI_FracLC_bare_",bigextfinal$year)
bigextfinal$RMSEref<-paste0("HI_FracLC_RMSE_",bigextfinal$year)
unique(bigextfinal$herbref)
unique(bigextfinal$woodref)
unique(bigextfinal$bareref)
unique(bigextfinal$RMSEref)


#SWITCH TO DATA.TABLE
bigextfinal<-as.data.table(bigextfinal)

#attribute year-of-fire land covers
#https://stackoverflow.com/questions/21466068/data-table-select-value-of-column-by-name-from-another-column

#CHUCKS ERROR for 2017 ---> but OK as landcover only goes 1999-2016

bigextfinal$herbcov_yearof<-as.numeric()
for (i in unique(bigextfinal[["herbref"]]))
  bigextfinal[ herbref==i, herbcov_yearof:=get(i) ]
warnings()

bigextfinal$woodcov_yearof=as.numeric()
for (i in unique(bigextfinal[["woodref"]]))
  bigextfinal[ woodref==i, woodcov_yearof:=get(i) ]

bigextfinal$barecov_yearof=as.numeric()
for (i in unique(bigextfinal[["bareref"]]))
  bigextfinal[ bareref==i, barecov_yearof:=get(i) ]

bigextfinal$RMSEcov_yearof=as.numeric()
for (i in unique(bigextfinal[["RMSEref"]]))
  bigextfinal[ RMSEref==i, RMSEcov_yearof:=get(i) ]

length(bigextfinal$herbcov_yearof[is.na(bigextfinal$herbcov_yearof)==FALSE])
hist(bigextfinal$herbcov_yearof)

length(bigextfinal$woodcov_yearof[is.na(bigextfinal$woodcov_yearof)==FALSE])
hist(bigextfinal$woodcov_yearof)

length(bigextfinal$barecov_yearof[is.na(bigextfinal$barecov_yearof)==FALSE])
hist(bigextfinal$barecov_yearof)

length(bigextfinal$herbcov_yearof[which(is.na(bigextfinal$herbcov_yearof)==FALSE & bigextfinal$burnt==1)] )


# Calculate Cumulative monthly rainfalls 
#(rainfall goes 23 months prior to 0 months prior, ie month of fire)

rfs24<-c(which(colnames(bigextfinal)=="rf_23mo_prior"):which(colnames(bigextfinal)=="rf_0mo_prior"))
rfs18<-c(which(colnames(bigextfinal)=="rf_17mo_prior"):which(colnames(bigextfinal)=="rf_0mo_prior"))
rfs12<-c(which(colnames(bigextfinal)=="rf_11mo_prior"):which(colnames(bigextfinal)=="rf_0mo_prior"))
rfs6<-c(which(colnames(bigextfinal)=="rf_15mo_prior"):which(colnames(bigextfinal)=="rf_0mo_prior"))
rfs3<-c(which(colnames(bigextfinal)=="rf_2mo_prior"):which(colnames(bigextfinal)=="rf_0mo_prior"))


bigextfinal$monthly_cumrf_24mo<-rowSums(bigextfinal[, .SD, .SDcols = rfs24])
bigextfinal$monthly_cumrf_18mo<-rowSums(bigextfinal[, .SD, .SDcols = rfs18])
bigextfinal$monthly_cumrf_12mo<-rowSums(bigextfinal[, .SD, .SDcols = rfs12])
bigextfinal$monthly_cumrf_6mo<-rowSums(bigextfinal[, .SD, .SDcols = rfs6])
bigextfinal$monthly_cumrf_3mo<-rowSums(bigextfinal[, .SD, .SDcols = rfs3])


#normalized(-ish) ignition density
bigextfinal$ign_trns<-bigextfinal$Statewide_Ign_Per_Sq_Mi^.025  
hist(bigextfinal$Statewide_Ign_Per_Sq_Mi)
hist(bigextfinal$ign_trns)

#set dummy var for random effect -> s(year, bs="re",by=dum)
bigextfinal$dum<-1

#write to file
write.csv(as.data.frame(bigextfinal[,2:ncol(bigextfinal)]), "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84_OVERWRITE.csv")
saveRDS(bigextfinal, "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/bigextfinal.rds")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   MODEL FITTING - Generalized Additive Models - binomial w year as random effect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


bigextfinal<-fread( "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84_OVERWRITE.csv")
head(bigextfinal)


#CHECK NAMES
names(bigextfinal)
#names(bigextfinal)[7]<-"county"
names(bigextfinal)[8]<-"mean_annual_temp"
names(bigextfinal)[9]<-"mean_annual_rainfall"
#names(bigextfinal)[108]<-"monthly_rf_0_mo_prior"

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

##MODEL 1.3.0 - NO mean annual rainfall + test soil moist.

#a<-Sys.time()
#m1.3.0<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
#                 s(woodcov_yearof, bs="tp", k=5)+
#                 s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
#                 s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
#                 s(monthly_cumrf_18mo, bs="tp", k=5)+s(monthly_cumrf_3mo, bs="tp", k=5)+
#                 s(monthly_rf_0_mo_prior, bs="tp", k=5)+
#                 s(year, bs="re",by=dum),data=cut, family=binomial)
#Sys.time()-a
#summary(m1.3.0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   GO-TO MODEL !!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


a<-Sys.time()
m1.3.1.bi<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                       s(woodcov_yearof, bs="tp", k=5)+
                       s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                       #s(maxTemp_C, bs="tp", k=5)+
                       s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                       s(rf_1mo_prior, bs="tp", k=5)+
                       s(monthly_cumrf_3mo, bs="tp", k=5)+
                       s(monthly_cumrf_12mo, bs="tp", k=5)+
                       te(herbcov_yearof,rf_1mo_prior)+
                       te(herbcov_yearof,monthly_cumrf_3mo)+
                       te(herbcov_yearof,monthly_cumrf_12mo)+
                       s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a
summary(m1.3.1.bi)

a<-Sys.time()
m1.3.1.mn<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                 #s(maxTemp_C, bs="tp", k=5)+
                 s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                 s(rf_1mo_prior, bs="tp", k=5)+
                 s(monthly_cumrf_3mo, bs="tp", k=5)+
                 s(monthly_cumrf_12mo, bs="tp", k=5)+
                 te(herbcov_yearof,rf_1mo_prior)+
                 te(herbcov_yearof,monthly_cumrf_3mo)+
                 te(herbcov_yearof,monthly_cumrf_12mo)+
                 s(year, bs="re",by=dum),data=mndat, family=binomial)
Sys.time()-a
summary(m1.3.1.mn)

a<-Sys.time()
m1.3.2.mn<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(barecov_yearof, bs="tp", k=5)+
                 s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                 #s(maxTemp_C, bs="tp", k=5)+
                 s(rf_1mo_prior, bs="tp", k=5)+
                 s(monthly_cumrf_3mo, bs="tp", k=5)+
                 s(monthly_cumrf_12mo, bs="tp", k=5)+
                 te(herbcov_yearof,rf_1mo_prior)+
                 te(herbcov_yearof,monthly_cumrf_3mo)+
                 te(herbcov_yearof,monthly_cumrf_12mo)+
                 s(year, bs="re",by=dum),data=mndat, family=binomial)
Sys.time()-a
summary(m1.3.2.mn)



save(m1.3.1.mn, m1.3.2.mn,
     file = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/Maui model comparison/maui_models.Rdata")

a<-Sys.time()
m1.3.1.oa<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                 #s(maxTemp_C, bs="tp", k=5)+
                 s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                 s(rf_1mo_prior, bs="tp", k=5)+
                 s(monthly_cumrf_3mo, bs="tp", k=5)+
                 s(monthly_cumrf_12mo, bs="tp", k=5)+
                 te(herbcov_yearof,rf_1mo_prior)+
                 te(herbcov_yearof,monthly_cumrf_3mo)+
                 te(herbcov_yearof,monthly_cumrf_12mo)+
                 s(year, bs="re",by=dum),data=oadat, family=binomial)
Sys.time()-a
summary(m1.3.1.oa)



a<-Sys.time()
m1.3.1.kaoa<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                 #s(maxTemp_C, bs="tp", k=5)+
                 s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                 s(rf_1mo_prior, bs="tp", k=5)+
                 s(monthly_cumrf_3mo, bs="tp", k=5)+
                 s(monthly_cumrf_12mo, bs="tp", k=5)+
                 te(herbcov_yearof,rf_1mo_prior)+
                 te(herbcov_yearof,monthly_cumrf_3mo)+
                 te(herbcov_yearof,monthly_cumrf_12mo)+
                 s(year, bs="re",by=dum),data=kaoadat, family=binomial)
Sys.time()-a
summary(m1.3.1.kaoa)



a<-Sys.time()
m1.3.1.ka<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                 #s(maxTemp_C, bs="tp", k=5)+
                 s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                 s(rf_1mo_prior, bs="tp", k=5)+
                 s(monthly_cumrf_3mo, bs="tp", k=5)+
                 s(monthly_cumrf_12mo, bs="tp", k=5)+
                 te(herbcov_yearof,rf_1mo_prior)+
                 te(herbcov_yearof,monthly_cumrf_3mo)+
                 te(herbcov_yearof,monthly_cumrf_12mo)+
                 s(year, bs="re",by=dum),data=kadat, family=binomial)
Sys.time()-a
summary(m1.3.1.ka)




##K-FOLDS for Cross-Validation
#evaluation with MLeval - ROC, AUC, etc.

nrow(mndat[mndat$burnt==1,])

testdata<-rbind(mndat[mndat$burnt==1,], mndat[sample(1:nrow(mndat),2000),])
nrow(testdata[testdata$burnt==0,])




##### evaluate model #####

evaluate<-function(data){
  dat <- data
  dat$abandonedAg_grassDominated_changeInWood <- NULL
  dat<-dat[complete.cases(dat)==TRUE,]
  
  dat_split <- split(dat, dat$burnt)  # split data by burnt/unburnt
  for(i in 1:length(dat_split)){ dat_split[[i]] <- dat_split[[i]][sample(nrow(dat_split[[i]])),] }  # randomize the rows for each dataframe
  for(i in 1:length(dat_split)){ dat_split[[i]]$index <- cut(seq(1,nrow(dat_split[[i]])), breaks=10, labels=FALSE) }  # add index
  dat <- do.call(rbind, dat_split)  # rbind back together
  predictiondf<-data.frame(UB=as.numeric(), B=as.numeric(),obs=as.numeric())   # UB = burnt, B = burnt
  for(k in 1:10){
    training<-dat[!dat$index==k,]
    testing<-dat[dat$index==k]
    model1<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                  s(woodcov_yearof, bs="tp", k=5)+
                  s(HI_EVAP_min_monthly_soil_mst, bs="tp", k=5)+
                  #s(maxTemp_C, bs="tp", k=5)+
                  s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                  s(rf_1mo_prior, bs="tp", k=5)+
                  s(monthly_cumrf_3mo, bs="tp", k=5)+
                  s(monthly_cumrf_12mo, bs="tp", k=5)+
                  te(herbcov_yearof,rf_1mo_prior)+
                  te(herbcov_yearof,monthly_cumrf_3mo)+
                  te(herbcov_yearof,monthly_cumrf_12mo)+
                  s(year, bs="re",by=dum),data=training, family=binomial)
    prediction1<-predict(model1, newdata=testing, type="response")
    predictiondf[ ((k*nrow(testing)-nrow(testing))+1)  : (k*nrow(testing)), 1] <- 1-prediction1
    predictiondf[ ((k*nrow(testing)-nrow(testing))+1)  : (k*nrow(testing)), 2] <- prediction1
    predictiondf[ ((k*nrow(testing)-nrow(testing))+1)  : (k*nrow(testing)), 3] <- ifelse(testing$burnt==1, "B", "UB")
  }
  predictiondf<-predictiondf[complete.cases(predictiondf)==TRUE,]
    require(MLeval)
  return(list(predictiondf, evalm(predictiondf, plots="r",rlinethick=0.8,fsize=8,bins=8)))
}

evalka<-evaluate(kadat)
evaloa<-evaluate(oadat)
evalkaoa<-evaluate(kaoadat)
evalbi<-evaluate(bidat)
evalmn<-evaluate(mndat)

save(m1.3.1.bi, m1.3.1.ka, m1.3.1.kaoa, m1.3.1.mn, m1.3.2.mn, m1.3.1.oa,
     evalbi, evalka, evalkaoa, evalmn, evaloa,
     file = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")
save(evalbi, evalka, evalkaoa, evalmn, evaloa, file = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_evals_UPDATED.Rdata")

save(evalmn, file="H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_eval_stats_mauinui.rda")

load("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata")

save.image('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/00_all_evals_and_full_data.Rdata')











load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/00_all_evals_and_full_data.Rdata')
test<-load(file="H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_eval_stats_mauinui.rda")

test$optres

predsmn<-data.frame(UB=1-accmn$predicted[complete.cases(accmn)==TRUE], 
                    B=accmn$predicted[complete.cases(accmn)==TRUE], 
                    obs=ifelse(accmn$burnt[complete.cases(accmn)==TRUE]==1, "B", "UB"))

test<-evalm(predsmn, plots="r",rlinethick=0.8,fsize=8,bins=8)

names(predsmn)

# #RMSE
# #for continuous predictions?
# rmse<-sqrt((sum((acctest$predicted[complete.cases(acctest)==TRUE]-acctest$burnt[complete.cases(acctest)==TRUE])^2))/length(acctest$burnt[complete.cases(acctest)==TRUE]))
# rmse1s<-sqrt((sum((acctest$predicted[which(complete.cases(acctest)==TRUE & acctest$burnt==1)]-
#                    acctest$burnt[which(complete.cases(acctest)==TRUE & acctest$burnt==1)])^2))/
#                     length(acctest$burnt[which(complete.cases(acctest)==TRUE & acctest$burnt==1)]))
# 
# ##change in RMSE w binary threshold
# acctest<-acctest[complete.cases(acctest)==TRUE,]
# rmseplot<-function(accuracydf){
#     accuracydf<-accuracydf[complete.cases(accuracydf)==TRUE,]
#   rmsedf<-data.frame(threshold=as.numeric(),rmsevar=as.numeric())
#   for(r in 1:100){
#     quantpredict<-ifelse(accuracydf$predicted>=quantile(accuracydf$predicted,(r/100)), 1, 0)
#     rmsedf[r,1]<-quantile(acctest$predicted,(r/100))
#     rmsedf[r,2]<-sqrt((sum((quantpredict-accuracydf$burnt)^2))/length(accuracydf$burnt))
#   }
# plot(rmsedf$threshold,rmsedf$var, pch="", ylab="RMSE", xlab="burnt threshold (quantile)" )
# lines(rmsedf$threshold,rmsedf$var )
# }
# 
# =
# 
# rmseplot(acctest)
# 
# 
# q75<-ifelse(acctest$predicted>=quantile(acctest$predicted,.75 ), 1,0)
# rmse<-sqrt((sum((q75[complete.cases(q75)==TRUE]-acctest$burnt[complete.cases(acctest)==TRUE])^2))/length(acctest$burnt[complete.cases(acctest)==TRUE]))
# length(q75[complete.cases(q75)==TRUE])
# 
# 
# 
# length(na.omit(acctest$predicted)^2)
# length(na.omit(acctest$predicted)^2)
# 
# ##ACCURACY VIZ
# plot(density(na.omit(acctest$predicted[acctest$burnt==1])), las=1, xlab="predicted fire prob")
# lines(density(na.omit(acctest$predicted[acctest$burnt==0])), col=2)
# 
# 
# 
# 
# length(accoa$burnt[accoa$fold==10])
# nrow(accoa)
# nrow(accoa[accoa$burnt==0,])
# 
# nrow(accoa[ ((k*nrow(testing)-nrow(testing))+1)  :(k*nrow(testing)), ])
# 
# tail(accoa)
# 
# #Randomly shuffle the data
# yourData<-yourData[sample(nrow(yourData)),]
# 
# #Create 10 equally size folds
# folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
# 
# 
# 
# 
# kfolds<-
# 
# 
# 
# oafolds<-createFolds(oadat$burnt, k=10)
# #Sets seed and folds
# #set.seed(123)
# #folds <- createFolds(df$default, k=10)
# #results <- lapply(folds, function(x) {
# #  credit_train <- df[-x, ]
# #  credit_test <- df[x, ]
# #  credit_model <- C5.0.formula(default ~., data = credit_train)
# #  credit_pred <- predict(credit_model, credit_test)
# #  credit_actual <- credit_test$default
# #  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
# #  return(kappa)
# #})
# #results


##PREDICTION RASTERS....

predictorlist

#County rasters for reference (proportional sampling per island) - Jon Price Moisture Zones
kauairef<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/kauai_moisture_zones.tif")
#remove niihau
newkaext<-extent(-159.8, -159.1999, 21.7501 , 22.25001)
kauairef<-crop(kauairef, newkaext)
oahuref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/Oahu_moisture_zones.tif")
mauinuiref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/mauinui_moisture_zones.tif")
bigislandref<-raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Moist_Zones_JP/bigisland_moisture_zones.tif")

#calculate mean monthly rainfall
monthlyrflist<-list.files(predictorlist[[1]], full.names=TRUE)
monthlyrfstack<-stack()
i=1
for(i in 1:12){
  if(i<10){
    meanrf<-mean(stack(monthlyrflist[grep(paste0("_0",i,"_"),monthlyrflist )]))
    monthlyrfstack<-stack(monthlyrfstack, meanrf)
    names(monthlyrfstack)[i]<-paste0("monthlyrf_0", i)
  }else{
    meanrf<-mean(stack(monthlyrflist[grep(paste0("_",i,"_"),monthlyrflist )]))
    monthlyrfstack<-stack(monthlyrfstack, meanrf)
    names(monthrfstack)[i]<-paste0("monthlyrf_", i)
  }
}

#fractional herb cover

fraccovlist<-list.files(predictorlist[[12]], full.names = TRUE)
current_herb<-raster(fraccovlist[36])
current_wood<-raster(fraccovlist[72])

#Evapotranspiration products

meanrfsoiltemp<-stack(raster(list.files(predictorlist[[4]], full.names = TRUE)[3]),
                      raster(list.files(predictorlist[[3]], full.names = TRUE)[3]),
                      raster(list.files(predictorlist[[5]], full.names = TRUE)))


#Build prediction stacks by month...ß



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##APRIL 2021 TO-DOs: COUNTY CUSTOM MODELS (County, )

#2020 Lesson --> Better fits for individual island models vs global model with island interaction (mversio 1.2.1)

#***KAUAI alone fits poorly, therefore****
# - fit for Maui nui, Big Island, Oahu separately
# - Fit Kauai/Oahu combined for Kauai prediction
# - Compare Kauai/Oahu vs Oahu only for Oahu prediction

# 1) RIP EFFECTS PLOTS
#     Proportional Scatter PER COUNTY plots per predictor w single model gam predictions

# 2) FIT COUNTY-SPECIFIC MODELS; subsetting data
#      USE Model Version 1.2.0 
#       rip off model summaries
#
# 3) VALIDATION?  Leave-one-out/k-fold?
#
# 4) PREDICTIONS --> 
#       a. "Current" monthly fire risk rasters using 2016 vegetation and mean monthly climate vars (I have code for these)
#       b. "Historical" - Rip prediction rasters for all year-months over data set, extract values within fire polygons




# 2) Run Multi-model selection based on AIC (I'll put money the global model is #1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SNIPPET BELOW MAY BE USEFUL FOR MODEL PREDICTION RASTER PLOTTING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#raster stack for prediction



herbcov_yearof<-raster("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii vegetation data/MLs_Unmixing/kauai/kauai_herb_2016.tif")  ### NEED DATA FROM HERE DOWN
woodcov_yearof<-raster("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii vegetation data/MLs_Unmixing/kauai/kauai_wood_2016.tif")

#use mean rainfall for cumulative rainfall
setwd("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii Climate data/Giambelluca/mean annual and monthly rainfall/kauai/")
meanrflist<-list.files("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii Climate data/Giambelluca/mean annual and monthly rainfall/kauai/",pattern=".tif", all.files=TRUE, full.names=FALSE)           
meanmonrfs<-stack(meanrflist)
cum_stack_3mo<-meanmonrfs[[1:3]]

a<-Sys.time()
monthcum3prior01<-meanmonrfs[[1]]+meanmonrfs[[11]]+meanmonrfs[[12]]
monthbrick01<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior01, meanmonrfs[[1]],  year, dum)
names(monthbrick01)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick01)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict01<-predict(monthbrick01, m1a, type="response", package="raster")

monthcum3prior02<-meanmonrfs[[1]]+meanmonrfs[[2]]+meanmonrfs[[12]]
monthbrick02<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior02, meanmonrfs[[2]],  year, dum)
names(monthbrick02)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick02)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict02<-predict(monthbrick02, m1a, type="response", package="raster")

monthcum3prior03<-meanmonrfs[[1]]+meanmonrfs[[2]]+meanmonrfs[[3]]
monthbrick03<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior03, meanmonrfs[[3]],  year, dum)
names(monthbrick03)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick03)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict03<-predict(monthbrick03, m1a, type="response", package="raster")

monthcum3prior04<-meanmonrfs[[4]]+meanmonrfs[[3]]+meanmonrfs[[2]]
monthbrick04<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior04, meanmonrfs[[4]],  year, dum)
names(monthbrick04)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick04)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict04<-predict(monthbrick04, m1a, type="response", package="raster")

monthcum3prior05<-meanmonrfs[[5]]+meanmonrfs[[4]]+meanmonrfs[[3]]
monthbrick05<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior05, meanmonrfs[[5]],  year, dum)
names(monthbrick05)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick05)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict05<-predict(monthbrick05, m1a, type="response", package="raster")

monthcum3prior06<-meanmonrfs[[6]]+meanmonrfs[[5]]+meanmonrfs[[4]]
monthbrick06<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior06, meanmonrfs[[6]],  year, dum)
names(monthbrick06)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick06)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict06<-predict(monthbrick06, m1a, type="response", package="raster")

monthcum3prior07<-meanmonrfs[[7]]+meanmonrfs[[6]]+meanmonrfs[[5]]
monthbrick07<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior07, meanmonrfs[[7]],  year, dum)
names(monthbrick07)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick07)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict07<-predict(monthbrick07, m1a, type="response", package="raster")

monthcum3prior08<-meanmonrfs[[8]]+meanmonrfs[[7]]+meanmonrfs[[6]]
monthbrick08<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior08, meanmonrfs[[8]],  year, dum)
names(monthbrick08)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick08)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict08<-predict(monthbrick08, m1a, type="response", package="raster")

monthcum3prior09<-meanmonrfs[[9]]+meanmonrfs[[8]]+meanmonrfs[[7]]
monthbrick09<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior09, meanmonrfs[[9]],  year, dum)
names(monthbrick09)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick09)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict09<-predict(monthbrick09, m1a, type="response", package="raster")

monthcum3prior10<-meanmonrfs[[10]]+meanmonrfs[[9]]+meanmonrfs[[8]]
monthbrick10<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior10, meanmonrfs[[10]],  year, dum)
names(monthbrick10)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick10)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict10<-predict(monthbrick10, m1a, type="response", package="raster")

monthcum3prior11<-meanmonrfs[[11]]+meanmonrfs[[10]]+meanmonrfs[[9]]
monthbrick11<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior11, meanmonrfs[[11]],  year, dum)
names(monthbrick11)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick11)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict11<-predict(monthbrick11, m1a, type="response", package="raster")

monthcum3prior12<-meanmonrfs[[12]]+meanmonrfs[[11]]+meanmonrfs[[10]]
monthbrick12<-brick( herbcov_yearof, woodcov_yearof,kauai_SPI_18mo,kauai_mean_annual_rainfall,kauai_mean_annual_temp,kauai_population_density_GHS_30mrp, ign_trns, monthcum3prior12, meanmonrfs[[12]],  year, dum)
names(monthbrick12)[1:3]<-c("herbcov_yearof", "woodcov_yearof", "kauai_SPI_18mo")
names(monthbrick12)[7:11]<-c("ign_trns","monthly_cumrf_3mo","monthly_rf_0_mo_prior", "year", "dum" )
monthpredict12<-predict(monthbrick12, m1a, type="response", package="raster")
Sys.time()-a

annualprob3<- 1-((1-monthpredict01)*(1-monthpredict02)*(1-monthpredict03)*(1-monthpredict04)*(1-monthpredict05)*(1-monthpredict06)*
                   (1-monthpredict07)*(1-monthpredict08)*(1-monthpredict09)*(1-monthpredict10)*(1-monthpredict11)*(1-monthpredict12))

plot(mfrow=c(1,1))
plot(	annualprob3,  col=rev(heat.colors(40,.9)))


coast<-readOGR("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Watersheds coastlines and other land units", "coast_n83")
coasttr<-spTransform(coast, CRS=crs(monthpredict12))

#FIRES
ref<-raster("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii Climate data/kauai_mean_annual_rainfall.tif")

fires<-readOGR("/Users/clayt/Temp_Data/Hawaii Data Sets/Hawaii Data Sets/Hawaii Fires/Hawaii State Fire Perimeters 1999-2018", "2019_1999_Hawaii_Fire_Perimeters")

fires<-spTransform(fires, CRS=crs(ref))

#quick fix dates
fires@data[271,1]<-20180803
fires@data[272,1]<-20180804
fires@data[271,"Year"]<-2018
fires@data[272,"Year"]<-2018

par(mfrow=c(1,2))
plot(monthpredict08, main="August Avg Fire Risk", zlim=c(0,1), col=rev(heat.colors(40,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict08, main="Historical Fires 1999-2018", zlim=c(0,1), col=rev(heat.colors(40,.9)))
plot(coasttr, add=TRUE)
plot(fires, add=TRUE, border="purple")

par(mfrow=c(3,4))
plot(monthpredict01, main="January", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict02, main="February", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict03, main="March", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict04, main="April", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict05, main="May", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict06, main="June", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict07, main="July", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict08, main="August", zlim=c(0,1), col=rev(heat.colors(40,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict09, main="September", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict10, main="October", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict11, main="November", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
plot(monthpredict12, main="December", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)


#for loop to save monthtly plots - NOT WORKING - png() won't sequence 
monseq<-c("01","02","03","04","05","06","07","08","09","10","11","12")
monnamseq<-c("January","February","March","April","May","June","July","August","September","October","November","December")

for(i in 1:12){
  png(paste0("C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_",monseq[i],monnamseq[i],".png"))
  plot(get(paste0("monthpredict",monseq[i])), main=monnamseq[i], zlim=c(0,1), col=rev(heat.colors(20,.9)))
  plot(coasttr, add=TRUE)
  dev.off()
}

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_01.png")
plot(monthpredict02, main="January", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_02.png")
plot(monthpredict02, main="February", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_03.png")
plot(monthpredict03, main="March", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_04.png")
plot(monthpredict04, main="April", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_05.png")
plot(monthpredict05, main="May", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_06.png")
plot(monthpredict06, main="June", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_07.png")
plot(monthpredict07, main="July", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_08.png")
plot(monthpredict08, main="August", zlim=c(0,1), col=rev(heat.colors(40,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_09.png")
plot(monthpredict09, main="September", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_10.png")
plot(monthpredict10, main="October", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_11.png")
plot(monthpredict11, main="November", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

png( "C:/Users/traue/Documents/Analyses/Hawaii flammability analyses/statewide/kauai_Setup/kauai_fire_prob_monthly_12.png")
plot(monthpredict12, main="December", zlim=c(0,1), col=rev(heat.colors(20,.9)))
plot(coasttr, add=TRUE)
dev.off()

plot(	probs)


