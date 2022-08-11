rm(list=ls())
options(scipen=999)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(exactextractr)
library(stringr)
library(sf)
library(data.table)

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
fires<-st_read("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")
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
  "H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/HI_EVAP_mean_annual_rainfall__statewide_250m.tif",
#2012 Hawaii Moisture Zones; PRice and Jacobi https://www.sciencebase.gov/catalog/item/57a902e8e4b05e859bdf3c83
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Moist_Zones_JP",
#H2013 Hawaii Evapotranspiration Climate products (UH-Manoa)	Giambelluca et al. http://evapotranspiration.geography.hawaii.edu/
#mean annual temp
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/tair_ann",
#mean annual rainfall
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/HI_EVAP_mean_annual_rainfall__statewide_250m.tif",
#minimum monthly soil moisture
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/HI_EVAP_min_monthly_soil_mst.tif",

#~~~~~~~~~~TOPOGRAPHY~~~~~~~~~~
#USGS 10-m DEM + Raster package to create for 30m slope, aspect, etc.
#Elevation -
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/HI_DEM_10m",
#roughness
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Roughness",
#ruggedness
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Ruggedness",
#Aspect
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Aspect",
#slope
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Slope",

#~~~~~~~~~~VEGETATION~~~~~~~~~~
#2017 USGS Land Cover Jacobi et al. 2017  Carbon Assessment of Hawaii: U.S. Geological Survey data release, https://doi.org/10.5066/F7DB80B9. 
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/CAH_LandCover_HI_GAP_revised_2017",
#2017 Hawaii Annual Fractional Land Cover (Lucas 2017); Annual** 1999-2016 (**NO 2014) % Cover Woody, Herbaceous, Bare; 30m
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split",

#~~~~~~~~~~IGNITIONS~~~~~~~~~~
##2013 HWMO ignition density (# ignitions per square mile per year) Trauernicht and Lucas 2016. derived from all statewide wildfire ignition points 2002-2013
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Hawaii_Ign_Dens/Hawaii_Statewide_Ign_Dens",
#2015 Population density (#ppl/250m cell) from EU Global Human Settlement project; https://ghsl.jrc.ec.europa.eu/ghs_pop2019.php (European Union; 250m)
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Hawaii_Population_density_statewide_GHS",
#Custom Road "density" = Count of roads per 250m cell derived from Open Street Maps data (all hwys != "unclassified")
"H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Hawaii road density OSM"
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
ref30m<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/CAH_LandCover_HI_GAP_revised_2017/CAH_LandCover.tif")
## 250m reference raster - Rainfall atlas - temperature (Giambelluca et al)
ref250m<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/tair_ann/w001001.adf")

# ref250id<-ref250m
# values(ref250id)<-seq(1, ncell(ref250id),1)
# ref250id<-projectRaster(ref250id, ref30m, method="ngb")
# mask30m<-ref30m   #GET RID OF OCEAN CELLS
# mask30m[mask30m==0]<-NA
# mask30m[mask30m==65535]<-NA
# ref250id<-mask(ref250id, mask=mask30m)
# writeRaster(ref250id, "C:/Users/nated/Google Drive (demaagdn@hawaii.edu)/Projects/Fire/2021_HI_FIre_Model_Data/2020_HI_250m_gridcell_reference_HIevap_30m.tif", overwrite=TRUE)
ref250id<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/2020_HI_250m_gridcell_reference_HIevap_30m.tif")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   A. BIG SAMPLE of BURNT/UNBURNT points at 30m resolution (land cover) at MONTHLY time-steps
#     1. sample N=10000 random points per year-month over entire fire history
#     2. sample ALL burnt points (30m grid) within fire perimeters per year-month
#     3. check for duplicate unburnt/burnt year-month-pixels (voxels); keep burnt, remove uburnt
#     4. check for duplicate voxels (30m) within 250m grid (climate variables) and ditch duplicates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fires<-readOGR("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters", "2019_1999_Hawaii_Fire_Perimeters")

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
kauairef<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Moist_Zones_JP/kauai_moisture_zones.tif")
#remove niihau
newkaext<-extent(-159.8, -159.1999, 21.7501 , 22.25001)
kauairef<-crop(kauairef, newkaext)
oahuref<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Moist_Zones_JP/Oahu_moisture_zones.tif")
mauinuiref<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Moist_Zones_JP/mauinui_moisture_zones.tif")
bigislandref<-raster("H:/My Drive/Projects/Fire/2021_HI_FIre_Model_Data/Moist_Zones_JP/bigisland_moisture_zones.tif")

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
areaperyr<-aggregate(area_ha~Year, data=fires@data, FUN=sum)
areaperyr$prop_burned<-(areaperyr$area_ha)/sum(countyareas$area)

#area burned per year per COUNTY
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


a<-Sys.time()
for(j in 1:length(yearmos)){		#	
  year <- substring(yearmos[j], 1, 4)
  month <- substring(yearmos[j], 5, 6)
  rastindex<-grep(paste(year,month,sep="_"), rasternames_mon)		#matches raster to year-mo sample
  raststack<-stack(rasternames_mon[(rastindex-24):rastindex])
  xsub<-spTransform(monthlyextraction[monthlyextraction@data$yyyymm==yearmos[j],], CRS=crs(raststack))
  newextract<-extract(raststack, xsub)
 # ? newextract<-extract(stack(rasternames_mon[(rastindex-24):rastindex]), monthlyextraction[monthlyextraction@data$yyyymm==yearmos[j],])  #extracts 0-24 months prior to year-mo
  xrefx<-min(grep(paste0(year,month), propscnty@data$yyyymm, fixed=TRUE))
#  monthlyextraction@data[((xrefx):((xrefx-1)+nrow(newextract))), ((xrefy+1):(xrefy+(ncol(newextract))))]<- newextract
  monthlyextraction@data[monthlyextraction@data$yyyymm==yearmos[j], ((xrefy+1):(xrefy+(ncol(newextract))))]<- newextract
  }
Sys.time()-a

for(i in 1:nlayers(raststack)){
    plot(raststack[[i]])
}


  
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
library(data.table)
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
write.csv(as.data.frame(bigextfinal[,2:137]), "D:/OneDrive - hawaii.edu/Documents/Projects/PICASC LandSea/Data/Intermediate/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84_OVERWRITE.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   MODEL FITTING - Generalized Additive Models - binomial w year as random effect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


bigextfinal<-fread( "D:/OneDrive - hawaii.edu/Documents/Projects/PICASC LandSea/Data/Intermediate/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84.csv")
head(bigextfinal)


library(mgcv)

#CHECK NAMES
names(bigextfinal)
names(bigextfinal)[9]<-"county"
names(bigextfinal)[10]<-"mean_annual_temp"
names(bigextfinal)[11]<-"mean_annual_rainfall"
names(bigextfinal)[114]<-"monthly_rf_0_mo_prior"

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




##### big island ----

# all variables
a<-Sys.time()
m1.3.1.bi_AllVars<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                       s(woodcov_yearof, bs="tp", k=5)+
                       s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                       s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                       s(rf_1mo_prior, bs="tp", k=5)+
                       s(monthly_cumrf_3mo, bs="tp", k=5)+
                       s(monthly_cumrf_12mo, bs="tp", k=5)+
                       te(herbcov_yearof,rf_1mo_prior)+
                       te(herbcov_yearof,monthly_cumrf_3mo)+
                       te(herbcov_yearof,monthly_cumrf_12mo)+
                       s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no herb cover
a<-Sys.time()
m1.3.1.bi_NoHerb<-gam(burnt~ 
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no wood cover
a<-Sys.time()
m1.3.1.bi_NoWood<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         te(herbcov_yearof,rf_1mo_prior)+
                         te(herbcov_yearof,monthly_cumrf_3mo)+
                         te(herbcov_yearof,monthly_cumrf_12mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no soil moisture
a<-Sys.time()
m1.3.1.bi_NoSoilMst<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         te(herbcov_yearof,rf_1mo_prior)+
                         te(herbcov_yearof,monthly_cumrf_3mo)+
                         te(herbcov_yearof,monthly_cumrf_12mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no annual temp
a<-Sys.time()
m1.3.1.bi_NoAnnTemp<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         te(herbcov_yearof,rf_1mo_prior)+
                         te(herbcov_yearof,monthly_cumrf_3mo)+
                         te(herbcov_yearof,monthly_cumrf_12mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no 1-month-prior rainfall
a<-Sys.time()
m1.3.1.bi_NoRf1mo<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         te(herbcov_yearof,monthly_cumrf_3mo)+
                         te(herbcov_yearof,monthly_cumrf_12mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no cumulative rainfall (3 months)
a<-Sys.time()
m1.3.1.bi_NoRf3mo<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_12mo, bs="tp", k=5)+
                         te(herbcov_yearof,rf_1mo_prior)+
                         te(herbcov_yearof,monthly_cumrf_12mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a

# no cumulative rainfall (12 months)
a<-Sys.time()
m1.3.1.bi_NoRf12mo<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                         s(woodcov_yearof, bs="tp", k=5)+
                         s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
                         s(mean_annual_temp, bs="tp", k=5)+s(ign_trns, bs="tp", k=5)+
                         s(rf_1mo_prior, bs="tp", k=5)+
                         s(monthly_cumrf_3mo, bs="tp", k=5)+
                         te(herbcov_yearof,rf_1mo_prior)+
                         te(herbcov_yearof,monthly_cumrf_3mo)+
                         s(year, bs="re",by=dum),data=bidat, family=binomial)
Sys.time()-a






a<-Sys.time()
m1.3.1.mn<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
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
m1.3.1.oa<-gam(burnt~ s(herbcov_yearof, bs="tp", k=5)+
                 s(woodcov_yearof, bs="tp", k=5)+
                 s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
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
                 s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
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
                 s(HIEVAP_min_monthly_soil_mst, bs="tp", k=5)+
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


