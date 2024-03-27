### loading the climate data


library(raster)
library(sf)
library(dismo)
library(envirem)




#loading the study area. using V3 in this case

study_area <- read_sf("studar/studarSV_V6.shp")



#### loading the climatologies created for 09-18 

setwd("~/Documents/Climatologies")



bioclim <- stack("mean_bioclim0918_corr.tif")


# Define the new names for each layer
new_names <- paste0("BIO", 1:19)

names(bioclim) <- new_names



### cropping to study area

bio_croped <- crop(bioclim, study_area)

bio_ma <- mask(bio_croped, study_area)

bio_rast <- rast(bio_ma)





#### loading the enirem files



envirem <- stack("mean_envirem0918_corr.tif")


envnames <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex", "continentality",
              "embergerQ", "growingDegDays0", "growingDegDays5", "maxTempColdest", "minTempWarmest",
              "meanTempColdest", "meanTempWarmest", "monthCountByTemp10", "PETColdestQuarter", 
              "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter", 
              "thermicityIndex")



names(envirem) <- envnames



envirem_croped <- crop(envirem, study_area)

envirem_ma <- mask(envirem_croped, study_area)

envirem_rast <- rast(envirem_ma)






#### Altitude #### 

setwd("~/Documents/R Project Bank Vole/Raster/raw/altitude")

altitude <-  brick("WC_alt_lonlat.tif")

##cropping masking altitude

altitude_cr <- crop(altitude, study_area)
altitude_ma <- mask(altitude_cr, study_area)

altitude_rast <- rast(altitude_ma)

alt_re <- resample(altitude_rast, bio_rast)


setwd("~/Desktop/research_paper /paper_framework/V6_congruent")




##### Creating Terrain ruggedness index from altitude raster

alt <- raster(altitude_rast)

tri <- terrain(alt, opt = "TRI")

### traying for the spat raster with terra and seing if there is a difference

TRI <- terrain(altitude_rast, v="TRI", neighbors=8, unit="degrees")

TRI_cr <- crop(TRI, study_area)
TRI_ma <- mask(TRI_cr, study_area)

TRI_re <- resample(TRI_ma, bio_rast)

TRI_re <- mask(TRI_re, study_area)

### seems to give the same output at least from the perspective of min max values





#### Land cover data #### 

#as info land cover 2015 form copernicus

LC <- brick("landcover/mergedLC_15.tif")

LC_cr <- crop(LC, study_area)

LC_ma <- mask(LC_cr, study_area)

LC_rast <- rast(LC_ma)

LC_re <- resample(LC_rast, bio_rast)



### getting the biovariables together

#resample of the variables that are out of resolution




bivars_stack <- c(bio_rast, envirem_rast, TRI_re, alt_re, LC_re)


writeRaster(bivars_stack, "biovars/biovarsV6.tif")

writeRaster(bivars_stack, "biovars/biovarsV6_fornames.grd")





#### now lets do the VIF step 

library(usdm)

vif_bivars <- vifstep(bivars_stack, th = 10)


#27 variables from the 40 input variables have collinearity problem: 
  
#  meanTempColdest BIO6 meanTempWarmest BIO10 maxTempColdest BIO11 thermicityIndex BIO1 growingDegDays0 BIO5 annualPET BIO12 BIO7 PETWarmestQuarter minTempWarmest BIO16 BIO9 continentality monthCountByTemp10 embergerQ BIO2 BIO8 BIO13 growingDegDays5 BIO17 PETseasonality BIO18 

#After excluding the collinear variables, the linear correlation coefficients ranges between: 
#  min correlation ( BIO15 ~ BIO3 ):  0.0065127 
#max correlation ( climaticMoistureIndex ~ BIO14 ):  0.7782632 

#---------- VIFs of the remained variables -------- 
#  Variables      VIF
#1                      BIO3 1.976112
#2                      BIO4 2.095935
#3                     BIO14 6.790145
#4                     BIO15 3.241353
#5                     BIO19 6.211774
#6  aridityIndexThornthwaite 3.090694
#7     climaticMoistureIndex 8.034360
#8         PETColdestQuarter 8.844829
#9          PETDriestQuarter 5.575137
#10        PETWettestQuarter 3.437582
#11                      TRI 2.881867
#12            WC_alt_lonlat 5.548141
#13              mergedLC_15 1.280391


bivars_to_keep <- exclude(bivars_stack, vif_bivars)

names(bivars_to_keep)


#checking the extent as it was ot correct to the study_area in an occasion later on

#study_area <- read_sf("studar/study_area(svthin+bvcrop).shp")

#bivars_to_keep <- crop(bivars_to_keep, study_area)
#bivars_to_keep <- mask(bivars_to_keep, study_area)





#saving 

writeRaster(bivars_to_keep , "biovars/biovars_VIFed_V6.tif")

writeRaster(bivars_to_keep , "biovars/biovars_VIFed_V6_forenames.grd", overwrite=TRUE)
