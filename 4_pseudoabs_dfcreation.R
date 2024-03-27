### 5_pseudoabsences for the datasets


library(flexsdm)
library(raster)
library(terra)
library(sf)
library(dplyr)



#loading occurrences  BV

BV <- read.csv("occ09-18/BV_inSV_nobuff_df.csv")


BV_lonlat <- dplyr::select(BV, decimalLongitude, decimalLatitude)



#loading occurrences  SV

SV <- read.csv("occ09-18/svthin_thin1.csv")

SV_lonlat <- dplyr::select(SV, decimalLongitude, decimalLatitude)



#loading biovariables

biovars <- brick("biovars/biovars_VIFed_V6.tif")



#creating a spatial raster

bivars_rast <- rast(biovars)



### pseudoabsences BV

elev <- bivars_rast$WC_alt_lonlat

pseudo_abs_env_BV <- sample_pseudoabs(
  data= BV, 
  x= "decimalLongitude",
  y= "decimalLatitude",
  n= nrow(BV),
  method= c("env_const", env= bivars_rast),
  rlayer= elev,
  maskval= NULL,
  sp_name= "Myodes glareolus")



### pseudoabsences SV

pseudo_abs_env_SV <- sample_pseudoabs(
  data= SV, 
  x= "decimalLongitude",
  y= "decimalLatitude",
  n= nrow(SV),
  method= c("env_const", env= bivars_rast),
  rlayer= elev,
  maskval= NULL,
  sp_name= "Chionomys nivalis")

class(BV)

### combining presences & abcences 

##for BV

BV_lonlat$pr_ab <- rep(1, nrow(BV_lonlat))

pseudoabsenv_BV_wo_spname <- dplyr::select(pseudo_abs_env_BV, decimalLongitude, decimalLatitude, pr_ab)

df_BV <- rbind(BV_lonlat, pseudoabsenv_BV_wo_spname)

write.csv(df_BV, "data_to_model/df_BV.csv", row.names = FALSE)


##for SV 

SV_lonlat$pr_ab <- rep(1, nrow(SV_lonlat))

pseudoabsenv_SV_wo_spname <- dplyr::select(pseudo_abs_env_SV, decimalLongitude, decimalLatitude, pr_ab)

df_SV <- rbind(SV_lonlat, pseudoabsenv_SV_wo_spname)

write.csv(df_SV, "data_to_model/df_SV.csv", row.names = FALSE)


#assigning to all the pres&abs the biovariables at the coordinate for BV

var_bv <- extract(biovars, as.matrix(df_BV[,1:2]))


#joining the df and the var together

df_var_BV <- cbind(df_BV, var_bv)

write.csv(df_var_BV, "data_to_model/df_BV+bivars.csv", row.names = FALSE)



#assigning to all the pres&abs the biovariables at the coordinate for SV

var_sv <- extract(biovars, as.matrix(df_SV[,1:2]))

#joining the df and the var together

df_var_SV <- cbind(df_SV, var_sv)

write.csv(df_var_SV, "data_to_model/df_SV+bivars.csv", row.names = FALSE)




### also creating background data for maxnet model

study_area <- read_sf("studar/studarSV_V6.shp")



### background BV

backBV <- sample_background(BV,
                            x= "decimalLongitude",
                            y= "decimalLatitude",
                            n= 472,
                            method = c("thickening", width=100000),
                            rlayer = elev
                            )


write.csv(backBV, "data_to_model/backBV.csv")


#assigning to all the backgroundpoints the biovariables at the coordinate for BV

var_backBV <- extract(biovars, as.matrix(backBV[,1:2]))


#joining the df and the var together

df_var_backBV <- cbind(backBV, var_backBV)

write.csv(df_var_backBV, "data_to_model/backBV.csv")





### background SV

backSV <- sample_background(SV,
                            x= "decimalLongitude",
                            y= "decimalLatitude",
                            n= 370,
                            method = c("thickening", width=100000),
                            rlayer = elev
)

#assigning to all the backgroundpoints the biovariables at the coordinate for SV

var_backSV <- extract(biovars, as.matrix(backSV[,1:2]))


#joining the df and the var together

df_var_backSV <- cbind(backSV, var_backSV)

write.csv(df_var_backSV, "data_to_model/backSV.csv")
