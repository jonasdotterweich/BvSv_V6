### trying to test the model with predicting the testing set


library(sf)
library(terra)
library(raster)
library(dplyr)
library(flexsdm)



#### removing the coordinates from the testing set



#testing_bivars_BV <- testing_BV_V6 %>% select(-...1, -decimalLatitude, -decimalLongitude, -pr_ab )

#max(testing_BV_V6$decimalLongitude)

# Create a vector of raster cells from the dataframe
##rast_cells <- rast(ncol = ncol(testing_bivars_BV), nrow = nrow(testing_bivars_BV), nlyr= 13,
#                   ext = ext(min(testing_BV_V6$decimalLongitude), max(testing_BV_V6$decimalLongitude), 
#                         min(testing_BV_V6$decimalLatitude), max(testing_BV_V6$decimalLatitude)))



#old_names <- colnames(testing_bivars_BV)

#colnames(testing_bivars_BV) <- c("val1", "val2", "val3", "val4", "val5","val6", "val7", "val8","val9",
 #                                "val10", "val11", "val12", "val13")

# Assign the values from the dataframe to the raster

#for(i in 1:13) {
  # Here, replace 'val1', 'val2', etc., with your actual column names
#  values(rast_cells[[i]]) <- testing_bivars_BV[[paste0("val", i)]]
#}
#warnings()


#colnames(testing_bivars_BV) <- old_names

#names(rast_cells) <- old_names



#### what I have realised now is that creating a spatrast from the dataframe might not be tthw way to go but rather filtering the exiisting spatrast to the dataframe existing might be the way


#first for BV

coords <- cbind(testing_BV_V6$decimalLongitude, testing_BV_V6$decimalLatitude)
points_sp <- vect(coords, type="points")






## then masking the raster 
# This will keep the raster values at the points locations and set others to NA

biovars <- rast("biovars/biovars_VIFed_V6.tif")


## need to assining a CRS for the points
crs(points_sp) <- crs(biovars)

testing_raster <- mask(biovars, points_sp)






### lets see what it takes now


##loading the study area

study_area <- read_sf("studar/studarSV_V6.shp")
studar_sp <- as_Spatial(study_area)



##loading the model

load("models/BV_ensembled_V6.3.Rda")



# predicting current BV

testing_BV <- sdm_predict(
  models = BV_ensembled_v6.3,
  pred = testing_raster,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)



test_BV_mean <- testing_BV$mean$mean

plot(test_BV_mean)

plot(testing_raster)




### this does not seem to work as intedned. 
## I try to get the biovariables and compare them to what is normally passed to the predict function


bivars_old <- brick("biovars/biovars_VIFed_V6.tif")

bivars_old <- rast(bivars_old)




curr_BV <- sdm_predict(
  models = BV_ensembled_v6.3,
  pred = bivars_old,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)



curr_BV$mean
plot(curr_BV$mean)


biovars_fut <- rast("biovars/biovars_futV6.tif")


fut_BV <- sdm_predict(
  models = BV_ensembled_v6.3,
  pred = biovars_fut,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


fut_BV$mean
plot(fut_BV$mean)





##loading the model

load("models/SV_ensembled_V6.Rda")



curr_SV <- sdm_predict(
  models = SV_ensembled_v6.2,
  pred = bivars_old,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)



curr_SV$mean
plot(curr_SV$mean)



fut_SV <- sdm_predict(
  models = SV_ensembled_v6.2,
  pred = biovars_fut,
  thr = "max_sens_spec",
  con_thr = FALSE,
  predict_area = studar_sp,
  clamp = TRUE,
  pred_type = "cloglog"
)


fut_SV$mean
plot(fut_SV$mean)
