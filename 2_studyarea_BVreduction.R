##### study area of sv thin + area for cropping with no buffer

library(sf)
library(sp)
library(mapview)
library(leaflet)
library(raster)
library(terra)
library(flexsdm)


## manually importing sv thin

## study area based on the thinne occurences of the snow vole with a 100 km buffer

studyarea <- calib_area(svthin_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


studar_sf <- st_as_sf(studyarea)

crs(studyarea) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(studyarea)



writeVector(studyarea, "studar/studarSV_V6.shp", overwrite=T)




### then study area that can be used to delimit the BV data and have the same buffer in the end. So to say SV study area with 1m width of buffer

studyarea_nobuff <- calib_area(svthin_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=1), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")





### then reducing the BV thin to the studyarea with no buffer

#first we need a spatial point data frame
xy <- bvthin_thin1[,c(2,3)]

BV_spat_df <- SpatialPointsDataFrame(coords = xy, data = bvthin_thin1,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



BV_red_nobuff<- crop(vect(BV_spat_df), studyarea_nobuff)


BV_red_nobuff_df <- as.data.frame(BV_red_nobuff)


writeVector(BV_red_nobuff, "occ09-18/BV_inSV_nobuff.shp", overwrite=T)
write.csv(BV_red_nobuff_df, "occ09-18/BV_inSV_nobuff_df.csv")
