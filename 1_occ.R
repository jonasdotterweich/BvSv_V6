## importing and cleaning the occurrences from gbif
#

### preparing the occurences 09-18


library(dplyr)
library(spThin)


## importing the dataframes manually with rstudio import from files


#selectiong only the ones with coordinates
C_glareolus <- X0038813_240229165702484 %>% distinct(decimalLongitude, decimalLatitude, .keep_all= TRUE)

#selectiong only the ones with Myodes glareolus species name... as thinning 
C_glareolus <- C_glareolus %>% filter(scientificName == "Myodes glareolus (Schreber, 1780)" | 
                                        scientificName == "Clethrionomys glareolus (Schreber, 1780)")


#####preparing dataset for spThin

C_glareolus_thin <-C_glareolus %>% dplyr::select(decimalLatitude, decimalLongitude, species)



### spThin



thin(
  loc.data= C_glareolus_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "try09-18",
  out.base = "bvthin",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_bv.txt",
  verbose = TRUE
)




#####  Now moving to C. nivalis, filtering it and seeing if thinning is ven needed



C_nivalis <- X0038802_240229165702484 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

C_nivalis <- C_nivalis  %>% filter(scientificName == "Chionomys nivalis (Martins, 1842)")

C_nivalis_thin <- C_nivalis %>% dplyr::select(decimalLatitude, decimalLongitude, species)


thin(
  loc.data= C_nivalis_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "try09-18",
  out.base = "svthin",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_sv.txt",
  verbose = TRUE
)




##### study area of sv thin + area for cropping with no buffer

