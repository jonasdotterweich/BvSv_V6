# Creating Biovariables for the future prediction 


library(raster)
library(sf)
library(dismo)
library(envirem)

#loading the study area. using V3 in this case

study_area <- read_sf("studar/studarSV_V6.shp")


# Set the working directory to where your TIFF files are manually

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/bio")

# List all TIFF files
tif_files <- list.files(pattern = "\\.tif$")


### -- Creating the BIOstack -- ###

bio_files <- list.files(pattern = "CHELSA_bio[0-9]+_.*\\.tif$")

num_extract <- as.numeric(gsub("CHELSA_bio([0-9]+)_.*\\.tif$", "\\1", bio_files))

bio_files <- bio_files[order(num_extract)]


bio_stack <- stack(lapply(bio_files, raster))


# Define the new names for each layer
new_names <- paste0("BIO", 1:19)



names(bio_stack) <- new_names

## with that the biostack is created

## biostack to extent

bio_croped <- crop(bio_stack, study_area)

bio_ma <- mask(bio_croped, study_area)


bio_rast <- rast(bio_ma)




## trying tBIO15## trying to create the envirems  ### 


## first, loaading the data to create the envirms with envirem package

# precipitation (used here 1981-2010 monthly means)

## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/pr")

prec_files <- list.files(pattern = "\\.tif$")

prec_stack <- stack(lapply(prec_files, raster))

prec_stack <- crop(prec_stack, study_area)

names(prec_stack)


prec_names <- c("precip_01", "precip_02", "precip_03", "precip_04", "precip_05", "precip_06", "precip_07", "precip_08", "precip_09", "precip_10", "precip_11", "precip_12")

names(prec_stack) <- prec_names


# temperature (used here 2041-2070 monthly means)

## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/tasmax")

tmax_files <- list.files(pattern = "\\.tif$")

tmax_stack <- stack(lapply(tmax_files, raster))

tmax_stack <- crop(tmax_stack, study_area)

tmax_names <- c("tmax_01", "tmax_02", "tmax_03", "tmax_04", "tmax_05", "tmax_06", "tmax_07", "tmax_08", "tmax_09", "tmax_10", "tmax_11", "tmax_12")

names(tmax_stack) <- tmax_names




## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/tasmin")

tmin_files <- list.files(pattern = "\\.tif$")

tmin_stack <- stack(lapply(tmin_files, raster))

tmin_stack <- crop(tmin_stack, study_area)

tmin_names <- c("tmin_01", "tmin_02", "tmin_03", "tmin_04", "tmin_05", "tmin_06", "tmin_07", "tmin_08", "tmin_09", "tmin_10", "tmin_11", "tmin_12")

names(tmin_stack) <- tmin_names


#### creating the masterstack for envirem

masterstack <- stack(tmin_stack, tmax_stack, prec_stack)

masterstack_rast <- rast(masterstack)


### solar radiation 

## taking envirems formula instead of calculating from chelsa "Mean monthly surface downwelling shortwave flux in air" (this would be the nearest to solrad from data directly)

BIO1 <- bio_croped$BIO1
# as it didnt work with the raster layer, is.lonlat missing I am converting it to a spatial raster

spatBIO1 <- rast(BIO1)



solrad <- ETsolradRasters(spatBIO1, 110)



## creation of envirems

namingScheme()

verifyRasterNames(masterstack_rast, solrad)

all <- generateEnvirem(masterstack_rast, solradstack = solrad, var = "all", tempScale = 10,
                       precipScale = 10)




### had the idea to check for scale. even though a scale was layed out in the creation of the envirem variables. I will check with the old 



bivars_old$climaticMoistureIndex

all$climaticMoistureIndex

# scale seem scorrect here, but waht abut the PETs#

bivars_old$PETColdestQuarter
all$PETColdestQuarter

#seems to be off scale

## important to kow howeevr if the BIO's are off scale too

bivars_old




#### applying now the corrections to the values and passing it to the envirem creation again 


tmin_corr <- tmin_stack * 0.1 + (-273.15)

tmax_corr <- tmax_stack * 0.1 + (-273.15)


prec_corr <- prec_stack * 0.1


masterstack_corr <- stack(tmin_corr, tmax_corr, prec_corr)

mastercorr_rast <- rast(masterstack_corr)


all_corr <- generateEnvirem(mastercorr_rast, solradstack = solrad, var = "all", tempScale = 1,
                       precipScale = 1)




all_corr$PETColdestQuarter


### finally seems to be correct

## at lest for envirem

## now checking BIO again


bio_ma$BIO4
bivars_old$BIO4
## seems off

## correction would be 
#(bio_ma$BIO4) * 0.1

bio_ma$BIO3
bivars_old$BIO3
## seems off

## correction would be 
#(bio_ma$BIO4) * 10

bio_ma$BIO14
bivars_old$BIO14

bio_ma$BIO15
bivars_old$BIO15

bio_ma$BIO19
bivars_old$BIO19





#### however all that no correction for the BIOS is done now 
### one solution would be to create the biovariables with the dismopackage for the future in the same way they were created for the past

### also what issue could it mak to have varing time spans for present and past climatc variables 10 vs 20 years


### well

## I woll create a biovars fut masterstack now

names(bivars_old)

biovars_fut <- c(bio_rast$BIO3, bio_rast$BIO4, bio_rast$BIO14, bio_rast$BIO15, bio_rast$BIO19,
                 all_corr$aridityIndexThornthwaite, all_corr$climaticMoistureIndex, all_corr$PETColdestQuarter, all_corr$PETDriestQuarter,
                 all_corr$PETWettestQuarter, bivars_old$TRI, bivars_old$WC_alt_lonlat, bivars_old$mergedLC_15)


### elecvation and TR are derived from old biovars as these values probably remain the same
## it is to be considered, wether a projction can be found for the landcover in the future


writeRaster(biovars_fut, "biovars/biovars_futV6.tif")
